#lang racket

(provide
  fmjson->feature-model)

(require json)
(require threading)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")


(define (fmjson-feature j name)
  (~> j (hash-ref 'features) (hash-ref (string->symbol name))))

(define (fmjson-version j [ext 'base])
  (~> j (hash-ref 'version) (hash-ref ext)))

; Return the names of all features in `j`, using a preorder traversal starting
; from `roots`.
(define (feature-order j)
  (define names-rev '())
  (define (walk name)
    (set! names-rev (cons name names-rev))
    (define children (~> j (fmjson-feature name) (hash-ref 'children)))
    (for ([n children]) (walk n)))
  (for ([n (hash-ref j 'roots)]) (walk n))
  (list->vector (reverse names-rev)))

(define (produces-group fj)
  (not (equal? "opt" (hash-ref fj 'gcard))))

; Direct translation of FMJSON to `feature-model`.  Generates one feature for
; every FMJSON feature, and a group for every feature having `gcard != opt`.
; Returns two values: the `feature-model`, and a vector of feature names.
(define (fmjson->feature-model j)
  (unless (= 1 (fmjson-version j))
    (raise (format "unsupported FMJSON version: ~a" (fmjson-version j))))

  (define order (feature-order j))
  (define feature-map
    (for/hash ([(name i) (in-indexed order)])
      (values name i)))
  (define group-map
    (let ([i 0])
      (for/hash ([name order] #:when (produces-group (fmjson-feature j name)))
        (begin0
          (values name i)
          (set! i (add1 i))))))

  (define (feature-id name)
    (if (eq? name 'null) -1
      (hash-ref feature-map name)))
  (define (group-id name)
    (hash-ref group-map name -1))

  ; json->feature sometimes has to generate additional dependencies.  These are
  ; used to handle `card = on` in non-top-level features.
  (define extra-deps
    (let ([acc '()])
      (match-lambda
        ['get (reverse acc)]
        [x (set! acc (cons x acc))])))

  (define (json->feature fj)
    (define name (hash-ref fj 'name))
    (define parent-name (hash-ref fj 'parent))
    (define pid (feature-id parent-name))
    (define gid (group-id parent-name))
    (define-values (on off)
      (match (hash-ref fj 'card)
        ["opt" (values #f #f)]
        ["off" #:when (= -1 pid) (values #f #t)]
        ["on" #:when (= -1 pid) (values #t #f)]
        ["off"
         (extra-deps (dependency pid (feature-id name) #f))
         (values #f #f)]
        ["on"
         (extra-deps (dependency pid (feature-id name) #t))
         (values #f #f)]))
    (feature pid gid 'depth-unset on off)
  )

  (define (json->group fj)
    ; The group's parent is its corresponding feature.
    (define pid (feature-id (hash-ref fj 'name)))
    (define-values (lo hi)
      (match (hash-ref fj 'gcard)
        ["opt" (raise "shouldn't run json->group when `gcard = opt`")]
        ["or" (values 1 '*)]
        ["mux" (values 0 1)]
        ["xor" (values 1 1)]
        [gc (raise (format "unknown group cardinality: ~a" gc))]))
    (group pid lo hi)
  )

  (define (json->constraint cj)
    (match (hash-ref cj 'kind)
      ["lit" (hash-ref cj 'val)]
      ["feat" (feature-id (hash-ref cj 'name))]
      ["op" #:when (equal? (hash-ref cj 'op) "xor")
       ; Special case for `xor`, which we don't support directly.  Instead, we
       ; unroll it into a chain of nested `(! (<=> a b))`.
       (for/fold ([acc #f]) ([arg (sequence-map json->constraint (hash-ref cj 'args))])
         `(! (<=> ,acc ,arg)))]
      ["op"
       (define op
         (match (hash-ref cj 'op)
           ["and" '&&]
           ["or" '||]
           ; `xor` handled above
           ["not" '!]
           ["imp" '=>]
           ["eqv" '<=>]
           [o (raise (format "unknown expr op: ~a" o))]))
       (cons op (map json->constraint (hash-ref cj 'args)))]
      [k (raise (format "unknown expr kind: ~a" k))]))

  (values
    (feature-model
      (for/vector ([name order]) (json->feature (fmjson-feature j name)))
      (let ([acc (make-vector (hash-count group-map))])
        (for ([(name idx) group-map])
          (vector-set! acc idx (json->group (fmjson-feature j name))))
        acc)
      (list->vector (extra-deps 'get))
      (cons '&& (map json->constraint (hash-ref j 'constraints))))
    order
  ))


(define (recalc-feature-depths fm)
  (define depth-map (make-hash))
  (hash-set! depth-map -1 -1)
  (define (depth i)
    (hash-ref! depth-map i
      (lambda ()
        (add1 (depth (feature-parent-id (feature-model-feature fm i)))))))

  (struct-copy feature-model fm
    [features
      (for/vector ([(f i) (in-indexed (feature-model-features fm))])
        (struct-copy feature f [depth (depth i)]))]))
