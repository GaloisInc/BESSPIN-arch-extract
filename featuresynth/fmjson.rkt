#lang racket

(provide
  fmjson->feature-model
  fmjson->clafer
)

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


(define (fmjson->clafer j)
  (define (feature-header in-group fj)
    (define gcard
      (match (hash-ref fj 'gcard)
        ["opt" ""]
        [gcard (string-append gcard " ")]))
    (define card
      (match (hash-ref fj 'card)
        ["on" #:when (not in-group) ""]
        ["opt" #:when in-group ""]
        ["on" " 1..1"]
        ["opt" " ?"]
        ["off" " 0..0"]))
    (string-append gcard (hash-ref fj 'name) card "\n"))

  (define path-map (make-hash))

  (define (walk prefix indent in-group fj)
    (define name (hash-ref fj 'name))
    (hash-set! path-map name (string-append prefix name))

    (define sub-prefix (string-append prefix name "."))
    (define sub-indent (string-append "  " indent))
    (define sub-in-group (not (equal? "opt" (hash-ref fj 'gcard))))

    (string-append*
      indent
      (feature-header in-group fj)
      (for/list ([child-name (hash-ref fj 'children)])
        (walk sub-prefix sub-indent sub-in-group (fmjson-feature j child-name)))))

  (define (constraint-string cj)
    (define (parenthesize s) (string-append "(" s ")"))
    (define expr
      (let loop ([cj cj])
        (match (hash-ref cj 'kind)
          ["lit" (if (hash-ref cj 'val) "(1 in 1)" "(0 in 1)")]
          ["feat" (hash-ref path-map (hash-ref cj 'name))]
          ["op"
           (define args (hash-ref cj 'args))
           (match (hash-ref cj 'op)
             ["and" (parenthesize (string-join (map loop args) " && "))]
             ["or" (parenthesize (string-join (map loop args) " || "))]
             ["xor" (parenthesize (string-join (map loop args) " ^^ "))]
             ["not" (parenthesize (string-join (cons "!" (map loop args))))]
             ["imp" (parenthesize (string-join (map loop args) " => "))]
             ["eqv" (parenthesize (string-join (map loop args) " <=> "))]
             [o (raise (format "unknown expr op: ~a" o))])]
          [k (raise (format "unknown expr kind: ~a" k))])))
    (string-append "[ " expr " ]\n"))

  (define feature-lines
    (for/list ([name (hash-ref j 'roots)])
      (walk "" "" #f (fmjson-feature j name))))
  (define constraint-lines
    (for/list ([cj (hash-ref j 'constraints)])
      (constraint-string cj)))

  (string-append*
    (append feature-lines (list "\n") constraint-lines)))
