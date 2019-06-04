#lang racket

(provide
  fmjson->feature-model
  feature-model->fmjson
  fmjson->clafer

  fmjson->ftree
)

(require racket/hash)
(require json)
(require threading)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "ftree.rkt")


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

(define (fmjson->feature-model j)
  (define ft (fmjson->ftree j))
  (ftree-split-opt-groups! ft)
  (ftree-force-card-opt! ft)
  (ftree-complete-order! ft)
  (values
    (ftree->feature-model ft)
    (list->vector (ftree-non-group-feature-order ft))))


(define (fmjson->ftree j)
  (unless (= 1 (fmjson-version j))
    (raise (format "unsupported FMJSON version: ~a" (fmjson-version j))))

  (define order (feature-order j))

  (define features (make-hash))
  (for ([k order])
    (define fj (fmjson-feature j k))
    (define parent (hash-ref fj 'parent))
    (hash-set! features k
      (fnode
        (hash-ref fj 'name)
        (if (eq? parent 'null) #f parent)
        (string->symbol (hash-ref fj 'card))
        (string->symbol (hash-ref fj 'gcard))
        )))

  (define constraints
    (map json->ftree-constraint (hash-ref j 'constraints)))

  (ftree features constraints (vector->list (feature-order j))))

(define (json->ftree-constraint cj)
  (match (hash-ref cj 'kind)
    ["lit" (hash-ref cj 'val)]
    ["feat" `(feature ,(hash-ref cj 'name))]
    ["op" #:when (equal? (hash-ref cj 'op) "xor")
     ; Special case for `xor`, which we don't support directly.  Instead, we
     ; unroll it into a chain of nested `(! (<=> a b))`.
     (for/fold ([acc #f]) ([arg (sequence-map json->ftree-constraint (hash-ref cj 'args))])
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
     (cons op (map json->ftree-constraint (hash-ref cj 'args)))]
    [k (raise (format "unknown expr kind: ~a" k))]))

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


; Given a function `f` that maps an element of `xs` to a value `j`, build the
; inverse mapping from each `j` to the indices `i` of the `xs` where `(f x)` is
; `j`.
(define (build-inverse-map f xs)
  (define m (make-hash))
  (for ([(x i) (in-indexed xs)] #:when #t
        [j (in-value (f x))] #:when j)
    (hash-update! m j (lambda (is) (cons i is)) (lambda () '())))
  (for/hash ([(j is) m]) (values j (reverse is))))

(define (as-string s)
  (if (symbol? s)
    (symbol->string s)
    s))

(define (feature-model->fmjson names fm)
  (define feature-children
    (build-inverse-map
      (lambda (f) (let ([i (feature-parent-id f)]) (if (= i -1) #f i)))
      (feature-model-features fm)))
  (define feature-groups
    (build-inverse-map
      (lambda (g) (let ([i (group-parent-id g)]) (if (= i -1) #f i)))
      (feature-model-groups fm)))
  (define group-members
    (build-inverse-map
      (lambda (f) (let ([j (feature-group-id f)]) (if (= j -1) #f j)))
      (feature-model-features fm)))

  (define (get-feature-name i)
    (as-string (vector-ref names i)))
    ;(as-string (feature-name (feature-model-feature fm i))))
  (define (get-group-name j)
    (format "grp_~a" (add1 j)))
    ;(as-string (group-name (feature-model-group fm j))))

  (define (feature-parent-name f)
    (cond
      [(not (= -1 (feature-group-id f)))
       (get-group-name (feature-group-id f))]
      [(not (= -1 (feature-parent-id f)))
       (get-feature-name (feature-parent-id f))]
      [else 'null]))

  (define (group-parent-name g)
    (cond
      [(not (= -1 (group-parent-id g)))
       (get-feature-name (group-parent-id g))]
      [else 'null]))

  (define (feature-children-names i)
    (append
      (map get-feature-name (hash-ref feature-children i '()))
      (map get-group-name (hash-ref feature-groups i '()))))

  (define normal-feature-json
    (for/hash ([(f i) (in-indexed (feature-model-features fm))])
      (define card
        (cond
          [(feature-force-on f) 'on]
          [(feature-force-off f) 'off]
          [else 'opt]))
      (values
        (get-feature-name i)
        `#hash(
          (name . ,(get-feature-name i))
          (parent . ,(feature-parent-name f))
          (children . ,(feature-children-names i))
          (card . ,card)
          (gcard . "opt")
        ))))

  (define group-feature-json
    (for/hash ([(g j) (in-indexed (feature-model-groups fm))]
               #:when (match/values (group-cards g) [(0 '*) #f] [(_ _) #t]))
      (define gcard
        (match/values (group-cards g)
          [(0 '*) "opt"]
          [(1 '*) "or"]
          [(0 1) "mux"]
          [(1 1) "xor"]
          [(lo hi) (raise (format "unsupported group cardinality ~a..~a" lo hi))]))
      (values
        (get-group-name j)
        `#hash(
          (name . ,(get-group-name j))
          (parent . ,(group-parent-name g))
          (children . ,(map get-feature-name (hash-ref group-members j '())))
          (card . "on")
          (gcard . ,gcard)
        ))))

  (define feature-json (hash-union normal-feature-json group-feature-json))

  (define roots
    (for/list ([fj (in-hash-values feature-json)]
               #:when (eq? 'null (hash-ref fj 'parent)))
      (hash-ref fj 'name)))

  `#hash(
    (features . ,feature-json)
    (roots . ,roots)
    (constraints . ())
    (version . #hash( ('base . 1) ))
  ))


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
