#lang racket

(provide
  (struct-out name)
  (struct-out name-list)
  feature-name-list

  fmjson->feature-model
  feature-model->fmjson

  fmjson->clafer
  feature-model->clafer

  fmjson->ftree
  ftree->fmjson
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
  (ftree-split-constraints! ft)
  (ftree-complete-order! ft
    (lambda (k) (list (format "~a-parent" k))))
  (define-values (fm names) (ftree->feature-model ft))
  (values fm names))


(define (fmjson->ftree j)
  (unless (= 1 (fmjson-version j))
    (raise (format "unsupported FMJSON version: ~a" (fmjson-version j))))

  (define features
    (make-hash
      (for/list ([fj (in-hash-values (hash-ref j 'features))])
        (define name (hash-ref fj 'name))
        (define parent (hash-ref fj 'parent))
        (cons
          name
          (fnode
            (mk-name name)
            (if (eq? parent 'null) #f parent)
            (string->symbol (hash-ref fj 'card))
            (string->symbol (hash-ref fj 'gcard))
            )))))

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

(define (ftree->fmjson ft)
  (define order (ftree-feature-order ft))
  (define order-map (for/hash ([(k i) (in-indexed order)]) (values k i)))
  (define (feature-id-< a b)
    (match/values (values (hash-ref order-map a #f)
                          (hash-ref order-map b #f))
      [(#f #f) (string<? a b)]
      [(#f _) #t]
      [(_ #f) #f]
      [(i j) (< i j)]))

  (define child-map (ftree-child-map ft))
  (define (child-list k)
    (sort (set->list (hash-ref child-map k)) feature-id-<))

  (define features
    (for/hash ([(k fn) (ftree-features ft)])
      (define parent (or (fnode-parent fn) 'null))
      (values
        (string->symbol k)
        `#hash(
          (name . ,k)
          (parent . ,parent)
          (children . ,(child-list k))
          (card . ,(symbol->string (fnode-card fn)))
          (gcard . ,(symbol->string (fnode-gcard fn)))
        ))))

  (define roots
    (sort
      (for/list ([(k fn) (ftree-features ft)] #:when (not (fnode-parent fn))) k)
      feature-id-<))

  (define constraints
    (map ftree-constraint->json (ftree-constraints ft)))

  (define version
    #hash( (base . 1) ))

  `#hash(
    (features . ,features)
    (roots . ,roots)
    (constraints . ,constraints)
    (version . ,version)
  ))

(define (ftree-constraint->json c)
  (match c
    [`(feature ,id)
      `#hash( (kind . "feat") (name . ,id) )]
    [(? boolean? b)
      `#hash( (kind . "lit") (val . ,b) )]
    [`(,op ,@cs)
      (define fmjson-op
        (match op
          ['&& "and"]
          ['|| "or"]
          ['! "not"]
          ['=> "imp"]
          ['<=> "eqv"]
          [o (raise (format "unknown constraint op: ~a" o))]))
      (define fmjson-args (map ftree-constraint->json cs))
      `#hash( (kind . "op") (op . ,fmjson-op) (args . ,fmjson-args) )]
    [_ (raise (format "unknown constraint kind: ~a" c))]))


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
  (define ft (feature-model->ftree names fm))
  (ftree-collapse-groups! ft)
  (ftree-complete-order! ft)
  (ftree-split-constraints! ft)
  (ftree-constraints-to-cards! ft)
  (ftree-names-as-ids! ft)
  (ftree->fmjson ft))

(define (feature-model->clafer names fm)
  (fmjson->clafer
    (feature-model->fmjson names fm)))

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
