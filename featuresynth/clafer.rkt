#lang racket

(provide
  (struct-out clafer)
  feature-model->clafer
  clafer->string
  )

(require "types.rkt")


(struct clafer (name card gcard children) #:transparent)

(define (feature-clafer-cardinality f)
  (cond
    [(feature-force-on f) 'force-on]
    [(feature-force-off f) 'force-off]
    [else 'default]))

(define (group-name j)
  (format "grp_~a" j))

(define (group-clafer-cardinality g)
  (match/values (group-cards g)
    [(1 1) 'xor]
    [(0 1) 'mux]
    [(1 '*) 'or]
    [(0 '*) 'opt]
    [(lo hi) (raise (format "unsupported group cardinality ~a..~a" lo hi))]))

; Build a tree of features reflecting the `feature-parent-id` relationships in
; `fm`.  Each node in the result is a hash that maps child IDs to nodes
; representing those features.  The returned value is the hash of children for
; the root feature `-1`.
(define (build-feature-tree fm)
  (define feature-hashes (build-vector (feature-model-num-features fm)
                                       (lambda (i) (make-hash))))
  (define group-hashes (build-vector (feature-model-num-groups fm)
                                     (lambda (i) (make-hash))))
  (define top (make-hash))

  ; Attach each group to its parent feature.
  (for ([(g j) (in-indexed (feature-model-groups fm))])
    (define p (group-parent-id g))
    (define h (vector-ref group-hashes j))
    (define k `(group ,j))
    (match/values (group-cards g)
      [(0 '*) (void)]
      [(_ _)
        (cond
          [(not (= -1 p)) (hash-set! (vector-ref feature-hashes p) k h)]
          [else (hash-set! top k h)])]))

  ; Attach each feature to its parent feature or group.
  (for ([(f i) (in-indexed (feature-model-features fm))])
    (define p (feature-parent-id f))
    (define j (feature-group-id f))
    (define h (vector-ref feature-hashes i))
    (define k `(feature ,i))
    (cond
      [(not (= -1 j)) (hash-set! (vector-ref group-hashes j) k h)]
      [(not (= -1 p)) (hash-set! (vector-ref feature-hashes p) k h)]
      [else (hash-set! top k h)]))

  top)

(define (build-path-map feature-names n)
  (define path-map (make-hash))
  (define (loop prefix n)
    (for ([(k sub-n) n])
      (define name
        (match k
          [`(feature ,i) (vector-ref feature-names i)]
          [`(group ,j) (group-name j)]))
      (hash-set! path-map k (string-append prefix name))
      (loop (format "~a~a." prefix name) sub-n)))
  (loop "" n)
  path-map)

(define (mk-node-key<? feature-names)
  (define (node-key-<? a b)
    (match (list a b)
      [`((feature ,i) (feature ,j))
        (string<? (vector-ref feature-names i) (vector-ref feature-names j))]
      [`((group ,i) (group ,j)) (< i j)]
      [`((feature ,_) ,_) #t]
      [`(,_ (feature ,_)) #f]
      ))
  node-key-<?)

(define (node->clafers feature-names fm n)
  (define pairs (sort (hash->list n) (mk-node-key<? feature-names) #:key car))
  (for/list ([p pairs])
    (match-define (cons k sub-n) p)
    (match k
      [`(feature ,i)
        (clafer (vector-ref feature-names i)
                (feature-clafer-cardinality (feature-model-feature fm i))
                'default
                (node->clafers feature-names fm sub-n))]
      [`(group ,j)
        (clafer (group-name j)
                'force-on
                (group-clafer-cardinality (feature-model-group fm j))
                (node->clafers feature-names fm sub-n))])))

(define (split-constraint c)
  (match c
    [`(&& ,@cs) (append* (map split-constraint cs))]
    [else (list c)]))

(define (render-constraint path-map c)
  (define (parenthesize s) (string-append "(" s ")"))
  (let loop ([c c])
    (match c
      [(? integer?) (hash-ref path-map `(feature ,c))]
      [#t "(1 in 1)"]
      [#f "(0 in 1)"]
      [`(&& ,@cs) (parenthesize (string-join (map loop cs) " && "))]
      [`(|| ,@cs) (parenthesize (string-join (map loop cs) " || "))]
      [`(! ,c) (format "(! ~a)" (loop c))]
      [`(=> ,c1 ,c2) (format "(~a => ~a)" (loop c1) (loop c2))]
      [`(<=> ,c1 ,c2) (format "(~a <=> ~a)" (loop c1) (loop c2))])))

(define (feature-model-constraints path-map fm)
  (define dependency-constraints
    (for/list ([d (feature-model-dependencies fm)]
               #:when (not (= -1 (dependency-a d) (dependency-b d))))
      (define a-path (hash-ref path-map `(feature ,(dependency-a d))))
      (define b-path (hash-ref path-map `(feature ,(dependency-b d))))
      (if (dependency-val d)
        (format "[ ~a => ~a ]" a-path b-path)
        (format "[ ~a => ! ~a ]" a-path b-path))))

  (define constraint-constraints
    (for/list ([c (split-constraint (feature-model-constraint fm))])
      (format "[ ~a ]" (render-constraint path-map c))))

  (append dependency-constraints constraint-constraints))

(define (feature-model->clafer feature-names fm)
  (define tree (build-feature-tree fm))
  (define path-map (build-path-map feature-names tree))
  (cons
    (node->clafers feature-names fm tree)
    (feature-model-constraints path-map fm)))

(define (clafer->lines c)
  (define card
    (match (clafer-card c)
      ['force-on "1..1"]
      ['force-off "0..0"]
      ['default "?"]))
  (define gcard
    (match (clafer-gcard c)
      ['xor "xor "]
      ['mux "mux "]
      ['or "or "]
      ['opt "opt "]
      ['default ""]))
  (if (not (null? (clafer-children c)))
    (let
      ([header (format "~a~a ~a {~n"
                       gcard (clafer-name c) card)]
       [body
         (append-map
           (lambda (c)
             (map (lambda (s) (string-append "  " s)) (clafer->lines c)))
           (clafer-children c))])
      `(,header ,@body "}\n"))
    (list (format "~a~a ~a~n"
                  gcard (clafer-name c) card))))

(define (clafer->string c)
  (match-define (cons clafers constraints) c)
  (string-append*
    (append
      (append-map clafer->lines clafers)
      (map (lambda (s) (string-append s "\n")) constraints))))
