#lang racket

(provide
  (struct-out clafer)
  feature-model->clafer
  clafer->string
  )

(require "types.rkt")


(struct clafer (name card children optional) #:transparent)

(define (group-name j)
  (format "grp_~a" j))

(define (group-clafer-cardinality g)
  (cond
    [(= 1 (group-min-card g) (group-max-card g)) 'xor]
    [(= 1 (group-min-card g)) 'or]
    [else 'opt]))

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
    (when (not (= 0 (group-min-card g) (group-max-card g)))
      (cond
        [(not (= -1 p)) (hash-set! (vector-ref feature-hashes p) k h)]
        [else (hash-set! top k h)])))

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
        (clafer (vector-ref feature-names i) 'default
                (node->clafers feature-names fm sub-n)
                #t)]
      [`(group ,j)
        (clafer (group-name j)
                (group-clafer-cardinality (feature-model-group fm j))
                (node->clafers feature-names fm sub-n)
                #f)])))

(define (feature-model-constraints path-map fm)
  (define feature-constraints
    (for/list ([(f i) (in-indexed (feature-model-features fm))]
               #:when (or (feature-force-on f) (feature-force-off f)))
      (cond
        [(feature-force-on f)
         (format "[ ~a ]" (hash-ref path-map `(feature ,i)))]
        [(feature-force-off f)
         (format "[ ! ~a ]" (hash-ref path-map `(feature ,i)))])))

  (define dependency-constraints
    (for/list ([d (feature-model-dependencies fm)]
               #:when (not (= -1 (dependency-a d) (dependency-b d))))
      (define a-path (hash-ref path-map `(feature ,(dependency-a d))))
      (define b-path (hash-ref path-map `(feature ,(dependency-b d))))
      (if (dependency-val d)
        (format "[ ~a => ~a ]" a-path b-path)
        (format "[ ~a => ! ~a ]" a-path b-path))))

  (append feature-constraints dependency-constraints))

(define (feature-model->clafer feature-names fm)
  (define tree (build-feature-tree fm))
  (define path-map (build-path-map feature-names tree))
  (cons
    (node->clafers feature-names fm tree)
    (feature-model-constraints path-map fm)))

(define (clafer->lines c)
  (define card
    (match (clafer-card c)
      ['xor "xor "]
      ['or "or "]
      ['opt "opt "]
      ['default ""]))
  (if (not (null? (clafer-children c)))
    (let
      ([header (format "~a~a~a {~n"
                       card (clafer-name c) (if (clafer-optional c) " ?" ""))]
       [body
         (append-map
           (lambda (c)
             (map (lambda (s) (string-append "  " s)) (clafer->lines c)))
           (clafer-children c))])
      `(,header ,@body "}\n"))
    (list (format "~a~a~a~n"
                  card (clafer-name c) (if (clafer-optional c) " ?" "")))))

(define (clafer->string c)
  (match-define (cons clafers constraints) c)
  (string-append*
    (append
      (append-map clafer->lines clafers)
      (map (lambda (s) (string-append s "\n")) constraints))))
