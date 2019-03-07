#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(current-bitwidth #f)


(struct feature (parent-id group-id depth) #:transparent)
(struct group (parent-id min-card max-card) #:transparent)
(struct dependency (a b) #:transparent)
(struct feature-model (features groups dependencies) #:transparent)

(define (feature-model-feature fm i)
  (vector-ref (feature-model-features fm) i))

(define (feature-model-group fm j)
  (vector-ref (feature-model-groups fm) j))

(define (feature-model-dependency fm k)
  (vector-ref (feature-model-dependencies fm) k))

(define (feature-model-group-members fm j)
  (apply +
    (map (lambda (f) (if (= j (feature-group-id f)) 1 0))
         (vector->list (feature-model-features fm)))))

(define (count-enabled-group-members fm cfg j)
  (apply +
    (for/list ([i (in-range (vector-length (feature-model-features fm)))])
      (define f (feature-model-feature fm i))
      (if (and (= j (feature-group-id f)) (vector-ref cfg i)) 1 0))))

(define (all-group-members-disabled fm cfg j)
  (apply &&
    (for/list ([i (in-range (vector-length (feature-model-features fm)))])
      (define f (feature-model-feature fm i))
      (if (= j (feature-group-id f)) (not (vector-ref cfg i)) #t))))

(define (valid-feature-model fm)
  (apply &&
    (append
      (for/list ([i (in-range (vector-length (feature-model-features fm)))])
        (define f (feature-model-feature fm i))
        (define p (feature-parent-id f))
        (define j (feature-group-id f))
        (&&
          (if (>= p 0)
            (= (feature-depth f) (+ 1 (feature-depth (feature-model-feature fm p))))
            (= (feature-depth f) 0))
          (if (>= j 0) (= p (group-parent-id (feature-model-group fm j))) #t)
          ))
      (for/list ([j (in-range (vector-length (feature-model-groups fm)))])
        (define g (feature-model-group fm j))
        (||
          ; Either the group is unused (no members, and 0..0 cardinality) ...
          (= 0 (group-min-card g) (group-max-card g)
             (feature-model-group-members fm j))
          ; Or it's an "xor" group (2+ members, 1..1 cardinality)
          (&&
            (= 1 (group-min-card g) (group-max-card g))
            (>= (feature-model-group-members fm j) 2)
            )
          ; Or it's an "or" group (1+ members, 1..* cardinality)
          (&&
            (= 1 (group-min-card g))
            (= (feature-model-group-members fm j) (group-max-card g))
            (>= (feature-model-group-members fm j) 1)
            )))
      (for/list ([k (in-range (vector-length (feature-model-dependencies fm)))])
        (define d (feature-model-dependency fm k))
        (define a (dependency-a d))
        (define b (dependency-b d))
        (||
          (= -1 a b)
          (&&
            (not (= a b))
            (<= 0 a)
            (<= 0 b)
            (< a (vector-length (feature-model-features fm)))
            (< b (vector-length (feature-model-features fm)))
          ))))))

(define (valid-configuration fm cfg)
  (apply &&
    (append
      (for/list ([i (in-range (vector-length (feature-model-features fm)))])
        (define f (feature-model-feature fm i))
        (define p (feature-parent-id f))
        (&&
          (if (>= p 0) (=> (vector-ref cfg i) (vector-ref cfg p)) #t)
          ))
      (for/list ([j (in-range (vector-length (feature-model-groups fm)))])
        (define g (feature-model-group fm j))
        (define p (group-parent-id g))
        (if (or (< p 0) (vector-ref cfg p))
          ; Group's parent is enabled, or group has no parent (always enabled).
          (&&
            (<= (group-min-card g)
                (count-enabled-group-members fm cfg j)
                (group-max-card g)))
          ; Group has a parent, and it's disabled.
          (all-group-members-disabled fm cfg j)
          ))
      (for/list ([k (in-range (vector-length (feature-model-dependencies fm)))])
        (define d (feature-model-dependency fm k))
        (define a (dependency-a d))
        (define b (dependency-b d))
        (&&
          (if (not (= a -1)) (=> (vector-ref cfg a) (vector-ref cfg b)) #t)
          )))))

(define (make-symbolic-feature-model num-features num-groups num-dependencies)
  (define (sym-feature-id)
    (define-symbolic* fid integer?)
    (assert (<= -1 fid))
    (assert (< fid num-features))
    fid)
  (define (sym-group-id)
    (define-symbolic* gid integer?)
    (assert (<= -1 gid))
    (assert (< gid num-groups))
    gid)

  (define (sym-feature i)
    (define-symbolic* depth integer?)
    (feature (sym-feature-id) (sym-group-id) depth))
  (define (sym-group i)
    (define-symbolic* min-card integer?)
    (define-symbolic* max-card integer?)
    (group (sym-feature-id) min-card max-card))
  (define (sym-dependency i)
    (dependency (sym-feature-id) (sym-feature-id)))

  (feature-model
    (apply vector (build-list num-features sym-feature))
    (apply vector (build-list num-groups sym-group))
    (apply vector (build-list num-dependencies sym-dependency))
    ))


(define (feature-with-parent f p)
  (feature p (feature-group-id f) (feature-depth f)))

(define (feature-model-with-features fm fs)
  (feature-model fs (feature-model-groups fm)))

(define (feature-model-with-feature-parents fm parents)
  (feature-model-with-features fm
    (for/vector ([f (feature-model-features fm)] [p parents])
      (feature-with-parent f p))))

(define example-fm
  (feature-model
    (vector
      (feature -1 0 0)
      (feature -1 0 0)
      (feature -1 -1 0))
    (vector
      (group -1 1 1))
    (vector)))

(feature-model-group-members example-fm 0)
(valid-feature-model example-fm)
(valid-configuration example-fm #(#t #f #t))
(valid-configuration example-fm #(#t #t #t))
(displayln (list 'enabled (count-enabled-group-members example-fm #(#t #f #t) 0)))

(define (synthesize-feature-model symbolic-fm tests)
  (define M
    (synthesize
      #:forall '()
      #:guarantee
      (begin
        (assert (valid-feature-model symbolic-fm))
        (for ([t tests])
          (assert (<=> (cdr t) (valid-configuration symbolic-fm (car t))))))))

  (if (unsat? M) #f (evaluate symbolic-fm M)))

(define (synthesize-alternative-feature-model symbolic-fm concrete-fms tests)
  (define M
    (synthesize
      #:forall '()
      #:guarantee
      (begin
        (assert (valid-feature-model symbolic-fm))
        (for ([t tests])
          (assert (<=> (cdr t) (valid-configuration symbolic-fm (car t)))))
        (for ([fm concrete-fms])
          (assert (not (equal? symbolic-fm fm))))
        )))

  (if (unsat? M) #f (evaluate symbolic-fm M)))

(define (distinguishing-input symbolic-fm concrete-fm tests)
  (define symbolic-config
    (build-vector
      (vector-length (feature-model-features symbolic-fm))
      (lambda (i) (define-symbolic* en boolean?) en)))

  (define M
    (synthesize
      #:forall '()
      #:guarantee
      (begin
        (assert (valid-feature-model symbolic-fm))
        ; symbolic-fm passes all tests
        (for ([t tests])
          (assert (<=> (cdr t) (valid-configuration symbolic-fm (car t)))))
        ; but symbolic-fm produces a different output from concrete-fm on
        ; symbolic-config
        (assert
          (not
            (<=>
              (valid-configuration symbolic-fm symbolic-config)
              (valid-configuration concrete-fm symbolic-config)))))))

  (if (unsat? M) #f (evaluate symbolic-config M)))

(define (oracle-guided-synthesis symbolic-fm oracle tests)
  (displayln (list "synthesizing from" (length tests) "tests"))
  (define synth-fm (synthesize-feature-model symbolic-fm tests))
  (if synth-fm
    (begin
      (define dist-input (distinguishing-input symbolic-fm synth-fm tests))
      (if dist-input
        (begin
          (define new-test (cons dist-input (oracle dist-input)))
          (oracle-guided-synthesis symbolic-fm oracle (cons new-test tests)))
        (begin
          (define alt-fm (synthesize-alternative-feature-model symbolic-fm (list synth-fm) tests))
          (when alt-fm (displayln (list "alt result" alt-fm)))
          synth-fm)))
    #f))



(define example-fm-2
  (feature-model
    (vector
      (feature -1 -1 0)
      (feature -1 -1 0)
      (feature 0 0 1)
      (feature 0 0 1)
      (feature 1 1 1)
      (feature 1 1 1)
    )
    (vector
      ;(group -1 1 1)
      (group 0 1 1)
      (group 1 1 1)
    )
    (vector
      (dependency 2 5)
    )
  ))

(define (init-tests fm oracle)
  (define num-features (vector-length (feature-model-features fm)))
  (for/list ([i (in-range num-features)])
    (define t (build-vector num-features (lambda (j) (= i j))))
    (cons t (oracle t))))

(define sketch-fm (make-symbolic-feature-model 6 2 1))
(define (oracle inp) (valid-configuration example-fm-2 inp))
(define synth-fm (oracle-guided-synthesis sketch-fm oracle '()))
(pretty-write (list "synthesis result" synth-fm))
