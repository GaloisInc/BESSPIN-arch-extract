#lang rosette

(provide
  (struct-out feature)
  (struct-out group)
  (struct-out dependency)
  (struct-out feature-model)
  (struct-out claim-fixed)
  (struct-out claim-dep)
  (struct-out claim-antidep)
  valid-feature-model
  all-claims
  feature-model-feature
  feature-model-group
  feature-model-dependency
  feature-model-group-members
  count-enabled-group-members
  all-group-members-disabled
  feature-model-num-features
  feature-model-num-groups
  feature-model-num-dependencies
  )


; Feature models

(struct feature (parent-id group-id depth force-on force-off) #:transparent)
(struct group (parent-id min-card max-card) #:transparent)
(struct dependency (a b val) #:transparent)
(struct feature-model (features groups dependencies) #:transparent)

(define (feature-model-feature fm i)
  (vector-ref (feature-model-features fm) i))

(define (feature-model-group fm j)
  (vector-ref (feature-model-groups fm) j))

(define (feature-model-dependency fm k)
  (vector-ref (feature-model-dependencies fm) k))

(define (feature-model-group-members fm j)
  (apply +
    (for/list ([f (feature-model-features fm)])
      (if (= j (feature-group-id f)) 1 0))))

(define (count-enabled-group-members fm cfg j)
  (apply +
    (for/list ([(f i) (in-indexed (feature-model-features fm))])
      (if (and (= j (feature-group-id f)) (vector-ref cfg i)) 1 0))))

(define (all-group-members-disabled fm cfg j)
  (apply &&
    (for/list ([(f i) (in-indexed (feature-model-features fm))])
      (if (= j (feature-group-id f)) (not (vector-ref cfg i)) #t))))

(define (feature-model-num-features fm) (vector-length (feature-model-features fm)))
(define (feature-model-num-groups fm) (vector-length (feature-model-groups fm)))
(define (feature-model-num-dependencies fm) (vector-length (feature-model-dependencies fm)))


; Claims

; A "claim" is a property that might be true of a feature model.

; Feature `a` always has value `val`.
(struct claim-fixed (a val) #:transparent)
; Feature `a` depends on feature `b`, either through an explicit dependency or
; a parent-child relationship.
(struct claim-dep (a b) #:transparent)
; Feature `a` depends on the absence of feature `b`, either through an explicit
; antidependency or membership in a common xor group.
(struct claim-antidep (a b) #:transparent)


; Feature model validity checks

(define (valid-feature-id fm i)
  (&&
    (>= i -1)
    (< i (feature-model-num-features fm))))

(define (valid-defined-feature-id fm i)
  (&&
    (>= i 0)
    (< i (feature-model-num-features fm))))

(define (valid-group-id fm i)
  (&&
    (>= i -1)
    (< i (feature-model-num-groups fm))))

(define (valid-feature fm f)
  (&&
    (valid-feature-id fm (feature-parent-id f))
    (valid-group-id fm (feature-group-id f))
    (if (>= (feature-parent-id f) 0)
      (let
        ([pf (feature-model-feature fm (feature-parent-id f))])
        (= (feature-depth f) (+ 1 (feature-depth pf))))
      (= 0 (feature-depth f)))
    (if (>= (feature-group-id f) 0)
      (let
        ([g (feature-model-group fm (feature-group-id f))])
        (= (feature-parent-id f) (group-parent-id g)))
      #t)
    (! (&& (feature-force-on f) (feature-force-off f)))
    (=> (feature-force-on f)
        (= -1 (feature-parent-id f) (feature-group-id f)))
    (=> (feature-force-off f)
        (= -1 (feature-parent-id f) (feature-group-id f)))
    ))

(define (valid-group fm j g)
  (&&
    (valid-feature-id fm (group-parent-id g))
    (let
      ([n (feature-model-group-members fm j)])
      (||
        ; Disabled/unused group
        (= 0 n (group-min-card g) (group-max-card g))
        ; XOR group
        (&&
          (= 1 (group-min-card g) (group-max-card g))
          (>= n 2))
        ; OR group
        (&&
          (= 1 (group-min-card g))
          (= n (group-max-card g))
          (>= n 2))
        ))))

(define (valid-dependency fm d)
  (&&
    (valid-feature-id fm (dependency-a d))
    (valid-feature-id fm (dependency-b d))
    (||
      (= -1 (dependency-a d) (dependency-b d))
      (let ([f (feature-model-feature fm (dependency-a d))])
        (&&
          (<= 0 (dependency-a d))
          (<= 0 (dependency-b d))
          (not (= (dependency-a d) (dependency-b d)))
          (not (= (feature-parent-id f)
                  (dependency-b d)))
          ; Prefer making A a child of B, over making A a child of the root
          ; with a dependency on B.
          (not (= -1 (feature-parent-id f)))
        )))))

(define (valid-feature-model fm)
  (apply &&
    (append
      (for/list ([f (feature-model-features fm)]) (valid-feature fm f))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (valid-group fm j g))
      (for/list ([d (feature-model-dependencies fm)]) (valid-dependency fm d))
      )))


; Claim generation

(define (all-claims-fixed fm val)
  (for/list ([i (in-range (feature-model-num-features fm))])
    (claim-fixed i val)))

(define (all-claims-dep fm)
  (for/list ([i (in-range (feature-model-num-features fm))]
             [j (in-range (feature-model-num-features fm))]
             #:when (not (= i j)))
    (claim-dep i j)))

(define (all-claims-antidep fm)
  (for/list ([i (in-range (feature-model-num-features fm))]
             [j (in-range (feature-model-num-features fm))]
             #:when (not (= i j)))
    (claim-antidep i j)))

(define (all-claims fm)
  (append
    (all-claims-fixed fm #f)
    (all-claims-fixed fm #t)
    (all-claims-dep fm)
    (all-claims-antidep fm)
    ))


