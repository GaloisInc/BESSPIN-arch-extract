#lang rosette

(provide
  (struct-out feature)
  (struct-out group)
  (struct-out dependency)
  (struct-out feature-model)
  (struct-out claim-fixed)
  (struct-out claim-dep)
  (struct-out claim-antidep)
  (struct-out claim-needs-child)
  valid-feature-model
  all-claims
  all-fixed-claims
  group-cards
  feature-model-feature
  feature-model-group
  feature-model-dependency
  feature-model-group-members
  feature-model-group-member-indices
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
(struct feature-model (features groups dependencies constraint) #:transparent)

(define (group-cards g)
  (values (group-min-card g) (group-max-card g)))

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

(define (feature-model-group-member-indices fm j)
  (for/list ([(f i) (in-indexed (feature-model-features fm))]
             #:when (= j (feature-group-id f))) i))

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
; Feature `a` depends on at least one of its children being enabled.  In other
; words, `a` contains an `or` or `xor` group.
;
; This one's a bit tricky to define and evaluate, since the actual children of
; `a` are not known until synthesis is complete.  So we overapproximate: this
; claim means that `a` depends on some `b` for which `(claim-dep b a)` has not
; been disproven.
(struct claim-needs-child (a) #:transparent)


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
        (&&
          (= (feature-depth f) (+ 1 (feature-depth pf)))
          (! (feature-force-on pf))
          (! (feature-force-off pf))
          ))
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
        (&&
          (= 0 (group-min-card g))
          (eq? '* (group-max-card g))
          (= 0 n))
        ; XOR group
        (&&
          (= 1 (group-min-card g))
          (eq? 1 (group-max-card g))
          (>= n 2))
        ; MUX group
        (&&
          (= 0 (group-min-card g))
          (eq? 1 (group-max-card g))
          (>= n 2))
        ; OR group
        (&&
          (= 1 (group-min-card g))
          (eq? '* (group-max-card g))
          (>= n 2))
        ))))

(define (valid-dependency fm d)
  (&&
    (valid-feature-id fm (dependency-a d))
    (valid-feature-id fm (dependency-b d))
    (if (&& (<= 0 (dependency-a d))
            (<= 0 (dependency-b d)))
      (let
        ([af (feature-model-feature fm (dependency-a d))]
         [bf (feature-model-feature fm (dependency-b d))])
        (&&
          (not (= (dependency-a d) (dependency-b d)))
          (not (= (feature-parent-id af)
                  (dependency-b d)))
          (! (feature-force-off af))
          (! (feature-force-on af))
          (! (feature-force-off bf))
          (! (feature-force-on bf))
        ))
      (= -1 (dependency-a d) (dependency-b d)))))

(define (valid-constraint fm c)
  (define (loop c) (valid-constraint fm c))
  (match c
    [(? integer?) (&& (<= 0 c) (valid-feature-id fm c))]
    [(? boolean?) #t]
    [(cons '&& args) (apply && (map loop args))]
    [(cons '|| args) (apply && (map loop args))]
    [(cons '! args) (apply && (map loop args))]
    [(cons '=> args) (apply && (map loop args))]
    [(cons '<=> args) (apply && (map loop args))]
    [else #f]
    ))

(define (valid-feature-model fm)
  (apply &&
    (append
      (for/list ([f (feature-model-features fm)]) (valid-feature fm f))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (valid-group fm j g))
      (for/list ([d (feature-model-dependencies fm)]) (valid-dependency fm d))
      (list (valid-constraint fm (feature-model-constraint fm)))
      )))


; Claim generation

(define (all-claims-fixed fm val)
  (for/list ([i (in-range (feature-model-num-features fm))])
    (claim-fixed i val)))

(define (all-claims-dep fm)
  (for/list ([i (in-range (feature-model-num-features fm))]
             #:when #t
             [j (in-range (feature-model-num-features fm))]
             #:when (not (= i j)))
    (claim-dep i j)))

(define (all-claims-antidep fm)
  (for/list ([i (in-range (feature-model-num-features fm))]
             #:when #t
             [j (in-range (feature-model-num-features fm))]
             #:when (not (= i j)))
    (claim-antidep i j)))

(define (all-claims-needs-child fm)
  (for/list ([i (in-range (feature-model-num-features fm))])
    (claim-needs-child i)))

(define (all-claims fm)
  (append
    (all-claims-fixed fm #f)
    (all-claims-fixed fm #t)
    (all-claims-dep fm)
    (all-claims-antidep fm)
    (all-claims-needs-child fm)
    ))

(define (all-fixed-claims fm)
  (append
    (all-claims-fixed fm #f)
    (all-claims-fixed fm #t)
    ))
