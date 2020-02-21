#lang rosette

(provide
  eval-feature-model
  eval-dependency
  eval-constraint
  constraint-clauses

  (struct-out claim-set)
  make-claim-set
  claim-set-count claim-set-member?
  claim-set-update claim-set-filter! claim-set-remove-feature!
  claim-set-eval claim-set-eval-claim claim-set-eval-claim-precise
  )

(require bdd/robdd)
(require "types.rkt")
(require "util.rkt")


; Feature model evaluation

(define (eval-feature fm i f cfg)
  (let
    ([p (feature-parent-id f)])
    (&&
      (if (>= p 0) (=> (vector-ref cfg i) (vector-ref cfg p)) #t)
      (=> (feature-force-on f) (vector-ref cfg i))
      (=> (feature-force-off f) (! (vector-ref cfg i)))
    )))

(define (eval-group fm j g cfg)
  (let
    ([p (group-parent-id g)])
    (if (or (< p 0) (vector-ref cfg p))
      ; Group's parent is enabled, or group has no parent (always enabled).
      (&&
        (<=
          (group-min-card g)
          (count-enabled-group-members fm cfg j))
        (or
          (eq? (group-max-card g) '*)
          (<=
            (count-enabled-group-members fm cfg j)
            (group-max-card g))))
      ; Group has a parent, and it's disabled.
      (all-group-members-disabled fm cfg j))))

(define (eval-dependency fm d cfg)
  (let
    ([a (dependency-a d)]
     [b (dependency-b d)]
     [val (dependency-val d)])
    (if (not (= a -1))
      (=> (vector-ref cfg a) (<=> val (vector-ref cfg b)))
      #t)))

; Evaluate a feature model to determine the validity of `cfg`.  Returns `#t` if
; `cfg` is allowed by model `fm`, and `#f` if it is forbidden.
(define (eval-feature-model fm cfg)
  (apply &&
    (append
      (for/list ([(f i) (in-indexed (feature-model-features fm))])
        (eval-feature fm i f cfg))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (eval-group fm j g cfg))
      (for/list ([d (feature-model-dependencies fm)])
        (eval-dependency fm d cfg))
      (list (eval-constraint (feature-model-constraint fm) cfg))
      )))


; Claim evaluation

(struct claim-set (
  ; Set of claims that are not yet disproved
  claims
  ; Cache of potential children of a feature.  For each parent feature `a`, we
  ; keep a list of potential child features `b` for which `(claim-dep b a)` is
  ; in `claims`.  This is used for efficient evaluation of `claim-needs-child`
  ; claims.
  ;
  ; A feature `a` is a key in this map only when `(claim-needs-child a)` is
  ; present in `claims`.
  child-map
  ; Hash mapping parent features to sets of bit masks.  Each mask represents a
  ; valid subset of the feature's children.  For example, if `#b101` is in the
  ; set, it means that (1) features 0 and 2 are children of the parent feature
  ; and (2) we have seen a passing test where the parent is enabled and the
  ; only enabled children were 0 and 2.
  ;
  ; If 0 ever appears as a bit mask, it means the parent was enabled but no
  ; children were, and thus the `claim-needs-child` is disproved.  The
  ; operations that manipulate mask sets check for this and call
  ; `claim-set-remove-parent!` when appropriate.
  valid-child-masks
) #:transparent)

(define (make-claim-set claims)
  (define cs (for/mutable-set ([c claims]) c))

  (define child-map (make-hash))
  (for ([c cs] #:when (claim-needs-child? c))
    (hash-set! child-map (claim-needs-child-a c) (mutable-set)))
  (for ([c cs] #:when (claim-dep? c))
    (match-define (claim-dep child parent) c)
    (when-let ([children (hash-ref child-map parent #f)])
      (set-add! children child)))

  (define valid-child-masks (make-hash))
  (for ([parent (in-hash-keys child-map)])
    (hash-set! valid-child-masks parent (set)))

  (claim-set
    cs
    child-map
    valid-child-masks
    ))

(define (claim-set-count cset)
  (set-count (claim-set-claims cset)))

(define (claim-set-member? cset c)
  (set-member? (claim-set-claims cset) c))

; Update caches, based on the newly discovered fact that `child` is not a child
; of `parent`.
(define (claim-set-remove-child! cset parent child)
  (when-let ([children (hash-ref (claim-set-child-map cset) parent #f)])
    (set-remove! children child))
  (claim-set-update-valid-child-masks! cset parent (mask-set-clear-bit child))
  )

(define (claim-set-remove-parent! cset parent)
  (set-remove! (claim-set-claims cset) (claim-needs-child parent))
  (hash-remove! (claim-set-child-map cset) parent)
  (hash-remove! (claim-set-valid-child-masks cset) parent))

; Apply `f` to compute new `valid-child-masks` for `parent`.
;
; If the new set of masks contains 0, then the `claim-needs-child` is
; disproved, and this funciton calls `claim-set-remove-parent!`.
(define (claim-set-update-valid-child-masks! cset parent f)
  (when-let ([masks (hash-ref (claim-set-valid-child-masks cset) parent #f)])
    (define new-masks (f masks))
    (if (set-member? new-masks 0)
      (claim-set-remove-parent! cset parent)
      (hash-set! (claim-set-valid-child-masks cset) parent new-masks)
    )))

(define (mask-set-clear-bit b)
  (define not-bit (bitwise-not (arithmetic-shift 1 b)))
  (lambda (masks)
    (for/set ([m masks]) (bitwise-and m not-bit))))

(define (mask-set-has-subset? masks m)
  (for/or ([m0 masks])
    ; Is `m0` a subset of `m`?
    (= m0 (bitwise-and m m0))))

(define (mask-set-add m)
  (lambda (masks)
    (if (not (mask-set-has-subset? masks m))
      (set-add masks m)
      masks)))

; Filter out claims for which `(f claim)` returns `#f`.
;
; When `claim-dep`s are filtered out, `child-map` and `valid-child-masks` are
; updated, and it's possible for a `claim-needs-child` to be disproved.
(define (claim-set-filter! cset f)
  (define removed
    (for/list ([c (claim-set-claims cset)] #:when (not (f c))) c))
  (define recheck-parents (mutable-set))
  ; All claims in `removed` are in `claims` as of the start of the loop, but
  ; it's possible for `claim-set-remove-child!` -> `claim-set-remove-parent!`
  ; to delete a claim during iteration.  This is why we need the `#:when`.
  (for ([c removed] #:when (set-member? (claim-set-claims cset) c))
    (set-remove! (claim-set-claims cset) c)
    (match c
      [(claim-dep child parent)
       (when-let ([children (hash-ref (claim-set-child-map cset) parent #f)])
         (when (set-member? children child)
           (claim-set-remove-child! cset parent child)))]
      [(claim-needs-child parent)
       (claim-set-remove-parent! cset parent)]
      [else (void)]
      )))

; Record a new passing test in `claim-set-valid-child-masks`.
;
; This can result in a `claim-needs-child` being disproved, if the new test
; shows that the parent is enabled but no child is.
(define (claim-set-add-passing-test! cset inp)
  ; Copy the child-map to avoid concurrent modification, in the case where
  ; `claim-set-update-valid-child-masks!` removes a parent from the child-map.
  (for ([(parent children) (hash-copy (claim-set-child-map cset))]
        #:when (vector-ref inp parent))
    (define inp-mask (claim-set-child-mask cset parent inp))
    (claim-set-update-valid-child-masks! cset parent (mask-set-add inp-mask)))
  )

(define (claim-set-update! cset inp)
  (claim-set-filter! cset (lambda (c) (claim-set-eval-claim cset c inp)))
  (claim-set-add-passing-test! cset inp)
  )

; Compute a bit mask showing the children of `parent` that are enabled in
; `inp`.
(define (claim-set-child-mask cset parent inp)
  (for/fold ([mask 0]) ([child (hash-ref (claim-set-child-map cset) parent '())]
                        #:when (vector-ref inp child))
    (bitwise-ior mask (arithmetic-shift 1 child))))

; Check if the set of children of `parent` that are enabled in `inp` is known
; to be sufficient to satisfy `(claim-needs-child parent)`.  In other words,
; has any subset of those children resulted in a passing test?
(define (claim-set-valid-children-enabled? cset parent inp)
  (define m (claim-set-child-mask cset parent inp))
  (if-let ([masks (hash-ref (claim-set-valid-child-masks cset) parent)])
    (mask-set-has-subset? masks m)
    (raise (format "tried to check child validity, but ~a has no children~n" parent))))

; Evaluate `c` (which should be present in `cset`) on `cfg`.  Returns `#t` if
; the claim is possibly satisfied by `cfg`, and `#f` if it is definitely
; unsatisfied.
;
; This function is suitable for use on symbolic `cfg`.
(define (claim-set-eval-claim cset c cfg)
  (match c
    [(claim-dep a b) (=> (vector-ref cfg a) (vector-ref cfg b))]
    [(claim-antidep a b) (=> (vector-ref cfg a) (! (vector-ref cfg b)))]
    [(claim-fixed a val) (<=> (vector-ref cfg a) val)]
    [(claim-needs-child a)
     (=> (vector-ref cfg a)
         (apply ||
           (for/list ([child (hash-ref (claim-set-child-map cset) a '())])
             (vector-ref cfg child))))]
  ))

; Evaluate all claims in `cfg`.  Returns `#t` if each claim is possibly
; satisfied, and `#f` if at least one claim is definitely unsatisfied.
;
; This function is suitable for use on symbolic `cfg`.
(define (claim-set-eval cset cfg)
  (apply &&
    (for/list ([c (claim-set-claims cset)])
      (claim-set-eval-claim cset c cfg))))

; Evaluate `c` (which should be present in `cset`) on `cfg`.  Returns `#t` if
; the claim is definitely satisfied by `cfg`, and `#f` if it is definitely
; unsatisfied, and `'unknown` otherwise.
;
; This function is *not* suitable for use on symbolic `cfg`.
(define (claim-set-eval-claim-precise cset c cfg)
  (match c
    ; `needs-child` needs special handling - it relies on an approximation of
    ; the children of the feature.
    [(claim-needs-child a)
     (cond
       ; If `a` is disabled, we don't care about its children.
       [(not (vector-ref cfg a)) #t]
       ; If all children are disabled, definitely fail.
       [(for/and ([child (hash-ref (claim-set-child-map cset) a)])
          (not (vector-ref cfg child)))
        #f]
       ; If a previous passing test revealed that `a` needs only a subset of
       ; the currently enabled children, definitely pass.
       [(claim-set-valid-children-enabled? cset a cfg) #t]
       [else 'unknown])]
    ; All other claims are evaluated precisely by ordinary `eval-claim`.
    [_ (claim-set-eval-claim cset c cfg)]
  ))

; TODO - deprecated - use `(when out (claim-set-update! cset inp))` instead
(define (claim-set-update cset inp out)
  (when out (claim-set-update! cset inp)))

(define (claim-uses i c)
  (match c
    [(claim-dep a b) (or (= a i) (= b i))]
    [(claim-antidep a b) (or (= a i) (= b i))]
    [(claim-fixed a _) (= a i)]
    [(claim-needs-child a) (= a i)]
  ))

(define (claim-set-remove-feature! cset i)
  (claim-set-filter!
    cset
    (lambda (c) (not (claim-uses i c)))))


; Constraint evaluation

(define (eval-constraint c cfg)
  (define (loop c) (eval-constraint c cfg))
  (match c
    [(? integer?) (vector-ref cfg c)]
    [(? boolean?) c]
    [(cons '&& args) (apply && (map loop args))]
    [(cons '|| args) (apply || (map loop args))]
    [(cons '! args) (apply ! (map loop args))]
    [(cons '=> args) (apply => (map loop args))]
    [(cons '<=> args) (apply <=> (map loop args))]
    ))

; Split a constraint into a list of clauses that are combined with `&&`.
(define (constraint-clauses c)
  (match c
    [(cons '&& cs)
     (append-map constraint-clauses cs)]
    [else (list c)]))

