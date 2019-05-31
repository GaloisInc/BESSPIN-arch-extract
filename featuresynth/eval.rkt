#lang rosette

(provide
  eval-feature-model
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
  ; Lookup table for potential children of a feature.  For each
  ; `(claim-needs-child a)` in `claims`, `child-map` has an entry for `a`,
  ; containing the set of all `b`s for which there is a `(claim-dep b a)` in
  ; `claims`.  This is used for efficient evaluation of `claim-needs-child`
  ; claims.
  child-map
  ; List of observed passing tests.  `test-child-counts` indexes into this
  ; vector.
  [passing-tests #:mutable]
  ; Hash that maps features with `claim-needs-child` to lists of passing tests
  ; where that feature is enabled.  When a `claim-dep` is disproved (so the set
  ; of potential children shrinks), we re-scan the previous tests to check if
  ; the `claim-needs-child` has been disproved.
  needs-child-tests
  ; A BDD that maps each known passing test to `#t`, and all other tests to
  ; `#f`.
  [passing-bdd #:mutable]
) #:transparent)

(define (make-claim-set claims num-features)
  (define cs (for/mutable-set ([c claims]) c))

  (define child-map (make-hash))
  (for ([c cs] #:when (claim-needs-child? c))
    (hash-set! child-map (claim-needs-child-a c) (mutable-set)))
  (for ([c cs] #:when (claim-dep? c))
    (match-define (claim-dep child parent) c)
    (when-let ([children (hash-ref child-map parent #f)])
      (set-add! children child)))

  (define needs-child-tests (make-hash))
  (for ([parent (in-hash-keys child-map)])
    (hash-set! needs-child-tests parent '()))

  (claim-set
    cs
    child-map
    '()
    needs-child-tests
    (init-bdd num-features)
    ))

(define (claim-set-count cset)
  (set-count (claim-set-claims cset)))

(define (claim-set-member? cset c)
  (set-member? (claim-set-claims cset) c))

(define (update-claim-set-passing-tests! cset upd)
  (set-claim-set-passing-tests! cset (upd (claim-set-passing-tests cset))))

(define (update-claim-set-passing-bdd! cset upd)
  (set-claim-set-passing-bdd! cset (upd (claim-set-passing-bdd cset))))

; Remove each claim in `cs` from `cset`.  Returns a set of features for which
; `claim-needs-child` need to be rechecked (because the set of children has
; changed).
(define (claim-set-remove-many! cset cs)
  (match-define (claim-set claims child-map _ needs-child-tests _) cset)
  (define recheck (mutable-set))

  (for ([c cs] #:when (set-member? claims c))
    (set-remove! claims c)
    (match c
      [(claim-dep child parent)
       (when-let ([children (hash-ref child-map parent #f)])
         (set-remove! children child)
         ; Parent has fewer children, so an old test might now be able to
         ; disprove `claim-needs-child parent`.
         (set-add! recheck parent))]
      [(claim-needs-child parent)
       (hash-remove! child-map parent)
       (hash-remove! needs-child-tests parent)]
      [else (void)]
    ))

  recheck)

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

(define (bdd-has-sat? bdd nvars)
  (> (robdd-sat-count bdd nvars) 0))

(define (bdd-restrict-multi bdd ts fs)
  (define bdd1
    (for/fold ([bdd bdd]) ([i ts])
      (robdd-restrict bdd i #t)))
  (define bdd2
    (for/fold ([bdd bdd1]) ([i fs])
      (robdd-restrict bdd i #f)))
  bdd2)

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
       [(apply &&
          (for/list ([child (hash-ref (claim-set-child-map cset) a '())])
            (not (vector-ref cfg child))))
        #f]
       ; If a previous passing test revealed that `a` needs only a subset of
       ; the currently enabled children, definitely pass.
       [(bdd-has-sat?
          (robdd-and
            (claim-set-passing-bdd cset)
            (make-filter-bdd
              (list a)
              (for/list ([child (hash-ref (claim-set-child-map cset) a '())]
                         #:when (not (vector-ref cfg child))) child)
              (vector-length cfg)))
          (vector-length cfg))
        #t]
       [else 'unknown])]
    ; All other claims are evaluated precisely by ordinary `eval-claim`.
    [_ (claim-set-eval-claim cset c cfg)]
  ))

(define (claim-set-recheck-needs-child cset claim)
  (match-define (claim-needs-child parent) claim)
  (for/and ([cfg (hash-ref (claim-set-needs-child-tests cset) parent)])
    (claim-set-eval-claim cset claim cfg)))

(define (init-bdd num-features)
  (make-robdd
    #f
    (for/list ([i (in-range num-features)])
      (string->symbol (format "f~a" i)))))

(define (config->bdd cfg)
  (make-robdd
    (cons 'and
      (for/list ([(v i) (in-indexed cfg)])
        (define sym (string->symbol (format "f~a" i)))
        (if v sym `(not ,sym))))
    (for/list ([i (in-range (vector-length cfg))])
      (string->symbol (format "f~a" i)))))

(define (make-filter-bdd ts fs num-features)
  (make-robdd
    (cons 'and
      (append
        (for/list ([i ts]) (string->symbol (format "f~a" i)))
        (for/list ([i fs]) `(not ,(string->symbol (format "f~a" i))))))
    (for/list ([i (in-range num-features)])
      (string->symbol (format "f~a" i)))))

(define (claim-set-update cset inp out)
  (when out
    (claim-set-filter! cset (lambda (c) (claim-set-eval-claim cset c inp)))

    (define (add-inp ts) (cons inp ts))
    (define (add-inp-bdd bdd) (robdd-or bdd (config->bdd inp)))
    (update-claim-set-passing-tests! cset add-inp)
    (update-claim-set-passing-bdd! cset add-inp-bdd)
    (for ([parent (in-hash-keys (claim-set-child-map cset))]
          #:when (vector-ref inp parent))
      (hash-update! (claim-set-needs-child-tests cset) parent add-inp))
    ))

(define (claim-set-filter! cset f)
  (define disproved1
    (for/list ([c (claim-set-claims cset)] #:when (not (f c))) c))
  (define recheck1 (claim-set-remove-many! cset disproved1))

  (when (not (set-empty? recheck1))
    (define disproved2
      (for/list ([parent recheck1]
                 #:when (hash-has-key? (claim-set-child-map cset) parent)
                 [claim (in-value (claim-needs-child parent))]
                 #:when (not (claim-set-recheck-needs-child cset claim)))
        claim))
    (define recheck2 (claim-set-remove-many! cset disproved2))

    (when (not (set-empty? recheck2))
      (raise "impossible: got rechecks when processing only needs-child claims?"))))

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

