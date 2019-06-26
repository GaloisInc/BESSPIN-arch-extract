#lang rosette

(provide
  simplify
  )

(require racket/random)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "synthesis.rkt")
(require "eval.rkt")


; Filter out items of `xs` that are implied by the other `xs`.  Calls `imp`
; with two arguments: the `x` being checked, and a list of remaining
; non-implied `xs`.
(define (filter-implied imp xs)
  (eprintf "items: ~a~n" xs)
  (define implied (make-vector (sequence-length xs) #f))
  (define (non-implied-xs)
    (for/list ([x xs] [imp implied] #:when (not imp)) x))
  (for ([(x i) (in-indexed xs)])
    (vector-set! implied i #t)
    (if (not (imp x (non-implied-xs)))
      (vector-set! implied i #f)
      (eprintf "discard redundant item ~a: ~a~n" i x)))
  (non-implied-xs))

(define (simple-solver)
  (define solver (z3-named*))
  (lambda args
    (match args
      [`(assert ,@cs) (solver-assert solver (flatten cs))]
      [`(check) (solver-check solver)]
      [`(check ,@cs)
        (solver-push solver)
        (solver-assert solver (flatten cs))
        (begin0
          (solver-check solver)
          (solver-pop solver))]
      )))

; Remove clauses from (feature-model-constraint fm) that are implied by the
; rest of the model.  Returns an updated feature model.
(define (fast-simplify-constraint concrete-fm)
  (define solver (simple-solver))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))

  (match-define (feature-model fs gs ds c) concrete-fm)
  (define concrete-fm-unconstrained (feature-model fs gs ds #t))

  (solver 'assert (eval-feature-model concrete-fm-unconstrained symbolic-config))

  (define (check-imp c cs)
    (unsat?
      (solver 'check
        (eval-constraint (cons '&& cs) symbolic-config)
        (eval-constraint `(! ,c) symbolic-config))))

  (define kept (filter-implied check-imp (constraint-clauses c)))
  (struct-copy feature-model concrete-fm
    [constraint (cons '&& kept)]))

; Remove dependencies from concrete-fm that are implied by the rest of the
; model.  Returns an updated feature model.
(define (fast-simplify-dependencies concrete-fm)
  (define solver (simple-solver))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))

  (match-define (feature-model fs gs ds c) concrete-fm)
  (define concrete-fm-unconstrained (feature-model fs gs #() c))

  (solver 'assert (eval-feature-model concrete-fm-unconstrained symbolic-config))

  (define (eval-dep d)
    (eval-dependency concrete-fm-unconstrained d symbolic-config))
  (define (check-imp d ds)
    (unsat?
      (solver 'check
        (! (eval-dep d))
        (map eval-dep ds))))

  (define kept (filter-implied check-imp ds))
  (struct-copy feature-model concrete-fm
    [dependencies (list->vector kept)]))

(define (accumulator)
  (define xs '())
  (match-lambda*
    [`(add ,x) (set! xs (cons x xs))]
    [`(get) xs]
    ))

(define (make-worker2 symbolic-fm concrete-fm [test-acc #f])
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (when (not test-acc) (set! test-acc (accumulator)))

  (for ([t (test-acc 'get)])
    (synth 'test t))

  (define expr #t)

  (define (find-equiv expr)
    (let loop ()
      (match (synth 'synthesize-with expr)
        [(? feature-model? fm) fm]
        [(? vector? new-inp)
         (define t (cons new-inp (eval-feature-model concrete-fm new-inp)))
         (synth 'test t)
         (test-acc 'add t)
         (loop)]
        [#f #f])))

  (match-lambda*
    [`(get-expr) expr]
    [`(add-clause ,e) (set! expr (&& expr e))]
    ; Check whether the current feature model and expr, plus additional
    ; constraint `e`, captures a feature model equivalent to `concrete-fm`.
    [`(check-clause ,e)
      (find-equiv (&& expr e))]
    ; `check-symbolic-fm`, then `set-symbolic-fm` if it succeeds.
    [`(try-clause ,e)
      (define new-expr (&& expr e))
      (define ok (find-equiv new-expr))
      (when ok (set! expr new-expr))
      ok]

    [`(synthesize) (find-equiv expr)]
    ))

; Remove clauses from `(feature-model-constraint symbolic-fm)` that can be
; replicated using other, non-constraint parts of the model.
(define (simplify-constraint symbolic-fm concrete-fm test-acc)
  (define (check-imp c cs)
    (define symbolic-fm*
      (struct-copy feature-model symbolic-fm [constraint (cons '&& cs)]))
    (define synth (make-worker2 symbolic-fm* concrete-fm test-acc))
    (synth 'synthesize))

  (define c (feature-model-constraint symbolic-fm))
  (define kept (filter-implied check-imp (constraint-clauses c)))
  (struct-copy feature-model symbolic-fm
    [constraint (cons '&& kept)]))

; Remove dependencies from symbolic-fm that can be replicated using other parts
; of the model.
(define (simplify-dependencies symbolic-fm concrete-fm test-acc)
  (define (check-imp d ds)
    (define symbolic-fm*
      (struct-copy feature-model symbolic-fm [dependencies (list->vector ds)]))
    (define synth (make-worker2 symbolic-fm* concrete-fm test-acc))
    (synth 'synthesize))

  (define ds (feature-model-dependencies symbolic-fm))
  (define kept (filter-implied check-imp ds))
  (struct-copy feature-model symbolic-fm
    [dependencies (list->vector kept)]))

; Remove groups from symbolic-fm that can be replicated using other parts of
; the model.
(define (simplify-groups symbolic-fm concrete-fm test-acc)
  (define (check-imp g gs)
    (define symbolic-fm*
      (struct-copy feature-model symbolic-fm [groups (list->vector gs)]))
    (define synth (make-worker2 symbolic-fm* concrete-fm test-acc))
    (synth 'synthesize))

  (define gs (feature-model-groups symbolic-fm))
  (define kept (filter-implied check-imp gs))
  (struct-copy feature-model symbolic-fm
    [groups (list->vector kept)]))


; Find an instance of `symbolic-fm` that is equivalent to `concrete-fm`, using
; `synth`.  This may involve adding additional tests to `synth` whose outputs
; are based on the behavior of `concrete-fm`, so it's not valid to use the same
; `synth` with different `concrete-fm`s.
;
; Returns a concrete feature model on success, or `#f` if no such model exists.
(define (find-equiv synth symbolic-fm concrete-fm [expr #t])
  (let loop ()
    (match (synth 'synthesize-with expr)
      [(? feature-model? fm) fm]
      [(? vector? new-inp)
       (synth 'test (cons new-inp (eval-feature-model concrete-fm new-inp)))
       (loop)]
      [#f #f])))

; Identify forced-on/forced-off features, and set the appropriate
; force-on/force-off flags in the feature model.
(define (force-fixed-features concrete-fm)
  (define conc-synth (oracle-guided-synthesis+ concrete-fm))

  (define features
    (for/vector ([(f i) (in-indexed (feature-model-features concrete-fm))])
      (cond
        [(conc-synth 'prove-fixed i #t)
         (struct-copy feature f [force-on #t])]
        [(conc-synth 'prove-fixed i #f)
         (struct-copy feature f [force-off #t])]
        [else f])))

  (struct-copy feature-model concrete-fm [features features]))

; Copy force-on/force-off flags from `fm1` to `fm2`.  Also sets parent and
; group IDs to -1 for features that have either flag set.
(define (copy-force-flags fm1 fm2)
  (define (copy f1 f2)
    (define on (feature-force-on f1))
    (define off (feature-force-off f1))
    (if (or on off)
      (struct-copy feature f2
        [force-on on] [force-off off]
        [parent-id -1] [group-id -1])
      (struct-copy feature f2 [force-on on] [force-off off])))
  (define new-features
    (for/vector ([f1 (feature-model-features fm1)]
                 [f2 (feature-model-features fm2)])
      (copy f1 f2)))
  (struct-copy feature-model fm2 [features new-features]))

(define (simplify concrete-fm)
  (define orig-concrete-fm concrete-fm)


  ; Concrete-only simplification.  These are fast to compute, and make life
  ; easier for the solver once we get into the symbolic cases.

  ; Set force-on/force-off for any features detected to be unconfigurable.
  (set! concrete-fm (force-fixed-features concrete-fm))

  ; Remove any clauses or dependencies that are obviously redundant.
  (set! concrete-fm (fast-simplify-constraint concrete-fm))
  (set! concrete-fm (fast-simplify-dependencies concrete-fm))


  (define symbolic-fm (feature-model->symbolic concrete-fm))
  ; Tset accumulator.  This lets us share test results across multiple
  ; `make-worker` calls.
  (define test-acc (accumulator))

  ; Add some extra groups and dependencies for the solver to work with.
  (set! symbolic-fm 
    (struct-copy feature-model symbolic-fm
      [dependencies (vector-append (feature-model-dependencies symbolic-fm)
                                   (build-vector 2 (lambda (_) (?*dependency))))]
      [groups (vector-append (feature-model-groups symbolic-fm)
                             (build-vector 2 (lambda (_) (?*group))))]
      ))

  ; Make force-on/force-off concrete.
  (set! symbolic-fm (copy-force-flags concrete-fm symbolic-fm))

  ; Remove unnecessary constraints, dependencies, and groups from symbolic-fm.
  ; Order matters: removing a constraint may add a new group or dependency, if
  ; one is available.
  (set! symbolic-fm (simplify-constraint symbolic-fm orig-concrete-fm test-acc))
  (set! symbolic-fm (simplify-dependencies symbolic-fm orig-concrete-fm test-acc))
  (set! symbolic-fm (simplify-groups symbolic-fm orig-concrete-fm test-acc))


  ((make-worker2 symbolic-fm orig-concrete-fm test-acc) 'synthesize))
