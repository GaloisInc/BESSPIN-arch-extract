#lang rosette

(provide
  simplify-feature-model
  )

(require racket/random)
(require rosette/solver/smt/z3)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "synthesis.rkt")


(define (make-worker symbolic-fm concrete-fm [tests-raw '()])
  ; symbolic-fm already defined as arg
  (define expr #t)
  (define tests
    (for/list ([(inp out meta) (test-parts tests-raw)])
      (cons inp out)))

  (define (find-equiv symbolic-fm expr)
    (define synth (oracle-guided-synthesis+ symbolic-fm))
    (for ([t tests]) (synth 'test t))
    (let loop ()
      (match (synth 'synthesize-with expr)
        [(? feature-model? fm) fm]
        [(? vector? new-inp)
         (define t (eval-feature-model concrete-fm new-inp))
         (synth 'test (cons new-inp t))
         (set! tests (cons tests t))
         (loop)]
        [#f #f])))

  (match-lambda*
    [`(get-symbolic-fm) symbolic-fm]
    [`(set-symbolic-fm ,fm) (set! symbolic-fm fm)]
    ; Return a feature model or #f, indicating whether or not `fm` captures a
    ; feature model equivalent to `concrete-fm`.
    [`(check-symbolic-fm ,fm)
      (find-equiv fm expr)]
    ; `check-symbolic-fm`, then `set-symbolic-fm` if it succeeds.
    [`(try-symbolic-fm ,fm)
      (define ok (find-equiv fm expr))
      (when ok (set! symbolic-fm fm))
      ok]

    [`(get-expr) expr]
    [`(add-clause ,e) (set! expr (&& expr e))]
    ; Check whether the current feature model and expr, plus additional
    ; constraint `e`, captures a feature model equivalent to `concrete-fm`.
    [`(check-clause ,e)
      (find-equiv symbolic-fm (&& expr e))]
    ; `check-symbolic-fm`, then `set-symbolic-fm` if it succeeds.
    [`(try-clause ,e)
      (define new-expr (&& expr e))
      (define ok (find-equiv symbolic-fm new-expr))
      (when ok (set! expr new-expr))
      ok]

    [`(check ,fm ,e)
      (find-equiv fm e)]
    [`(try ,fm ,e)
      (define new-expr (&& expr e))
      (define ok (find-equiv fm new-expr))
      (when ok
        (set! symbolic-fm fm)
        (set! expr new-expr))
      ok]

    [`(get-new-concrete-fm)
      (find-equiv symbolic-fm expr)]
    ))


; Given a symbolic feature model and a concrete instantiation, synthesize an
; equivalent concrete model from increasingly restricted variants of the
; symbolc model.  Essentially, we start from the given concrete feature model,
; then try to synthesize an equivalent model with one less constraint, one less
; dependency, etc.
;
; Simplification is another oracle-guided synthesis process, using the previous
; concrete model as the oracle.  We use the provided `tests` to initialize the
; synthesis procedure, so it doesn't need to start from scratch.
(define (simplify-feature-model symbolic-fm concrete-fm tests)
  (define worker (make-worker symbolic-fm concrete-fm tests))

  ;(simplify-constraint worker)
  (remove-deps worker)

  (worker 'get-new-concrete-fm))


; Try removing each clause of `symbolic-fm`'s explicit constraint.  The result
; is a new symbolic feature model that captures at least one concrete feature
; model equivalent to `concrete-fm`.
(define (simplify-constraint worker)
  (match-define (feature-model fs gs ds c) (worker 'get-symbolic-fm))
  (define clauses (constraint-clauses c))
  (define removed-clauses (make-vector (length clauses) #f))
  (define (new-constraint)
    (cons '&&
      (for/list ([c clauses] [rem removed-clauses] #:when (not rem)) c)))
  (define (new-symbolic-fm) (feature-model fs gs ds (new-constraint)))

  (define (suggested-constraint c)
    (match c
      [(? integer? i) (feature-force-on (vector-ref fs i))]
      [`(! ,(? integer? i)) (feature-force-off (vector-ref fs i))]
      [else #t]
      ))

  (for ([(c i) (in-indexed clauses)])
    (vector-set! removed-clauses i #t)
    (if (worker 'try (new-symbolic-fm) (suggested-constraint c))
      (printf "simplify: removed clause ~a~n" i)
      (vector-set! removed-clauses i #f)))

  ; Last successful `try-symbolic-fm` is kept in `worker` automatically.
  )

(define (remove-deps worker)
  (match-define (feature-model fs gs ds c) (worker 'get-symbolic-fm))
  (define (new-symbolic-fm n)
    (feature-model fs gs (vector-take ds n) c))

  (let loop ([n (- (vector-length ds) 1)])
    (printf "trying with ~a explicit deps~n" n)
    (define ok (worker 'try-symbolic-fm (new-symbolic-fm n)))
    (when (and ok (> n 0))
      (loop (- n 1)))))


; Split a constraint into a list of clauses that are combined with `&&`.
(define (constraint-clauses c)
  (match c
    [(cons '&& cs)
     (append-map constraint-clauses cs)]
    [else (list c)]))

; Return a vector of booleans of size `(length cs)`, where each position is
; `#t` if the corresponding clause in `cs` is implied by the non-`#t` clauses.
(define (find-implied-clauses concrete-fm)
  (define solver (z3))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))

  (match-define (feature-model fs gs ds c) concrete-fm)
  (define concrete-fm-unconstrained (feature-model fs gs ds #t))
  (define cs (constraint-clauses c))
  (printf "clauses: ~a~n" cs)

  (solver-assert
    solver
    (list (eval-feature-model concrete-fm-unconstrained symbolic-config)))

  (define implied (make-vector (length cs) #f))
  (define (non-implied-clauses)
    (for/list ([c cs] [imp implied] #:when (not imp)) c))
  (define (model-implies-clause c)
    (solver-push solver)
    (solver-assert solver
      (list
        (eval-constraint (cons '&& (non-implied-clauses)) symbolic-config)
        (eval-constraint `(! ,c) symbolic-config)))
    (begin0
      (unsat? (solver-check solver))
      (solver-pop solver)))

  (for ([(c i) (in-indexed cs)])
    (vector-set! implied i #t)
    (if (not (model-implies-clause c))
      (vector-set! implied i #f)
      (printf "disproved clause ~a: ~a~n" i c)))

  implied)

; Fast pass for removing unneeded constraints.  This requires `concrete-fm` to
; be an instantiation of exactly `symbolic-fm`.
(define (fast-simplify-constraint symbolic-fm concrete-fm)
  (define implied (find-implied-clauses concrete-fm))
  (define (filter-constraint fm)
    (match-define (feature-model fs gs ds c) fm)
    (define reduced-c
      (cons '&&
        (for/list ([(c i) (in-indexed (constraint-clauses c))]
                   [imp implied]
                   #:when (not imp)) c)))
    (feature-model fs gs ds reduced-c))
  (values
    (filter-constraint symbolic-fm)
    (filter-constraint concrete-fm)))


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
