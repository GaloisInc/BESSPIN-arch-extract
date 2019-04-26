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
  (set!-values (symbolic-fm concrete-fm)
               (fast-simplify-constraint symbolic-fm concrete-fm))
  concrete-fm)

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

