#lang rosette

(provide
  check-sat
  check-sat-must
)

(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "synthesis.rkt")
(require "ftree.rkt")

;; Check that concrete-fm is satisfiable
(define (check-sat concrete-fm)
  (define solver (z3-named*))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))
  (solver-assert solver
                 (list (eval-feature-model concrete-fm symbolic-config)))
  (sat? (solver-check solver)))

;; Given a list of feature indices, `features`, return a list E such that
;; E[i] <=> concrete-fm implies feature `features`[i]
(define (check-sat-must concrete-fm idxlist)
  (define solver (z3-named*))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))
  (solver-assert solver
                 (list (eval-feature-model concrete-fm symbolic-config)))
  (for/list ([i idxlist])
    (begin
      (solver-push solver)
      (solver-assert solver (list (not (vector-ref symbolic-config i))))
      (define v (unsat? (solver-check solver)))
      (solver-pop solver 1) v)))
