#lang rosette

(provide
  check-sat
)

(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "synthesis.rkt")

(define (check-sat concrete-fm)
  (define solver (z3-named*))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))
  (solver-assert solver
                 (flatten (eval-feature-model concrete-fm symbolic-config)))
  (sat? (solver-check solver)))
