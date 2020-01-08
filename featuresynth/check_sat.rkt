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

(define (check-sat concrete-fm)
  (define solver (z3-named*))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))
  (solver-assert solver
                 (list (eval-feature-model concrete-fm symbolic-config)))
  (sat? (solver-check solver)))

(define (check-sat-must concrete-fm names features)
  (define solver (z3-named*))
  (define symbolic-config (?*config (feature-model-num-features concrete-fm)))
  (define (name->symb n)
    (vector-ref symbolic-config (vector-member (name n 0) (name-list-features names))))
  (solver-assert solver
                 (list (eval-feature-model concrete-fm symbolic-config)))
  (for/list ([n features])
    (begin
      (solver-push solver)
      (solver-assert solver
                     (list (not (name->symb n))))
      (define v (unsat? (solver-check solver)))
      (solver-pop solver 1) v)))
