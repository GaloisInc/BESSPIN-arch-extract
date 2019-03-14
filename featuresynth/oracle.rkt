#lang racket

(provide
  run-oracle
  )

(require "build.rkt")
(require "eval.rkt")
(require "util.rkt")


(define (oracle-eval-fm fm chan)
  (for ([msg (in-place-channel chan)])
    (match-define `(,inp ,meta) msg)
    (define out (eval-feature-model fm inp))
    (place-channel-put chan `(,inp ,out ,meta))))

(define (run-oracle spec chan)
  (match spec
    [(list 'eval-fm fm)
     (oracle-eval-fm (vector->feature-model fm) chan)]
    ))
