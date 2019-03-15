#lang racket

(provide
  run-manager
  )

(require racket/place)

(require "types.rkt")
(require "util.rkt")
(require "strategy.rkt")
(require "oracle.rkt")
(require "build.rkt")


(define run-strategy* run-strategy)
(define run-oracle* run-oracle)


(define (run-manager strategy-specs oracle-spec init-inputs)
  (define strategy-chans
    (for/vector ([strat strategy-specs])
      (unless (place-message-allowed? strat) (raise "bad strategy"))
      (place/context chan (run-strategy* strat chan))))
  (unless (place-message-allowed? oracle-spec) (raise "bad oracle"))
  (define oracle-chan (place/context chan (run-oracle* oracle-spec chan)))
  (define seen-tests (mutable-set))

  (define (dispatch-test t)
    (for ([chan strategy-chans])
      (place-channel-put chan t)))
  (define (dispatch-input inp)
    (place-channel-put oracle-chan inp))

  (for ([inp init-inputs])
    (dispatch-input `(,inp ())))

  (define strategy-evt
    (apply choice-evt
      (for/list ([(chan i) (in-indexed strategy-chans)])
        (handle-evt chan (lambda (x) (list i x))))))
  (define any-evt
    (choice-evt
      (handle-evt strategy-evt (lambda (x) (cons 'strategy x)))
      (handle-evt oracle-chan (lambda (x) (list 'oracle x)))))

  (define (loop)
    (match (sync any-evt)
      [`(strategy ,i (,result ,meta))
       (cond
         [(and (vector? result)
               (> (vector-length result) 0)
               (eq? 'struct:feature-model (vector-ref result 0)))
          (vector->feature-model result)]
         [(false? result) result]
         [else
          (when (not (set-member? seen-tests result))
            (set-add! seen-tests result)
            (dispatch-input `(,result ,meta)))
          (loop)]
         )]
      [`(oracle ,test)
       (dispatch-test test)
       (loop)]
      [else (raise "bad event")]
      ))

  (loop))
