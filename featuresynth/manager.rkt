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


(define (run-manager strategy-specs oracle-spec init-inputs
                     [test-record-port #f])
  (define strategy-chans
    (for/vector ([strat strategy-specs])
      (unless (place-message-allowed? strat) (raise "bad strategy"))
      (place/context chan (run-strategy* strat chan))))
  (unless (place-message-allowed? oracle-spec) (raise "bad oracle"))
  (define oracle-chan (place/context chan (run-oracle* oracle-spec chan)))
  (define seen-tests (mutable-set))

  (define (dispatch-test t)
    (define msg (cons 'test t))
    (for ([chan strategy-chans])
      (place-channel-put chan msg))
    (when test-record-port
      (writeln t test-record-port)))
  (define (dispatch-input inp)
    (place-channel-put oracle-chan inp))

  (for ([inp init-inputs])
    (dispatch-input `(,inp ())))

  (define strategy-evt
    (apply choice-evt
      (for/list ([(chan i) (in-indexed strategy-chans)])
        (handle-evt chan (lambda (x) (cons i x))))))
  (define any-evt
    (choice-evt
      (handle-evt strategy-evt (lambda (x) (cons 'strategy x)))
      (handle-evt oracle-chan (lambda (x) (list 'oracle x)))))

  (define quit-votes 0)

  (define (loop)
    (match (sync any-evt)
      [`(strategy ,i input ,inp ,meta)
        (when (not (set-member? seen-tests inp))
            (set-add! seen-tests inp)
            (dispatch-input `(,inp ,meta)))
        (loop)]
      [`(strategy ,i solution ,fmv)
        (vector->feature-model fmv)]
      [`(strategy ,i vote-quit ,quiet)
        (when (not quiet)
          (for ([chan strategy-chans])
            (place-channel-put chan '(vote-quit))))
        (set! quit-votes (+ 1 quit-votes))
        (if (< quit-votes (vector-length strategy-chans))
          (loop)
          #f)]
      [`(strategy ,i unvote-quit)
        (set! quit-votes (- quit-votes 1))]
      [`(strategy ,i fix-feature ,idx ,val)
        (for ([chan strategy-chans])
          (place-channel-put chan `(fix-feature ,idx ,val)))
        (loop)]
      [`(oracle ,test)
       (dispatch-test test)
       (loop)]
      [evt
        (displayln evt)
        (raise "bad event")]
      ))

  (loop))
