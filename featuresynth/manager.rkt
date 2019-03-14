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


(define (run-manager strategy-specs oracle-spec num-oracle-threads init-inputs)
  (define strategy-chans
    (for/vector ([strat strategy-specs])
      (unless (place-message-allowed? strat) (raise "bad strategy"))
      (place/context chan (run-strategy* strat chan))))
  (unless (place-message-allowed? oracle-spec) (raise "bad oracle"))
  (define oracle-chans
    (for/vector ([i (in-range num-oracle-threads)])
      (place/context chan (run-oracle* oracle-spec chan))))
  (define oracle-idle (make-vector num-oracle-threads #t))
  (define pending-tests '())
  (define seen-tests (mutable-set))

  (define (find-idle-oracle)
    (for/first ([(idle i) (in-indexed oracle-idle)] #:when idle) i))
  (define (dispatch-test t)
    (for ([chan strategy-chans])
      (place-channel-put chan t)))
  ; When an input arrives, try to send it to an idle oracle thread.
  (define (dispatch-input inp)
    (if-let ([i (find-idle-oracle)])
      (begin
        (place-channel-put (vector-ref oracle-chans i) inp)
        (vector-set! oracle-idle i #f))
      (set! pending-tests (cons inp pending-tests))))
  ; When an oracle thread finishes, try to send it a pending input.
  (define (request-input i)
    (if (not (null? pending-tests))
      (begin
        (place-channel-put (vector-ref oracle-chans i) (car pending-tests))
        (set! pending-tests (cdr pending-tests)))
      (vector-set! oracle-idle i #t)))

  (for ([inp init-inputs])
    (dispatch-input `(,inp ())))

  (define strategy-evt
    (apply choice-evt
      (for/list ([(chan i) (in-indexed strategy-chans)])
        (handle-evt chan (lambda (x) (list i x))))))
  (define oracle-evt
    (apply choice-evt
      (for/list ([(chan i) (in-indexed oracle-chans)])
        (handle-evt chan (lambda (x) (list i x))))))
  (define any-evt
    (choice-evt
      (handle-evt strategy-evt (lambda (x) (cons 'strategy x)))
      (handle-evt oracle-evt (lambda (x) (cons 'oracle x)))))

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
      [`(oracle ,i ,test)
       (dispatch-test test)
       (request-input i)
       (loop)]
      [else (raise "bad event")]
      ))

  (loop))
