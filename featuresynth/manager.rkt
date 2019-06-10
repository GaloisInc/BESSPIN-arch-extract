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


(define (run-manager strategy-specs oracle-spec oracle-args init-inputs init-tests
                     [test-record-port #f])
  (define strategy-chans
    (for/vector ([strat strategy-specs])
      (unless (place-message-allowed? strat) (raise "bad strategy"))
      (place/context chan (run-strategy* strat chan))))
  (unless (place-message-allowed? oracle-spec) (raise "bad oracle"))
  (define oracle-chan
    (place/context chan (run-oracle* oracle-spec oracle-args chan)))
  (define seen-tests (mutable-set))

  (define strategy-reactive (make-vector (vector-length strategy-chans) #f))
  (define strategy-has-recovery (make-vector (vector-length strategy-chans) #f))
  (define strategy-failed (make-vector (vector-length strategy-chans) #f))
  (define (all-strategies-failed)
    (for/and ([reactive strategy-reactive]
              [failed strategy-failed])
      (or reactive failed)))
  (define (attempt-recovery)
    (define idx (vector-member #t strategy-has-recovery))
    (if (not idx) #f
      (begin
        (printf "attempting recovery using strategy ~a (~a)~n"
                idx (list-ref strategy-specs idx))
        (place-channel-put (vector-ref strategy-chans idx) '(recover))
        (vector-fill! strategy-failed #f))))

  (define (broadcast-strategies msg)
    (for ([chan strategy-chans])
      (place-channel-put chan msg)))
  (define (dispatch-test t)
    (broadcast-strategies `(test ,@t))
    (when test-record-port
      (writeln t test-record-port)
      (flush-output test-record-port)))
  (define (dispatch-input inp)
    (place-channel-put oracle-chan inp))

  ; Initialization
  (for ([t init-tests])
    (set-add! seen-tests (first t))
    (dispatch-test t))
  (for ([inp init-inputs])
    (when (not (set-member? seen-tests inp))
      (dispatch-input `(,inp ()))))
  ; Don't start the strategies until all `init-inputs` have been evaluated by
  ; the oracle.
  (define start-countdown (length init-inputs))
  (printf "manager: waiting for ~a results before starting strategies~n" start-countdown)

  (define strategy-evt
    (apply choice-evt
      (for/list ([(chan i) (in-indexed strategy-chans)])
        (handle-evt chan (lambda (x) (cons i x))))))
  (define any-evt
    (choice-evt
      (handle-evt strategy-evt (lambda (x) (cons 'strategy x)))
      (handle-evt oracle-chan (lambda (x) (list 'oracle x)))))

  (define (loop)
    (define evt (sync any-evt))
    (define result (handle evt))
    ;(define-values (result-lst cpu-time real-time gc-time)
    ;  (time-apply handle (list evt)))
    ;(define result (car result-lst))
    ;(printf "handled msg in ~a / ~a / ~a ms~n" cpu-time real-time gc-time)
    (if (eq? result 'continue) (loop) result))
  ; Handle a single event.  Returns 'continue if the event loop should keep
  ; running, or any other value to indicate it should terminate and return that
  ; value.
  (define (handle evt)
    (match evt
      [`(strategy ,i input ,inp ,meta)
        (when (not (set-member? seen-tests inp))
            (set-add! seen-tests inp)
            (dispatch-input `(,inp ,meta)))
        'continue]
      [`(strategy ,i solution ,fmv)
        (vector->feature-model fmv)]
      [`(strategy ,i fix-feature ,idx ,val)
        (for ([chan strategy-chans])
          (place-channel-put chan `(fix-feature ,idx ,val)))
        'continue]
      [`(strategy ,i property reactive ,val)
        (vector-set! strategy-reactive i val)
        'continue]
      [`(strategy ,i property has-recovery ,val)
        (vector-set! strategy-has-recovery i val)
        'continue]
      [`(strategy ,i property failed ,val)
        (printf "FAILED: strategy ~a (~a)~n"
                i (list-ref strategy-specs i))
        (vector-set! strategy-failed i val)
        (if (and (all-strategies-failed) (not (attempt-recovery)))
          (begin
            (printf "All strategies failed, and no recovery options are available.~n")
            (printf " -- SYNTHESIS FAILED --~n")
            #f)
          'continue)]
      [`(strategy ,i recovery-done)
        (broadcast-strategies '(start))
        'continue]
      [`(oracle ,test)
        (dispatch-test test)
        (when (> start-countdown 0)
          (set! start-countdown (sub1 start-countdown))
          (printf "manager: waiting for ~a results~n" start-countdown)
          (when (= start-countdown 0)
            (printf "manager: starting strategies!~n")
            (broadcast-strategies '(start))))
        'continue]
      [evt
        (displayln evt)
        (raise "bad event")]
      ))

  (loop))
