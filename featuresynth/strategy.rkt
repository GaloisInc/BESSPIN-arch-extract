#lang racket

(provide
  strategy-bitflip
  strategy-distinguish
  run-strategy
  )

(require racket/place)

(require "util.rkt")
(require "eval.rkt")
(require "types.rkt")
(require "synthesis.rkt")


(define (vote-quit chan [quiet #f])
  (place-channel-put chan '(vote-quit ,quiet)))
(define (unvote-quit chan)
  (place-channel-put chan '(unvote-quit)))

(define (fail-flag chan [quiet #f])
  (define val #f)
  (lambda (new-val)
    (if (eq? new-val 'get)
      val
      (begin
        (when (and new-val (not val))
          (vote-quit chan quiet))
        (when (and val (not new-val))
          (unvote-quit chan))
        (set! val new-val)))))

(define (strategy-bitflip chan)
  (printf "strategy-bitflip running~n")
  (vote-quit chan #t)
  (for ([msg (in-place-channel chan)])
    (match msg
      [`(test ,inp ,out ,meta)
        (when (and out (not (assoc 'bitflip-depth meta)))
          (for ([i (in-range (vector-length inp))])
            (define inp*
              (for/vector ([(v j) (in-indexed inp)])
                (if (= i j) (not v) v)))
            (place-channel-put chan `(input ,inp* ((bitflip-depth . 1))))))]
      [else (void)]
      )))

(define (strategy-distinguish symbolic-fm chan)
  (printf "strategy-distinguish running~n")
  (define failed (fail-flag chan))
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (for ([msgs (in-place-channel-chunks chan)])
    (for ([msg msgs])
      (match msg
        [`(test ,inp ,out ,meta)
          (synth 'test `(,inp . ,out))]
        [`(fix-feature ,idx ,val)
          (synth 'fix-feature idx val)
          (failed #f)]
        [else (void)]))

    (when (not (failed 'get))
      (define result (synth 'synthesize))
      (cond
        [(feature-model? result)
         (place-channel-put chan `(solution ,(struct->vector* result)))]
        [(vector? result)
         (place-channel-put chan `(input ,result ()))]
        [(false? result)
         (failed #t)]))))

(define (strategy-disprove symbolic-fm chan)
  (printf "strategy-disprove running~n")
  (define failed (fail-flag chan))
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define claims (all-claims symbolic-fm))
  (printf "begin with ~a claims~n" (length claims))
  (for ([msgs (in-place-channel-chunks chan)])
    (define test-count 0)
    (define positive-count 0)
    (for ([msg msgs])
      (match msg
        [`(test ,inp ,out ,meta)
          (synth 'test `(,inp . ,out))
          (set! test-count (+ 1 test-count))
          (when out
            (set! claims (filter (lambda (c) (eval-claim c inp)) claims))
            (set! positive-count (+ 1 positive-count)))]
        [`(fix-feature ,idx ,val)
          (synth 'fix-feature idx val)
          (failed #f)]
        [else (void)]))
    (when (not (failed 'get))
      (printf "~a/~a positive tests - ~a claims remain~n"
              positive-count test-count (length claims))
      (define result (synth 'disprove claims))
      (cond
        [(vector? result) (place-channel-put chan `(input ,result ()))]
        [(false? result) (failed #t)]))))

; Run silently until either another strategy vote-quits, or 100 positive tests
; or 1000 negative tests go by without removing any "fixed" claims.  Once
; triggered, broadcast all "fixed" claims to other strategies, vote-quit, and
; exit.
(define (strategy-boredom threshold symbolic-fm chan)
  (printf "strategy-boredom running~n")
  (define claims (all-fixed-claims symbolic-fm))
  (define prev-count (length claims))
  (define counter threshold)
  (for ([msg (in-place-channel chan)])
    (match msg
      [`(test ,inp ,out ,meta)
        (when out
          (set! claims (filter (lambda (c) (eval-claim c inp)) claims))
          (when (< (length claims) prev-count)
            (set! counter threshold)
            (printf "strategy-boredom: ~a claims remain; reset counter~n"
                    (length claims)))
          (set! prev-count (length claims)))
        (set! counter (- counter (if out 10 1)))]
      ['(vote-quit)
       (set! counter 0)]
      [else (void)])

    (when (<= counter 0)
      (printf "strategy-boredom got bored!  broadcasting ~a claims~n"
              (length claims))
      (for ([c claims])
        (displayln c)
        (place-channel-put chan
          `(fix-feature ,(claim-fixed-a c) ,(claim-fixed-val c))))
      ; TODO may need a delay here, so we don't actually cause the manager to
      ; quit before other strategies have a chance to unvote-quit
      (vote-quit chan))
    #:break (<= counter 0)
    (void)))

; TODO strategy-quorum

(define (run-strategy spec chan)
  (match spec
    [(list 'bitflip)
     (strategy-bitflip chan)]
    [(list 'distinguish nf ng nd)
     (strategy-distinguish (?*feature-model nf ng nd) chan)]
    [(list 'disprove nf ng nd)
     (strategy-disprove (?*feature-model nf ng nd) chan)]
    [(list 'boredom threshold nf ng nd)
     (strategy-boredom threshold (?*feature-model nf ng nd) chan)]

    ))
