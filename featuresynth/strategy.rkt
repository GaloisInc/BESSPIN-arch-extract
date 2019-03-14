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


(define (strategy-bitflip chan)
  (printf "strategy-bitflip running~n")
  (for ([t (in-place-channel chan)])
    (match-define `(,inp ,out ,meta) t)
    (when (and out (not (assoc 'bitflip-depth meta)))
      (for ([i (in-range (vector-length inp))])
        (define inp*
          (for/vector ([(v j) (in-indexed inp)])
            (if (= i j) (not v) v)))
        (place-channel-put chan `(,inp* ((bitflip-depth . 1))))))))

(define (strategy-distinguish symbolic-fm chan)
  (printf "strategy-distinguish running~n")
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (for ([ts (in-place-channel-chunks chan)])
    (for ([t ts])
      (match-define `(,inp ,out ,meta) t)
      (synth 'test `(,inp . ,out)))
    (define result (synth 'synthesize))
    (place-channel-put chan `(,(struct->vector* result) ()))))

(define (strategy-disprove symbolic-fm chan)
  (printf "strategy-disprove running~n")
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define claims (all-claims symbolic-fm))
  (printf "begin with ~a claims~n" (length claims))
  (for ([ts (in-place-channel-chunks chan)])
    (define positive-count 0)
    (for ([t ts])
      (match-define `(,inp ,out ,meta) t)
      (when out
        (set! claims (filter (lambda (c) (eval-claim c inp)) claims))
        (set! positive-count (add1 positive-count)))
      (synth 'test `(,inp . ,out)))
    (printf "~a/~a positive tests - ~a claims remain~n"
            positive-count (length ts) (length claims))
    (define result (synth 'disprove claims))
    #:break (not result)
    (place-channel-put chan `(,result ()))))

; TODO strategy-quorum

(define (run-strategy spec chan)
  (match spec
    [(list 'bitflip)
     (strategy-bitflip chan)]
    [(list 'distinguish nf ng nd)
     (strategy-distinguish (?*feature-model nf ng nd) chan)]
    [(list 'disprove nf ng nd)
     (strategy-disprove (?*feature-model nf ng nd) chan)]

    ))
