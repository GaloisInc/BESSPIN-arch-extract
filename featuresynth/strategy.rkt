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


(define (fail-flag chan [quiet #f])
  (define val #f)
  (lambda (x)
    (match x
      ['get val]
      ['set
       (place-channel-put chan '(property failed #t))
       (set! val #t)]
      ['reset (set! val #f)])))


(define (strategy-bitflip chan)
  (printf "strategy-bitflip running~n")
  (place-channel-put chan `(property reactive #t))
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
  (define started #f)
  (define failed (fail-flag chan))
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define num-tests 0)
  (define num-positive 0)
  (for ([msgs (in-place-channel-chunks chan)])
    (for ([msg msgs])
      (match msg
        [`(start)
          (set! started #t)
          (failed 'reset)]
        [`(test ,inp ,out ,meta)
          (set! num-tests (add1 num-tests))
          (when out (set! num-positive (add1 num-positive)))
          (synth 'test `(,inp . ,out))]
        [`(fix-feature ,idx ,val)
          (synth 'fix-feature idx val)]
        [else (void)]))

    (when (and started (not (failed 'get)))
      (printf "distinguish: synthesizing from ~a tests (~a positive)~n"
              num-tests num-positive)
      (define result (synth 'synthesize))
      (cond
        [(feature-model? result)
         (place-channel-put chan `(solution ,(struct->vector* result)))]
        [(vector? result)
         (place-channel-put chan `(input ,result ()))]
        [(false? result)
         (failed 'set)]))))

(define (strategy-disprove symbolic-fm chan)
  (printf "strategy-disprove running~n")
  (define started #f)
  (define failed (fail-flag chan))
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define cset (make-claim-set (all-claims symbolic-fm)
                               (feature-model-num-features symbolic-fm)))
  (printf "disprove: begin with ~a claims~n" (claim-set-count cset))
  (define num-claims (claim-set-count cset))
  (for ([msgs (in-place-channel-chunks chan)])
    (define test-count 0)
    (define positive-count 0)
    (for ([msg msgs])
      (match msg
        [`(start)
          (set! started #t)
          (failed 'reset)]
        [`(test ,inp ,out ,meta)
          (synth 'test `(,inp . ,out))
          (claim-set-update cset inp out)
          (set! test-count (+ 1 test-count))
          (when out (set! positive-count (+ 1 positive-count)))]
        [`(fix-feature ,idx ,val)
          (claim-set-remove-feature! cset idx)
          (synth 'fix-feature idx val)]
        [else (void)]))
    (when (and started (not (failed 'get)))
      (when (not (= num-claims (claim-set-count cset)))
        (set! num-claims (claim-set-count cset))
        (printf "disprove: ~a claims remain~n" num-claims))
      (define result (synth 'disprove cset))
      (cond
        [(vector? result) (place-channel-put chan `(input ,result ()))]
        [(false? result) (failed 'set)]))))

; Simple strategy for identifying features that have fixed values (always #t /
; always #f).  This strategy keeps track of the set of remaining `claim-fixed`
; claims as claims are disproved by new passing tests.  When the number of
; `claim-fixed` claims hasn't decreased in a while (controlled by `threshold`),
; we assume the remaining claims are all true, and assert them for all threads.
(define (strategy-boredom threshold symbolic-fm chan)
  (printf "strategy-boredom running~n")
  (place-channel-put chan `(property reactive #t))
  (place-channel-put chan `(property has-recovery #t))
  (define cset (make-claim-set (all-fixed-claims symbolic-fm)
                               (feature-model-num-features symbolic-fm)))
  (define prev-count (claim-set-count cset))
  (define counter threshold)
  (define done #f)

  (define (fire)
    (printf "strategy-boredom got bored!  broadcasting ~a claims~n"
            (claim-set-count cset))
    (for ([c (claim-set-claims cset)])
      (displayln c)
      (place-channel-put chan
        `(fix-feature ,(claim-fixed-a c) ,(claim-fixed-val c))))
    (place-channel-put chan `(property has-recovery #f)))

  (for ([msg (in-place-channel chan)])
    (match msg
      [`(test ,inp ,out ,meta)
        (claim-set-update cset inp out)
        (when out
          (when (< (claim-set-count cset) prev-count)
            (set! counter threshold)
            (printf "boredom: ~a claims remain; reset counter~n"
                    (claim-set-count cset)))
          (set! prev-count (claim-set-count cset)))
        (set! counter (- counter (if out 10 1)))]
      [`(fix-feature ,idx ,_)
        (claim-set-remove-feature! cset idx)]
      [`(recover)
        (fire)
        (set! done #t)
        (place-channel-put chan '(recovery-done))]
      [else (void)])

    (when (<= counter 0)
      (fire)
      (set! done #t))

    #:break done
    (void)))

; Find the one feature whose setting caused the test to fail.  This looks for a
; feature for which `(claim-fixed f (not (vector-ref cfg f)))` is present, and
; all other claims would pass if `f` was flipped.
(define (lone-fixed-failure-reason cset cfg [lenient #f])
  (for/or ([i (in-range (vector-length cfg))])
    (define val (vector-ref cfg i))
    (define flipped-cfg (vector-copy cfg))
    (vector-set! flipped-cfg i (not val))
    (and
      (or lenient (set-member? (claim-set-claims cset) (claim-fixed i (not val))))
      (for/and ([c (claim-set-claims cset)])
        (eq? #t (claim-set-eval-claim-precise cset c flipped-cfg)))
      ; When the two above cases are #t, we return `i`.  Otherwise, #f.
      i)))

; Slightly smarter strategy for identifying features that have fixed values
; (always #t / always #f).  This strategy keeps track of the set of remaining
; claims as they are disproved by passing tests.  Then, for each failing test,
; it tries to identify a single feature whose value is opposite of a remaining
; `claim-fixed` claim and which is the "sole reason" why the test failed.  If
; the same feature shows up enough times as the sole reason for failure, then
; we assert its `claim-fixed`.
(define (strategy-reason threshold symbolic-fm chan)
  (printf "strategy-reason running~n")
  (place-channel-put chan `(property reactive #t))
  (define cset (make-claim-set (all-claims symbolic-fm)
                               (feature-model-num-features symbolic-fm)))
  (define counters (make-hash))
  (define done #f)

  ; "Test failed because feature `i` had value `val`"
  (define (failed-because i val)
    (define cur (hash-ref counters i 0))
    (define next (+ 1 cur))
    (printf "reason: test failed because ~a had value ~a (~a times)~n" i val next)
    (hash-set! counters i next)
    (when (>= next threshold)
      (printf "reason: BROADCAST: fix ~a as ~a~n" i (not val))
      (claim-set-remove-feature! cset i)
      (place-channel-put chan `(fix-feature ,i ,(not val)))))

  (for ([msg (in-place-channel chan)])
    (match msg
      [`(test ,inp ,out ,meta)
        (claim-set-update cset inp out)
        (when (not out)
          (when-let ([idx (lone-fixed-failure-reason cset inp)])
            (failed-because idx (vector-ref inp idx))))]
      [`(fix-feature ,idx ,_)
        (claim-set-remove-feature! cset idx)]
      [else (void)])))

; Try to identify features that are fixed: the constraints and tests so far
; prove that they are always #t or always #f.  Upon detecting a fixed feature,
; this strategy immediately broadcasts a `fix-feature` message.
;
; This strategy doesn't tell the solver anything it can't already prove, but
; does help it to produce nicer-looking feature models.  A feature that's
; forced off by a constraint can appear anywhere in the feature model, whereas
; a feature with `feature-force-off` set must appear at top level.
(define (strategy-find-fixed start-tests step-tests symbolic-fm chan)
  (printf "strategy-find-fixed running~n")
  (define started #f)
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define seen-enabled (mutable-set))
  (define seen-disabled (mutable-set))
  ; Features whose status is known (forced-on, forced-off, varying)
  (define known-features (mutable-set))
  (define need-tests start-tests)
  (place-channel-put chan `(property reactive #t))

  (define (try-prove idx val)
    (when (synth 'prove-fixed idx val)
      (printf "find-fixed: proved ~a = ~a~n" idx val)
      (place-channel-put chan `(fix-feature ,idx ,val))
      (synth 'fix-feature idx val)
      (set-add! known-features idx)))

  (for ([msgs (in-place-channel-chunks chan)])
    (for ([msg msgs])
      (match msg
        [`(start)
          (set! started #t)]
        [`(test ,inp ,out ,meta)
          (synth 'test `(,inp . ,out))
          (set! need-tests (- need-tests 1))
          (when out
            (for ([(v i) (in-indexed inp)])
              (when v (set-add! seen-enabled i))
              (unless v (set-add! seen-disabled i))))
          ]
        [`(fix-feature ,idx ,val)
          (synth 'fix-feature idx val)
          (set-add! known-features idx)]
        [else (void)]))

    (when (and started (<= need-tests 0))
      (printf "find-fixed: checking ~a features with ~a tests~n"
              (- (feature-model-num-features symbolic-fm)
                 (set-count known-features))
              (length (synth 'get-tests)))
      (for ([i (in-range (feature-model-num-features symbolic-fm))]
            #:when (not (set-member? known-features i)))
        (printf "find-fixed:   ~a~n" i)

        ; If we've never seen it disabled, then maybe it's forced on.
        (when (not (set-member? seen-disabled i))
          (try-prove i #t))
        (when (not (set-member? seen-enabled i))
          (try-prove i #f))

        ; If we've seen it both enabled and disabled, then we don't need to
        ; check it any more.
        (when (and (set-member? seen-enabled i) (set-member? seen-disabled i))
          (set-add! known-features i))
      )
      (printf "find-fixed: ~a features still unknown~n"
              (- (feature-model-num-features symbolic-fm)
                 (set-count known-features)))
      (set! need-tests step-tests))

    #:break (= (feature-model-num-features symbolic-fm)
               (set-count known-features))
    (void)
  )

  (printf "find-fixed: no more unknown features - terminating~n")
)

; TODO strategy-quorum

(define (run-strategy spec chan)
  (match spec
    [(list 'bitflip)
     (strategy-bitflip chan)]
    [(list 'distinguish nf ng nd c)
     (strategy-distinguish (?*feature-model nf ng nd c) chan)]
    [(list 'disprove nf ng nd c)
     (strategy-disprove (?*feature-model nf ng nd c) chan)]
    [(list 'boredom threshold nf ng nd c)
     (strategy-boredom threshold (?*feature-model nf ng nd c) chan)]
    [(list 'reason threshold nf ng nd c)
     (strategy-reason threshold (?*feature-model nf ng nd c) chan)]
    [(list 'find-fixed start step nf ng nd c)
     (strategy-find-fixed start step (?*feature-model nf ng nd c) chan)]
    ))
