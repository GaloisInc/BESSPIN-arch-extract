; Feature-model synthesis demo
#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require toml)
(require racket/random)
(require racket/place)
(require "synthesis.rkt")
(require "build.rkt")
(require "types.rkt")
(require "eval.rkt")
(require "util.rkt")
(require "manager.rkt")
(require "clafer.rkt")
(current-bitwidth #f)


; Demo

(define example-fm-2
  (make-feature-model
    (list
      (cons 'a1 (feature #f #f 0 #f #f))
      (cons 'a2 (feature #f #f 0 #f #f))
      (cons 'b1 (feature 'a1 'gb 1 #f #f))
      (cons 'b2 (feature 'a1 'gb 1 #f #f))
      (cons 'c1 (feature 'a2 'gc 1 #f #f))
      (cons 'c2 (feature 'a2 'gc 1 #f #f))
    )
    (list
      (cons 'gb (group 'a1 1 1))
      (cons 'gc (group 'a2 1 1))
    )
    (list
      (dependency 'b1 'c2 #t)
    )
  ))


(define secure-cpu-isa-fm
  (make-feature-model
    (list
      (cons 'riscv  (feature #f 'g-isa 0 #f #f))
      (cons 'intel  (feature #f 'g-isa 0 #f #f))
      (cons 'arm    (feature #f 'g-isa 0 #f #f))
      (cons 'rv32   (feature 'riscv 'g-riscv-width 1 #f #f))
      (cons 'rv64   (feature 'riscv 'g-riscv-width 1 #f #f))
      (cons 'rv128  (feature 'riscv 'g-riscv-width 1 #f #f))
      (cons 'rv-m   (feature 'riscv #f 1 #f #f))
      (cons 'rv-a   (feature 'riscv #f 1 #f #f))
      (cons 'rv-d   (feature 'riscv #f 1 #f #f))
      (cons 'rv-c   (feature 'riscv #f 1 #f #f))
      (cons 'rv-f   (feature 'riscv #f 1 #f #f))
      (cons 'ia32   (feature 'intel 'g-intel-arch 1 #f #f))
      (cons 'x86-64 (feature 'intel 'g-intel-arch 1 #f #f))
      (cons 'aarch32 (feature 'arm 'g-arm-arch 1 #f #f))
      (cons 'aarch64 (feature 'arm 'g-arm-arch 1 #f #f))
    )
    (list
      (cons 'g-isa (group #f 1 1))
      (cons 'g-riscv-width (group 'riscv 1 1))
      (cons 'g-intel-arch (group 'intel 1 1))
      (cons 'g-arm-arch (group 'arm 1 1))
    )
    (list
      (dependency 'rv-d 'rv-f #t)
    )
  ))
(assert (valid-feature-model secure-cpu-isa-fm))

(define secure-cpu-isa-init-tests
  (list
    #(#t #f #f #t #f #f #f #f #f #f #f #f #f #f #f)
    #(#f #t #f #f #f #f #f #f #f #f #f #t #f #f #f)
    #(#f #f #t #f #f #f #f #f #f #f #f #f #f #t #f)
  ))


(define secure-cpu-arch-fm
  (make-feature-model
    (list
      (cons 'tagging    (feature #f 'g-arch 0 #f #f))
      (cons 'crypto     (feature #f 'g-arch 0 #f #f))
      (cons 'dram       (feature 'tagging 'g-tagging 1 #f #f))
      (cons 'registers  (feature 'tagging 'g-tagging 1 #f #f))
      (cons 'icache     (feature 'tagging 'g-tagging 1 #f #f))
      (cons 'dcache     (feature 'tagging 'g-tagging 1 #f #f))
      (cons 'randomness (feature 'crypto #f 1 #f #f))
      (cons 'hash       (feature 'crypto #f 1 #f #f))
      (cons 'symmetric  (feature 'crypto #f 1 #f #f))
      (cons 'asymmetric (feature 'crypto #f 1 #f #f))
      (cons 'prng       (feature 'randomness 'g-randomness 2 #f #f))
      (cons 'true-rng   (feature 'randomness 'g-randomness 2 #f #f))
      (cons 'hash-drbg  (feature 'prng 'g-prng 3 #f #f))
      (cons 'hmac-drbg  (feature 'prng 'g-prng 3 #f #f))
      (cons 'ctr-drbg   (feature 'prng 'g-prng 3 #f #f))
      (cons 'sha2       (feature 'hash 'g-hash 2 #f #f))
      (cons 'sha3       (feature 'hash 'g-hash 2 #f #f))
      (cons 'md5        (feature 'hash 'g-hash 2 #f #f))
      (cons 'aes        (feature 'symmetric 'g-symmetric 2 #f #f))
      (cons '3des       (feature 'symmetric 'g-symmetric 2 #f #f))
      (cons 'simon      (feature 'symmetric 'g-symmetric 2 #f #f))
      (cons 'speck      (feature 'symmetric 'g-symmetric 2 #f #f))
      (cons 'rsa        (feature 'asymmetric 'g-asymmetric 2 #f #f))
      (cons 'elgamal    (feature 'asymmetric 'g-asymmetric 2 #f #f))
    )
    (list
      (cons 'g-arch (group #f 1 2))
      (cons 'g-tagging (group 'tagging 1 4))
      (cons 'g-randomness (group 'randomness 1 2))
      (cons 'g-prng (group 'prng 1 3))
      (cons 'g-hash (group 'hash 1 3))
      (cons 'g-symmetric (group 'symmetric 1 4))
      (cons 'g-asymmetric (group 'asymmetric 1 2))
    )
    (list
      (dependency 'hash-drbg 'sha2 #t)
      (dependency 'hmac-drbg 'hash #t)
      (dependency 'ctr-drbg 'symmetric #t)
    )
  ))
(assert (valid-feature-model secure-cpu-arch-fm))

(define secure-cpu-arch-init-tests
  (list
    #(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
    #(#t #f #t #t #t #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
    #(#f #t #f #f #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
    #(#f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
  ))


(random-seed 12345)
;(define oracle-fm example-fm-2)
;(define symbolic-fm (?*feature-model 6 2 1))
;(define init-tests '())
(define oracle-fm secure-cpu-isa-fm)
(define symbolic-fm (?*feature-model 15 4 1))
(define init-tests secure-cpu-isa-init-tests)
;(define oracle-fm secure-cpu-arch-fm)
;(define symbolic-fm (?*feature-model 24 8 3))
;(define init-tests secure-cpu-arch-init-tests)

(define (oracle inp) (eval-feature-model oracle-fm inp))


(define (do-synthesize)
  (define synth-fm-pair (oracle-guided-synthesis symbolic-fm oracle '()))

  (define synth-fm (car synth-fm-pair))
  (pretty-write (list "synthesis result" synth-fm))
  (pretty-write-to-file synth-fm "fm.rktd")

  (define synth-tests (cdr synth-fm-pair))
  (pretty-write-to-file synth-tests "tests-raw.rktd")
  (displayln (list "wrote" (length synth-tests) "tests to file"))
)

(define (do-synthesize2)
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define added-tests (mutable-set))

  (define (add-test inp)
    (when (not (set-member? added-tests inp))
      (set-add! added-tests inp)
      (define out (oracle inp))
      (synth 'test (cons inp out))))

  (define (add-test* inp)
    (when (not (set-member? added-tests inp))
      (set-add! added-tests inp)
      (define out (oracle inp))
      (synth 'test (cons inp out))

      (when out
        (printf "found positive test!~n")
        (for ([i (in-range (vector-length inp))])
          (define inp*
            (for/vector ([(v j) (in-indexed inp)])
              (if (= j i) (not v) v)))
          (add-test inp*)))))

  (define (loop)
    (define result (synth 'synthesize))
    (cond
      [(vector? result)
       (add-test* result)
       (loop)]
      [(feature-model? result) result]
      [(false? result) result]))

  (for ([inp init-tests]) (add-test* inp))

  (define synth-fm (loop))
  (pretty-write (list "synthesis result" synth-fm))
  (pretty-write-to-file synth-fm "fm.rktd")

  (define synth-tests (synth 'get-tests))
  (pretty-write-to-file synth-tests "tests-raw.rktd")
  (displayln (list "wrote" (length synth-tests) "tests to file"))
)

(define (do-claims)
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (define added-tests (mutable-set))
  (define claims (all-claims symbolic-fm))

  ; Add test `inp`, but only if it hasn't been previously added.  Returns `#t`
  ; if `inp` is a new test input and the oracle returns `#t` on it.
  (define (add-test0 inp)
    (if (not (set-member? added-tests inp))
      (begin
        (set-add! added-tests inp)
        (define out (oracle inp))
        (synth 'test (cons inp out))
        (printf "add test #~a~n" (set-count added-tests))
        (when out
          (set! claims (filter (lambda (c) (eval-claim c inp)) claims))
          (printf "  positive: ~a claims remain~n" (length claims))
          )
        out)
      #f))

  (define (add-test inp)
    (when (add-test0 inp)
      (for ([i (in-range (vector-length inp))])
        (define inp*
          (for/vector ([(v j) (in-indexed inp)])
            (if (= j i) (not v) v)))
        (add-test0 inp*))))



  (define (loop)
    (define result (synth 'disprove claims))
    (when result
      (add-test result)
      (loop)))
    ;(when (not (null? claims))
    ;  (define result (synth 'disprove claims))
    ;  (wresult
    ;    (begin
    ;      (add-test result)
    ;      (loop))
    ;    (begin
    ;      (printf "proved claims ~a~n" cs)
    ;      (synth 'assert-claims cs)
    ;      (set! claims (filter (lambda (c) (not (member c cs))) claims))
    ;      (loop))
    ;    )))


  (printf "begin with ~a claims (~a features)~n"
          (length claims) (feature-model-num-features symbolic-fm))
  (for ([inp init-tests]) (add-test inp))
  (loop)
  (printf "claim loop ended after ~a tests: ~a claims remain~n"
          (set-count added-tests) (length claims))
  (for ([c claims])
    (displayln c))


  (define test-count (set-count added-tests))

  (define (loop2)
    (define result (synth 'synthesize))
    (cond
      [(vector? result)
       (add-test result)
       (loop2)]
      [(feature-model? result) result]
      [(false? result) result]))

  (define synth-fm (loop2))
  (printf "finished after ~a more tests (~a total)~n"
          (- (set-count added-tests) test-count) (set-count added-tests))
  (pretty-write synth-fm)

  (pretty-write-to-file (set->list added-tests) "tests-raw.rktd")
)

(define (do-synthesize-from-tests)
  (define tests-orig (read-from-file "tests.rktd"))
  (define tests
    (for/list ([t tests-orig])
      (cons (car t) (oracle (car t)))))
  (define synth-fm-pair (oracle-guided-synthesis symbolic-fm oracle tests))

  (define synth-fm (car synth-fm-pair))
  (pretty-write (list "synthesis result" synth-fm))
  (pretty-write-to-file synth-fm "fm.rktd")

  (define synth-tests (cdr synth-fm-pair))
  (pretty-write-to-file synth-tests "tests-raw.rktd")
  (displayln (list "wrote" (length synth-tests) "tests to file"))
)

(define (do-minimize)
  (define synth-fm (vector->feature-model (read-from-file "fm.rktd")))
  (define synth-tests (read-from-file "tests-raw.rktd"))

  (displayln "minimizing tests...")
  (define min-tests (minimize-tests symbolic-fm synth-fm synth-tests))
  (pretty-write-to-file min-tests "tests.rktd")
  (displayln (list "reduced from" (length synth-tests) "to" (length min-tests)))
)

(define (do-threaded)
  (define symbolic-fm-args
    (list
      (feature-model-num-features symbolic-fm)
      (feature-model-num-groups symbolic-fm)
      (feature-model-num-dependencies symbolic-fm)))
  (define fm
    (run-manager
      `(
        (bitflip)
        (distinguish ,@symbolic-fm-args)
        (disprove ,@symbolic-fm-args)
        ;(boredom 100 ,@symbolic-fm-args)
        )
      `(eval-fm ,(struct->vector* oracle-fm))
      init-tests
      ))
  (pretty-write fm))

(do-threaded)
;(do-claims)
;(do-synthesize2)
;(do-minimize)
;(do-synthesize-from-tests)
