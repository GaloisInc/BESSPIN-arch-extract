; Feature-model synthesis demo
#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require toml)
(require "synthesis.rkt")
(require "build.rkt")
(require "types.rkt")
(require "eval.rkt")
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


(define (pretty-write-to-file v path)
  (call-with-output-file* path
    (lambda (f) (pretty-write v f))
    #:exists 'truncate))

(define (read-from-file path)
  (call-with-input-file* path
    (lambda (f) (read f))))

(random-seed 12345)
(define symbolic-fm (?*feature-model 15 4 1))
(define (oracle inp) (eval-feature-model secure-cpu-isa-fm inp))
(define init-tests secure-cpu-isa-init-tests)
;(define symbolic-fm (?*feature-model 24 8 3))
;(define (oracle inp) (eval-feature-model secure-cpu-arch-fm inp))
;(define init-tests secure-cpu-arch-init-tests)
;(define symbolic-fm (?*feature-model 6 2 1))
;(define (oracle inp) (eval-feature-model example-fm-2 inp))


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

(do-synthesize2)
;(do-minimize)
;(do-synthesize-from-tests)
