; Feature-model synthesis demo
#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require toml)
(require racket/random)
(require racket/place)
(require bdd/robdd)
(require json)
(require "synthesis.rkt")
(require "build.rkt")
(require "types.rkt")
(require "eval.rkt")
(require "util.rkt")
(require "manager.rkt")
(require "unsatcore.rkt")
(require "sample.rkt")
(require "fmjson.rkt")
(require "ftree.rkt")
(current-bitwidth #f)


; Demo

(define example-fm-2-raw
  (list
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
    #t
  ))


(define secure-cpu-isa-fm-raw
  (list
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
    #t
  ))

(define secure-cpu-isa-init-tests
  (list
    #(#t #f #f #t #f #f #f #f #f #f #f #f #f #f #f)
    #(#f #t #f #f #f #f #f #f #f #f #f #t #f #f #f)
    #(#f #f #t #f #f #f #f #f #f #f #f #f #f #t #f)
  ))


(define secure-cpu-arch-fm-raw
  (list
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
    #t
  ))

(define secure-cpu-arch-init-tests
  (list
    #(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
    #(#t #f #t #t #t #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
    #(#f #t #f #f #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
    #(#f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
  ))


(define constraint-test-fm-raw
  (list
    (list
      (cons 'a (feature #f #f 0 #f #f))
      (cons 'b (feature #f #f 0 #f #f))
      (cons 'c (feature #f #f 0 #f #f))
    )
    (list)
    (list)
    '(=> (&& a b) c)
  ))

(define constraint-test-init-tests
  (list
    #(#t #t #t)
    #(#t #f #f)
    #(#f #t #f)
    #(#f #f #f)
    #(#f #f #t)
    ))


(random-seed 12345)
;(define oracle-fm-raw example-fm-2-raw)
;(define symbolic-fm (?*feature-model 6 2 1))
;(define init-tests '())
(define oracle-fm-raw secure-cpu-isa-fm-raw)
(define symbolic-fm (?*feature-model 15 4 1))
(define init-tests secure-cpu-isa-init-tests)
;(define oracle-fm-raw secure-cpu-arch-fm-raw)
;(define symbolic-fm (?*feature-model 24 8 3))
;(define init-tests secure-cpu-arch-init-tests)
;(define oracle-fm-raw constraint-test-fm-raw)
;(define symbolic-fm (?*feature-model 3 0 0 #:constraint '(=> (&& _ _) _)))
;(define init-tests constraint-test-init-tests)

(define oracle-fm (apply make-feature-model oracle-fm-raw))
(assert (valid-feature-model oracle-fm))
(define feature-names
  (for/vector ([x (first oracle-fm-raw)]) (symbol->string (car x))))

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

;(define (do-claims)
;  (define synth (oracle-guided-synthesis+ symbolic-fm))
;  (define added-tests (mutable-set))
;  (define claims (all-claims symbolic-fm))
;
;  ; Add test `inp`, but only if it hasn't been previously added.  Returns `#t`
;  ; if `inp` is a new test input and the oracle returns `#t` on it.
;  (define (add-test0 inp)
;    (if (not (set-member? added-tests inp))
;      (begin
;        (set-add! added-tests inp)
;        (define out (oracle inp))
;        (synth 'test (cons inp out))
;        (printf "add test #~a~n" (set-count added-tests))
;        (when out
;          (set! claims (filter (lambda (c) (eval-claim c inp)) claims))
;          (printf "  positive: ~a claims remain~n" (length claims))
;          )
;        out)
;      #f))
;
;  (define (add-test inp)
;    (when (add-test0 inp)
;      (for ([i (in-range (vector-length inp))])
;        (define inp*
;          (for/vector ([(v j) (in-indexed inp)])
;            (if (= j i) (not v) v)))
;        (add-test0 inp*))))
;
;
;
;  (define (loop)
;    (define result (synth 'disprove claims))
;    (when result
;      (add-test result)
;      (loop)))
;    ;(when (not (null? claims))
;    ;  (define result (synth 'disprove claims))
;    ;  (wresult
;    ;    (begin
;    ;      (add-test result)
;    ;      (loop))
;    ;    (begin
;    ;      (printf "proved claims ~a~n" cs)
;    ;      (synth 'assert-claims cs)
;    ;      (set! claims (filter (lambda (c) (not (member c cs))) claims))
;    ;      (loop))
;    ;    )))
;
;
;  (printf "begin with ~a claims (~a features)~n"
;          (length claims) (feature-model-num-features symbolic-fm))
;  (for ([inp init-tests]) (add-test inp))
;  (loop)
;  (printf "claim loop ended after ~a tests: ~a claims remain~n"
;          (set-count added-tests) (length claims))
;  (for ([c claims])
;    (displayln c))
;
;
;  (define test-count (set-count added-tests))
;
;  (define (loop2)
;    (define result (synth 'synthesize))
;    (cond
;      [(vector? result)
;       (add-test result)
;       (loop2)]
;      [(feature-model? result) result]
;      [(false? result) result]))
;
;  (define synth-fm (loop2))
;  (printf "finished after ~a more tests (~a total)~n"
;          (- (set-count added-tests) test-count) (set-count added-tests))
;  (pretty-write synth-fm)
;
;  (pretty-write-to-file (set->list added-tests) "tests-raw.rktd")
;)

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

(define (output-feature-model fm)
  (pretty-write fm)
  (define clafer-str (feature-model->clafer feature-names fm))
  (displayln clafer-str)
  (call-with-output-file*
    "fsdemo.cfr"
    #:exists 'truncate
    (lambda (f) (write-string clafer-str f))))

(define (output-unsat symbolic-fm tests)
  (printf "Minimizing failing input...~n")
  (define min-tests
    (minimize-unsat-core symbolic-fm
      (get-unsat-core symbolic-fm tests)))
  (define vs (minimize-features symbolic-fm min-tests))
  (printf "~n")
  (printf "Relevant features:~n")
  (for ([v vs]) (printf "  ~a~n" (vector-ref feature-names v)))
  (printf "~n")
  (printf "No valid feature model exists for the combination of the following tests:~n")
  (for ([t min-tests])
    (match-define `(,inp ,out ,meta) t)
    (define inp-names (for/list ([i vs]) (vector-ref feature-names i)))
    (printf "  ~a ~a~n" (if out "ok: " "bad:") inp-names)))

(define (read-resume-tests)
  (define resume-tests
    (if (file-exists? "resume-tests.rktd")
      (call-with-default-reading-parameterization
        (lambda () (read-many-from-file "resume-tests.rktd")))
      '()))
  (displayln `(read ,(length resume-tests) resume tests))
  resume-tests)

(define symbolic-fm-args
  (list
    (feature-model-num-features symbolic-fm)
    (feature-model-num-groups symbolic-fm)
    (feature-model-num-dependencies symbolic-fm)
    #t))

(define (make-symbolic-fm)
  (apply ?*feature-model symbolic-fm-args))

(define (do-threaded)
  ;(define resume-tests (read-resume-tests))
  (define resume-tests '())

  (define fm
    (run-manager
      `(
        (bitflip)
        (distinguish ,@symbolic-fm-args)
        (disprove ,@symbolic-fm-args)
        (boredom 1000 ,@symbolic-fm-args)
        )
      `(eval-fm ,(struct->vector* oracle-fm))
      #hash()
      init-tests
      resume-tests
      (open-output-file "test-log.rktd" #:exists 'truncate)
      ))

  (if fm
    (output-feature-model fm)
    (output-unsat (apply ?*feature-model symbolic-fm-args)
                  (read-many-from-file "test-log.rktd")))
  )


(define (do-sample)
  (define var-config
    (for/vector ([i (in-range (feature-model-num-features oracle-fm))])
      (string->symbol (format "v~a" i))))
  (define bexp (feature-model-bexp oracle-fm var-config))
  (define bdd (make-robdd bexp (vector->list var-config)))
  (define sol-count (robdd-sat-count bdd (vector-length var-config)))
  (printf "~a configurations~n" sol-count)
  (define sol-0 (robdd-nth-sat bdd (vector-length var-config) 0))
  (define sol-n (robdd-nth-sat bdd (vector-length var-config) (- sol-count 1)))
  (printf "solution #0: ~a~n" sol-0)
  (printf "solution #~a: ~a~n" (- sol-count 1) sol-n)

  (for ([i (in-range 20)])
    (define idx (random sol-count))
    (define sol (robdd-nth-sat bdd (vector-length var-config) idx))
    (printf "solution #~a: ~a => ~a~n" idx sol (eval-feature-model oracle-fm sol)))
)


(define (do-ftree)
  (define j (call-with-input-file* "test.fm.json" read-json))
  (define ft (fmjson->ftree j))
  (pretty-write ft)

  (ftree-split-opt-groups! ft)
  (ftree-force-card-opt! ft)
  (ftree-complete-order! ft)
  (pretty-write ft)

  (define fm (ftree->feature-model ft))
  (define names (list->vector (ftree-non-group-feature-order ft)))
  (pretty-write fm)

  (define ft2 (feature-model->ftree names fm))
  (pretty-write ft2)

  (define j2 (ftree->fmjson ft))
  (pretty-write j2)
  )


;(do-ftree)
;(do-sample)
;(do-claims2)
(do-threaded)
;(do-claims)
;(do-synthesize2)
;(do-minimize)
;(do-synthesize-from-tests)
