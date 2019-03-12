; Feature-model synthesis demo
#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require toml)
(require "synthesis.rkt")
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
      (dependency 'b1 'c2)
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
      (dependency 'rv-d 'rv-f)
    )
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

(do-synthesize)
;(do-minimize)
;(do-synthesize-from-tests)
