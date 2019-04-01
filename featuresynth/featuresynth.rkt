#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require racket/random)
(require "util.rkt")
(require "synthesis.rkt")
(require "types.rkt")
(require "eval.rkt")
(require "manager.rkt")
(require "clafer.rkt")
(require "config.rkt")
(current-bitwidth #f)


(define config-path
  (let ([args (current-command-line-arguments)])
    (if (< 0 (vector-length args))
      (vector-ref args 0)
      "featuresynth.toml")))

(read-config-file config-path)


(define (read-features-from-port p)
  (for/vector ([l (in-lines p)]
               #:when (non-empty-string? (string-trim l)))
    (string-trim l)))

(define (read-features-from-command cmd)
  (define-values (proc p-out p-in p-err)
    (subprocess #f #f (current-error-port) "/bin/sh" "-c" cmd))
  (close-output-port p-in)
  ; p-out is closed by read-features-from-port
  (define result (read-features-from-port p-out))
  (subprocess-wait proc)
  (if (= 0 (subprocess-status proc))
    result
    #f))

(define feature-names
  (read-features-from-command
    (expand-command config-list-features-command
                    `#hash( (config-path . ,config-path) ))))

(define feature-map
  (for/hash ([(f i) (in-indexed feature-names)])
    (values (string->symbol f) i)))

(define (parse-test feature-syms)
  feature-syms
  (define feature-idxs
    (for/set ([f feature-syms])
      (hash-ref feature-map f
                (lambda () (error "unknown feature in input:" f)))))
  (build-vector (vector-length feature-names)
                (lambda (i) (set-member? feature-idxs i))))

(define (read-tests port)
  (define (loop)
    (define inp (read port))
    (if (eof-object? inp)
      '()
      (cons (parse-test inp) (loop))))
  (loop))

(define init-tests
  (if config-init-tests-file
    (call-with-default-reading-parameterization
      (lambda ()
        (call-with-input-file* config-init-tests-file read-tests)))
    '()))

(define (read-resume-tests port)
  (define (loop)
    (define inp (read port))
    (if (eof-object? inp)
      '()
      (cons inp (loop))))
  (loop))

(define resume-tests
  (if (and config-resume-tests-file (file-exists? config-resume-tests-file))
    (call-with-default-reading-parameterization
      (lambda ()
        (call-with-input-file* "resume-tests.rktd" read-resume-tests)))
    '()))

(define (synthesize)
  (define symbolic-fm-args
    (list
      (vector-length feature-names)
      config-max-groups
      config-max-dependencies))
  (run-manager
    `(
      (bitflip)
      (distinguish ,@symbolic-fm-args)
      (disprove ,@symbolic-fm-args)
      (boredom ,config-boredom-threshold ,@symbolic-fm-args)
      )
    `(multi-cached
       ,config-oracle-threads
       ,config-oracle-cache-file
       (command
         ,config-oracle-command
         ,feature-names))
    `#hash( (config-path . ,config-path) )
    init-tests
    resume-tests
    (open-output-file "test-log.rktd" #:exists 'truncate)
    )
  )

(random-seed 12345)
(define fm (synthesize))
(pretty-write fm)
(define clafer-str (clafer->string (feature-model->clafer feature-names fm)))
(displayln clafer-str)
(when config-out-file
  (call-with-output-file*
    config-out-file
    #:exists 'truncate
    (lambda (f) (write-string clafer-str f))))
