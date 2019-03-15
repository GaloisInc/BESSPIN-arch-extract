#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require toml)
(require racket/random)
(require "util.rkt")
(require "synthesis.rkt")
(require "types.rkt")
(require "eval.rkt")
(require "manager.rkt")
(current-bitwidth #f)


(define config-path
  (let ([args (current-command-line-arguments)])
    (if (< 0 (vector-length args))
      (vector-ref args 0)
      "featuresynth.toml")))

(define config-list-features-command "false")
(define config-oracle-command "false")
(define config-oracle-cache-file #f)
(define config-init-tests-file #f)
(define config-max-groups 0)
(define config-max-dependencies 0)

(let
  ([c (hash-ref (parse-toml (file->string config-path)) 'featuresynth hash)])

  (if-let ([x (hash-ref c 'list-features-command #f)])
    (begin
      (assert (string? x))
      (set! config-list-features-command x))
    (void))

  (if-let ([x (hash-ref c 'oracle-command #f)])
    (begin
      (assert (string? x))
      (set! config-oracle-command x))
    (void))

  (if-let ([x (hash-ref c 'oracle-cache-file #f)])
    (begin
      (assert (string? x))
      (set! config-oracle-cache-file x))
    (void))

  (if-let ([x (hash-ref c 'init-tests-file #f)])
    (begin
      (assert (string? x))
      (set! config-init-tests-file x))
    (void))

  (if-let ([x (hash-ref c 'max-groups #f)])
    (begin
      (assert (exact-nonnegative-integer? x))
      (set! config-max-groups x))
    (void))

  (if-let ([x (hash-ref c 'max-dependencies #f)])
    (begin
      (assert (exact-nonnegative-integer? x))
      (set! config-max-dependencies x))
    (void))
)


(define (read-features-from-port p)
  (for/list ([l (in-lines p)]
             #:when (non-empty-string? (string-trim l)))
    (string->symbol (string-trim l))))

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
  (read-features-from-command config-list-features-command))
(define init-tests
  (if config-init-tests-file
    (for/list ([l (file->lines config-init-tests-file)])
      (read (open-input-string l)))
    '()))

(define (synthesize)
  (define symbolic-fm-args
    (list
      (length feature-names)
      config-max-groups
      config-max-dependencies))
  (run-manager
    `(
      (bitflip)
      (distinguish ,@symbolic-fm-args)
      (disprove ,@symbolic-fm-args)
      )
    `(multi-cached
       6
       ,config-oracle-cache-file
       (command
         ,config-oracle-command
         ,feature-names))
    init-tests
    )
  )

(random-seed 12345)
(synthesize)
