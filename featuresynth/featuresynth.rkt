#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(require toml)
(require "synthesis.rkt")
(current-bitwidth #f)


(define config-path
  (let ([args (current-command-line-arguments)])
    (if (< 0 (vector-length args))
      (vector-ref args 0)
      "featuresynth.toml")))

(define config-list-features-command "false")
(define config-oracle-command "false")
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

(define (run-oracle-command cmd feature-names config)
  (define-values (proc p-out p-in p-err)
    (subprocess #f #f (current-error-port) "/bin/sh" "-c" cmd))
  (close-input-port p-out)
  (for ([name feature-names]
        [val config])
    (fprintf p-in "~a ~a~n" (if val 1 0) name))
  (close-output-port p-in)
  (subprocess-wait proc)
  (= 0 (subprocess-status proc)))

(define (make-command-oracle oracle-command feature-names)
  (lambda (config)
    (run-oracle-command oracle-command feature-names config)))

(define feature-names
  (read-features-from-command config-list-features-command))
(define oracle
  (make-command-oracle config-oracle-command feature-names))


(define symbolic-fm
  (?*feature-model
    (length feature-names)
    config-max-groups
    config-max-dependencies))

(define synth-fm-pair (oracle-guided-synthesis symbolic-fm oracle '()))
(define synth-fm (car synth-fm-pair))
(define synth-tests (cdr synth-fm-pair))
(pretty-write synth-fm)
