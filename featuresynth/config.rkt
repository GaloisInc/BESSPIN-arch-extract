#lang rosette
(provide
  config-list-features-command
  config-oracle-command
  config-oracle-cache-file
  config-init-tests-file
  config-max-groups
  config-max-dependencies
  config-out-file
  read-config-file
  expand-command
)
(require "util.rkt")
(require toml)

(define config-list-features-command "false")
(define config-oracle-command "false")
(define config-oracle-cache-file #f)
(define config-init-tests-file #f)
(define config-max-groups 0)
(define config-max-dependencies 0)
(define config-out-file #f)

(define (read-config-file path)
  (define c
    (hash-ref (parse-toml (file->string path)) 'featuresynth hash))

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

  (if-let ([x (hash-ref c 'out-file #f)])
    (begin
      (assert (string? x))
      (set! config-out-file x))
    (void))
)

(define (expand-command cmd meta)
  (define (repl s c)
    (case c
      [("c") (hash-ref meta 'config-path)]
      [("i")
       (define parts (cons "inst" (map number->string
                                       (hash-ref meta 'instance '()))))
       (string-join parts "_")]
      [("%") "%"]
      [else (error "unknown placeholder in command:" s)]))
  (regexp-replace* #rx"%(.)" cmd repl))