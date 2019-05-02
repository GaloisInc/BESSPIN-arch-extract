#lang rosette
(provide
  config-list-features-command
  config-oracle-command
  config-oracle-cache-file
  config-oracle-threads
  config-init-tests-file
  config-resume-tests-file
  config-max-groups
  config-max-dependencies
  config-boredom-threshold
  config-reason-threshold
  config-hard-constraints
  config-out-file
  read-config-file
  expand-command
)
(require "util.rkt")
(require toml)

(define config-list-features-command "false")
(define config-oracle-command "false")
(define config-oracle-cache-file #f)
(define config-oracle-threads 4)
; TODO: names are a bit misleading - init-tests-file contains test *inputs*,
; while resume-tests-file contains entire tests (`(input output meta)` lists,
; like in test-log.rktd)
(define config-init-tests-file #f)
(define config-resume-tests-file #f)
(define config-max-groups 0)
(define config-max-dependencies 0)
(define config-boredom-threshold 1000)
(define config-reason-threshold 15)
(define config-hard-constraints '())
(define config-out-file #f)

(define (get-typed c k t)
  (match (hash-ref c k #f)
    [#f #f]
    [(? t x) x]
    [x (raise (format "expected config value for ~a to satisfy ~a, but it was ~a~n"
                      k t x))]))

(define (get-constraint-list c k)
  (define raw (get-typed c k list?))
  (if (not raw) #f
    (call-with-default-reading-parameterization
      (lambda ()
        (for/list ([s raw])
          (read (open-input-string s)))))))

(define (read-config-file path)
  (define c
    (hash-ref (parse-toml (file->string path)) 'featuresynth hash))

  (when-let ([x (get-typed c 'list-features-command string?)])
    (set! config-list-features-command x))

  (when-let ([x (get-typed c 'oracle-command string?)])
    (set! config-oracle-command x))

  (when-let ([x (get-typed c 'oracle-cache-file string?)])
    (set! config-oracle-cache-file x))

  (when-let ([x (get-typed c 'oracle-threads exact-positive-integer?)])
    (set! config-oracle-threads x))

  (when-let ([x (get-typed c 'init-tests-file string?)])
    (set! config-init-tests-file x))

  (when-let ([x (get-typed c 'resume-tests-file string?)])
    (set! config-resume-tests-file x))

  (when-let ([x (get-typed c 'max-groups exact-nonnegative-integer?)])
    (set! config-max-groups x))

  (when-let ([x (get-typed c 'max-dependencies exact-nonnegative-integer?)])
    (set! config-max-dependencies x))

  (when-let ([x (get-typed c 'boredom-threshold exact-nonnegative-integer?)])
    (set! config-boredom-threshold x))

  (when-let ([x (get-typed c 'reason-threshold exact-nonnegative-integer?)])
    (set! config-reason-threshold x))

  (when-let ([x (get-constraint-list c 'hard-constraints)])
    (set! config-hard-constraints x))

  (when-let ([x (get-typed c 'out-file string?)])
    (set! config-out-file x))
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
