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

(define (make-cached-oracle cache-file oracle)
  (define cache-entries
    (if (file-exists? cache-file)
      (for/list ([l (file->lines cache-file)]) (read (open-input-string l)))
      '()))
  (define cache (make-hash cache-entries))
  (define out-file (open-output-file cache-file #:exists 'append))
  (lambda (config)
    (if (hash-has-key? cache config)
      (hash-ref cache config)
      (let ([result (oracle config)])
        (writeln (cons config result) out-file)
        (flush-output out-file)
        (hash-set! cache config result)
        result))))

(define feature-names
  (read-features-from-command config-list-features-command))
(define oracle
  (let ([cmd-oracle (make-command-oracle config-oracle-command feature-names)])
    (if config-oracle-cache-file
      (make-cached-oracle config-oracle-cache-file cmd-oracle)
      cmd-oracle)))
(define init-tests
  (if config-init-tests-file
    (for/list ([l (file->lines config-init-tests-file)])
      (read (open-input-string l)))
    '()))

(define symbolic-fm
  (?*feature-model
    (length feature-names)
    config-max-groups
    config-max-dependencies))


(define (synthesize)
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
    ;  (define cs (random-sample claims (min 5 (length claims))
    ;                            #:replacement? #f))
    ;  (define result (synth 'disprove cs))
    ;  (if result
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

  (pretty-write-to-file (set->list added-tests) "tests-raw-fs.rktd")
  )

(random-seed 12345)
(synthesize)
