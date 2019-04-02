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
(require "build.rkt")
(require "unsatcore.rkt")
(current-bitwidth #f)


(define args (current-command-line-arguments))

(define config-path
  (if (< 0 (vector-length args))
    (vector-ref args 0)
    "featuresynth.toml"))

(define subcommand
  (drop (vector->list args) 1))


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

(define init-tests
  (if config-init-tests-file
    (call-with-default-reading-parameterization
      (lambda () (map parse-test (read-many-from-file config-init-tests-file))))
    '()))

(define resume-tests
  (if (and config-resume-tests-file (file-exists? config-resume-tests-file))
    (call-with-default-reading-parameterization
      (lambda () (read-many-from-file config-resume-tests-file)))
    '()))

(define name-map (feature-name-list->name-map feature-names))

(define hard-constraints
  (for/list ([c config-hard-constraints])
    (resolve-constraint name-map c)))

(define symbolic-fm-args
  (list
    (vector-length feature-names)
    config-max-groups
    config-max-dependencies
    (cons '&& hard-constraints)))

(define (make-symbolic-fm)
  (apply ?*feature-model symbolic-fm-args))

(define (synthesize)
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
         ,feature-names
         (&& ,@hard-constraints)))
    `#hash( (config-path . ,config-path) )
    init-tests
    resume-tests
    (open-output-file "test-log.rktd" #:exists 'truncate)
    )
  )

(define (do-synthesize)
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
  )

(define (do-unsat-core)
  (define symbolic-fm (make-symbolic-fm))
  (when (or (not config-resume-tests-file)
            (not (file-exists?  config-resume-tests-file)))
    (raise "must provide resume-tests-file with tests that produce an UNSAT result"))

  (define synth (oracle-guided-synthesis+ symbolic-fm #:unsat-cores #t))
  (fprintf (current-error-port) "compute unsat core from ~a tests~n" (length resume-tests))
  (define U (synth 'unsat-core resume-tests))
  (fprintf (current-error-port) "unsat core (~a tests):~n" (length U))
  (for ([t U]) (displayln t))
  )

(define (do-minimize-unsat-core)
  (define symbolic-fm (make-symbolic-fm))
  (when (or (not config-resume-tests-file)
            (not (file-exists?  config-resume-tests-file)))
    (raise "must provide resume-tests-file with tests that produce an UNSAT result"))

  (define (check tests)
    (fprintf (current-error-port) "try with ~a tests~n" (length tests))
    (define synth (oracle-guided-synthesis+ symbolic-fm))
    (for ([t tests])
      (match-define `(,inp ,out ,meta) t)
      (synth 'test (cons inp out)))
    (not (synth 'check-sat)))

  (define min-tests (delta-minimize resume-tests check))
  (fprintf (current-error-port) "reduced ~a -> ~a tests~n"
           (length resume-tests) (length min-tests))
  (for ([t min-tests]) (displayln t))
  )

(define (minimal-unsat-core? symbolic-fm tests)
  (for/and ([(unsat-test i) (in-indexed tests)])
    (fprintf (current-error-port) "minimal-unsat-core?: check ~a~n" i)
    (define synth (oracle-guided-synthesis+ symbolic-fm))

    (for/list ([(t j) (in-indexed tests)] #:when (not (= i j)))
      (match-define `(,inp ,out ,meta) t)
      (synth 'test (cons inp out)))
    (define sat1 (synth 'check-sat))

    (match-define `(,inp ,out ,meta) unsat-test)
    (synth 'test (cons inp out))
    (define sat2 (synth 'check-sat))

    (and sat1 (not sat2))))

(define (slice-test vs t)
  (match-define `(,inp ,out ,meta) t)
  (define sliced-inp (for/vector ([i vs]) (vector-ref inp i)))
  `(,sliced-inp ,out ,meta))

(define (make-sliced-symbolic-fm vs)
  (define sliced-feature-names
    (for/vector ([i vs]) (vector-ref feature-names i)))
  (define nm (feature-name-list->name-map sliced-feature-names))
  (define cs
    (for/list ([c config-hard-constraints])
      (resolve-constraint nm c)))
  (?*feature-model
    (length vs)
    config-max-groups
    config-max-dependencies
    (cons '&& cs)))

(define (check-var-slice vs)
  (fprintf (current-error-port) "check-var-slice: ~a~n" vs)
  (define symbolic-fm (make-sliced-symbolic-fm vs))
  (define tests (for/list ([t resume-tests]) (slice-test vs t)))
  (minimal-unsat-core? symbolic-fm tests))

(define (do-slice-tests)
  (define all-vs (for/list ([i (in-range (vector-length feature-names))]) i))
  (define min-vs (delta-minimize all-vs check-var-slice))

  (printf  "sliced ~a variables, leaving only ~a:~n"
          (length all-vs) (length min-vs))
  (for ([i min-vs])
    (printf "~a ~a~n" i (vector-ref feature-names i)))

  (printf "these test results produce a contradiction:~n")
  (for ([t resume-tests])
    (match-define `(,inp ,out ,meta) t)
    (define pretty-inp
      (for/list ([i min-vs] #:when (vector-ref inp i))
        (vector-ref feature-names i)))
    (printf "~a ~a~n" (if out "good:" "bad: ") pretty-inp))
  )


(match subcommand
  ['() (do-synthesize)]
  ['("synthesize") (do-synthesize)]
  ['("unsat-core") (do-unsat-core)]
  ['("minimize-unsat-core") (do-minimize-unsat-core)]
  ['("slice-tests") (do-slice-tests)]
  )
