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

(define raw-resume-tests
  (if (and config-resume-tests-file (file-exists? config-resume-tests-file))
    (call-with-default-reading-parameterization
      (lambda () (read-many-from-file config-resume-tests-file)))
    '()))

(define name-map (feature-name-list->name-map feature-names))

(define hard-constraints
  (for/list ([c config-hard-constraints])
    (resolve-constraint name-map c)))

(define resume-tests
  (filter
    (match-lambda
      [`(,inp ,out ,meta)
        (and
          ; If it failed the old constraint, then we don't know whether it
          ; would pass or fail under the current one.
          (not (assoc 'fails-constraint meta))
          ; Also filter out tests that fail the current constraint.  The solver
          ; will already know from the constraint itself that these tests fail.
          (=> out (eval-constraint (cons '&& hard-constraints) inp)))])
    raw-resume-tests))

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
      ;(find-fixed 20 10 ,@symbolic-fm-args)
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
  (if fm
    (output-feature-model fm)
    (output-unsat (apply ?*feature-model symbolic-fm-args)
                  ; TODO - more principled way to get tests out of the manager
                  (read-many-from-file "test-log.rktd")))
  )

(define (output-feature-model fm)
  (pretty-write fm)
  (define clafer-str (clafer->string (feature-model->clafer feature-names fm)))
  (displayln clafer-str)
  (when config-out-file
    (call-with-output-file*
      config-out-file
      #:exists 'truncate
      (lambda (f) (write-string clafer-str f)))))

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

  ;(printf "Reduced feature model template:~n")
  ;(pretty-write (slice-symbolic-fm symbolic-fm vs))
  ;(printf "~n")

  (printf "No valid feature model exists for the combination of the following tests:~n")
  (for ([t min-tests])
    (printf "  ~a ~a~n"
            (if (second t) "ok: " "bad:")
            (test-features t vs)))
  )



(define (do-unsat-core)
  (output-unsat (apply ?*feature-model symbolic-fm-args) resume-tests))

; Get the names of all features enabled in the input for test `t`.
(define (test-features t [vs #f])
  (match-define `(,inp ,out ,meta) t)
  (for/list ([i (or vs (in-range (vector-length inp)))] #:when (vector-ref inp i))
    (vector-ref feature-names i)))

(define (do-render-tests)
  (for ([t resume-tests])
    (displayln (test-features t))))


(match subcommand
  ['() (do-synthesize)]
  ['("synthesize") (do-synthesize)]
  ['("unsat-core") (do-unsat-core)]
  ['("render-tests") (do-render-tests)]
  )
