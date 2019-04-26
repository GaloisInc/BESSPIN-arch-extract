#lang rosette

(provide
  (struct-out feature)
  (struct-out group)
  (struct-out dependency)
  (struct-out feature-model)
  ?* ?*feature-model ?*config
  oracle-guided-synthesis
  oracle-guided-synthesis+
  minimize-tests
  minimize-counterexample
)

(require racket/random)
(require rosette/solver/smt/z3)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")


; Symbolic construction helpers

; (?*) is a dynamic version of (??) - it generates a distinct constant on
; every call.

(define (?*) (define-symbolic* i integer?) i)
(define (?*bool) (define-symbolic* b boolean?) b)

(define (?*feature-id) (define-symbolic* fid integer?) fid)
(define (?*group-id) (define-symbolic* gid integer?) gid)

(define (?*feature)
  (feature (?*feature-id) (?*group-id) (?*) (?*bool) (?*bool)))

(define (?*group)
  (group (?*feature-id) (?*) (?*)))

(define (?*dependency)
  (dependency (?*feature-id) (?*feature-id) (?*bool)))

(define (?*feature-model num-features num-groups num-dependencies [constraint #t])
  (feature-model
    (build-vector num-features (lambda (i) (?*feature)))
    (build-vector num-groups (lambda (i) (?*group)))
    (build-vector num-dependencies (lambda (i) (?*dependency)))
    (convert-constraint-wildcards constraint)
  ))

(define (?*config num-features)
  (build-vector num-features (lambda (i) (?*bool))))

(define (clone-symbolic-feature-model fm)
  (?*feature-model
    (vector-length (feature-model-features fm))
    (vector-length (feature-model-groups fm))
    (vector-length (feature-model-dependencies fm))))

(define (convert-constraint-wildcards c)
  (define loop convert-constraint-wildcards)
  (match c
    ['_ (?*feature-id)]
    [(? integer?) c]
    [(? boolean?) c]
    [(cons '&& args) (cons '&& (map loop args))]
    [(cons '|| args) (cons '|| (map loop args))]
    [(cons '! args) (cons '! (map loop args))]
    [(cons '=> args) (cons '=> (map loop args))]
    [(cons '<=> args) (cons '<=> (map loop args))]
    ))


; Synthesis

; Like `(evaluate cfg M)`, but for any entries of the config that remain
; symbolic, it flips a coin.
(define (evaluate-config cfg M)
  (for/vector ([x (evaluate cfg M)])
    (if (term? x)
      (= 1 (random 2))
      x)))

(define (try-evaluate e M)
  (if (unsat? M) #f (evaluate e M)))

(define (try-evaluate-config e M)
  (if (unsat? M) #f (evaluate-config e M)))


; Sets up a solver for performing oracle-guided synthesis, and returns a
; closure that can be called in the following ways:
;
; * `(f 'synthesize)`: Try to synthesize a feature model from the current set
;   of tests.  Returns a `feature-model?` if synthesis produces a unique
;   program, returns a new test input if synthesis produces a non-unique
;   program, and returns `#f` if synthesis fails to produce any program.
; * `(f 'test t)`: Add `t` to the current set of tests.  `t` should be a pair
;   of a test input (a vector of booleans, one for each feature in
;   `symbolic-fm`) and output (a boolean, with `#t` indicating that the input
;   vector represents a valid configuration).
(define (oracle-guided-synthesis+ symbolic-fm
                                  #:unsat-cores [unsat-cores #f])
  (define opts
    (if unsat-cores (hash ':produce-unsat-cores 'true) (hash)))
  (define solver (z3 #:options opts))
  (define symbolic-config (?*config (feature-model-num-features symbolic-fm)))
  (define tests '())

  ; Try to synthesize a unique program that passes all current tests.
  (define (synthesize)
    (printf "synthesizing from ~a tests (~a positive)~n"
            (length tests) (length (filter cdr tests)))
    (if-let ([concrete-fm (try-evaluate symbolic-fm (solver-check solver))])
      ; Found a program - now check if it's unique
      (distinguish concrete-fm)
      ; Didn't find a program
      #f))
  (define (synthesize-with expr)
    (solver-push solver)
    (solver-assert solver (list expr))
    (begin0
      (synthesize)
      (solver-pop solver)))
  (define (distinguish concrete-fm)
    (solver-push solver)
    (solver-assert solver
      (list
        (not (<=> (eval-feature-model symbolic-fm symbolic-config)
                  (eval-feature-model concrete-fm symbolic-config)))))
    (define M (solver-check solver))
    (begin0
      (if-let ([concrete-config (try-evaluate-config symbolic-config M)])
        ; Program is not unique - return a new test input
        concrete-config
        ; Program is unique - return it
        concrete-fm)
      (solver-pop solver)))

  (define (distinguish-prog concrete-fm)
    (solver-push solver)
    (solver-assert solver
      (list
        (not (<=> (eval-feature-model symbolic-fm symbolic-config)
                  (eval-feature-model concrete-fm symbolic-config)))))
    (define M (solver-check solver))
    (begin0
      (if (sat? M)
        ; Program is not unique - return a new test input
        (cons
          (evaluate symbolic-fm M)
          (evaluate-config symbolic-config M))
        ; Program is unique - return it
        #f)
      (solver-pop solver)))

  ; Try to synthesize a program `P` and input `I` such that (1) `P` passes all
  ; current tests, (2) `eval(P, I)` returns true (i.e., `I` is a valid
  ; configuration of feature model `P`), and (3) `I` disproves at least one
  ; claim in `claims`.
  (define (disprove claims)
    (define valid-constraint
      (eval-feature-model symbolic-fm symbolic-config))
    (define disprove-constraint
      (for/fold ([acc #f]) ([c claims])
        (|| acc (! (eval-claim c symbolic-config)))))
    (solver-push solver)
    (solver-assert solver (list valid-constraint disprove-constraint))
    (begin0
      (try-evaluate-config symbolic-config (solver-check solver))
      (solver-pop solver)))

  ; Compute an unsat core from a list of new tests.  The unsat core is a subset
  ; of the new tests that is sufficient to cause synthesis to fail.
  (define (unsat-core tests)
    (solver-push solver)
    (define M (solver-check solver))
    (when (not (sat? M)) (raise "ran unsat-core in already unsatisfiable state"))
    ; Assert predicates and build a table mapping symbolic exprs to tests
    (define test-map
      (for/hasheq ([t tests])
        (match-define `(,inp ,out ,meta) t)
        (define expr (<=> out (eval-feature-model symbolic-fm inp)))
        (solver-assert solver (list expr))
        (values expr t)))
    (define U (solver-debug2 solver))
    (when (not (unsat? U)) (raise "tests did not produce an unsat result"))
    (solver-pop solver)
    (for/list ([expr (core U)])
      (hash-ref test-map expr expr)))

  ; Try to prove that for all feature models satisfying the current tests,
  ; feature `idx` has value `val` in all configurations accepted by the feature
  ; model.
  (define (prove-fixed idx val)
    (solver-push solver)
    ; Assert that
    ;  (1) There exists a feature model that passes all tests
    ;  (2) There exists a configuration that satisfies the model
    ;  (3) The configuration does not assign `val` to feature `idx`.
    ; If this is UNSAT, then we have proved that the feature is fixed to `val`.
    (solver-assert solver (list (eval-feature-model symbolic-fm symbolic-config)))
    (begin0
      ; If there are no valid feature models or no valid configurations, abort.
      (if (not (sat? (solver-check solver))) #f
        (begin
          (solver-assert solver (list (! (<=> val (vector-ref symbolic-config idx)))))
          (unsat? (solver-check solver))))
      (solver-pop solver)))

  (solver-assert solver (list (valid-feature-model symbolic-fm)))

  (lambda args
    (match args
      [(list 'test (cons inp out))
       (solver-assert solver
         (list (<=> out (eval-feature-model symbolic-fm inp))))
       (set! tests (cons (cons inp out) tests))
       (void)]
      [(list 'synthesize) (synthesize)]
      [(list 'synthesize-with expr) (synthesize-with expr)]
      [(list 'distinguish fm) (distinguish-prog fm)]
      [(list 'get-tests) tests]
      [(list 'disprove claims) (disprove claims)]
      [(list 'assert-claims claims)
       (solver-assert solver
         (for/list ([c claims]) (eval-claim c symbolic-config)))]
      [(list 'fix-feature idx val)
       (define f (feature-model-feature symbolic-fm idx))
       (if val
         (solver-assert solver (list (feature-force-on f)))
         (solver-assert solver (list (feature-force-off f))))]
      [(list 'unsat-core tests) (unsat-core tests)]
      [(list 'check-sat) (sat? (solver-check solver))]
      [`(prove-fixed ,idx ,val) (prove-fixed idx val)]
    )))

(define (oracle-guided-synthesis symbolic-fm oracle init-tests)
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (for ([t init-tests]) (synth 'test t))
  (define (loop)
    (define result (synth 'synthesize))
    (cond
      [(feature-model? result) (cons result (synth 'get-tests))]
      [(vector? result)
       (synth 'test (cons result (oracle result)))
       (loop)]
      [(false? result) result]
      [else (raise "unreachable")]))
  (loop))


(define (check-unique solver symbolic-fm concrete-fm tests)
  (lambda (filt)
    (let
      ([symbolic-config (?*config (feature-model-num-features symbolic-fm))])

      (solver-clear solver)
      (solver-assert solver (list (valid-feature-model symbolic-fm)))
      (solver-assert solver
        (for/list ([(t i) (in-indexed tests)] #:when (filt i))
          (<=> (cdr t) (eval-feature-model symbolic-fm (car t)))))
      (solver-assert solver
        (list
          (not (<=> (eval-feature-model symbolic-fm symbolic-config)
                    (eval-feature-model concrete-fm symbolic-config)))))
      (unsat? (solver-check solver)))))

(define (check-unsynthesizable solver symbolic-fm tests)
  (lambda (filt)
    (let
      ([symbolic-config (?*config (feature-model-num-features symbolic-fm))])

      (solver-clear solver)
      (solver-assert solver (list (valid-feature-model symbolic-fm)))
      (solver-assert solver
        (for/list ([(t i) (in-indexed tests)] #:when (filt i))
          (<=> (cdr t) (eval-feature-model symbolic-fm (car t)))))
      (unsat? (solver-check solver)))))

(define (minimize-tests* tests check)
  (define tests* (for/vector ([t tests]) t))
  (define removed (mutable-set))

  (define (check-without lo hi)
    (define (filt i)
      (not (or (and (<= lo i) (< i hi)) (set-member? removed i))))
    (check filt))
  (define (remove-all lo hi)
    (displayln (list "removing" (- hi lo) "tests"))
    (for ([i (in-range lo hi)])
      (set-add! removed i)))
  (define (any-kept lo hi)
    (for/fold ([acc #f]) ([i (in-range lo hi)])
      (or acc (not (set-member? removed i)))))

  (define n (sequence-length tests))
  (for ([i (in-range (- (integer-length n) 1) -1 -1)])
    (displayln (list "depth" i ":" (- n (set-count removed)) "tests remain"))
    ; Divide `tests` into `step`-sized chunks, and try deleting each one.  The
    ; next time around the outer loop, we'll try chunks of half the size.  This
    ; strategy is much faster than a linear scan because it often can discard
    ; large chunks (100+ tests) in the early iterations, and later solver
    ; queries are faster when some tests have already been removed.
    (define step (arithmetic-shift 1 i))
    (for ([lo (in-range 0 n step)])
      (define hi (min n (+ lo step)))
      (when (and (< lo hi) (any-kept lo hi))
        (displayln (list "try without" lo ".." hi))
        (when (check-without lo hi)
          (remove-all lo hi)))))

  (for/list ([(t i) (in-indexed tests)]
             #:when (not (set-member? removed i)))
    t))

(define (minimize-tests symbolic-fm concrete-fm tests)
  (minimize-tests* tests (check-unique (z3) symbolic-fm concrete-fm tests)))

(define (minimize-counterexample symbolic-fm tests)
  (minimize-tests* tests (check-unsynthesizable (z3) symbolic-fm tests)))
