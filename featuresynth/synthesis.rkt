#lang rosette

(provide
  if-let
  (struct-out feature)
  (struct-out group)
  (struct-out dependency)
  (struct-out feature-model)
  valid-feature-model eval-feature-model
  ?*feature-model ?*config
  make-feature-model vector->feature-model
  oracle-guided-synthesis
  oracle-guided-synthesis+
  minimize-tests
)
  
(require racket/random)
(require rosette/solver/smt/z3)


(define-syntax-rule (if-let ([x e]) f1 f2)
  (let ([x e])
    (if x f1 f2)))


(struct feature (parent-id group-id depth force-on force-off) #:transparent)
(struct group (parent-id min-card max-card) #:transparent)
(struct dependency (a b) #:transparent)
(struct feature-model (features groups dependencies) #:transparent)

(define (feature-model-feature fm i)
  (vector-ref (feature-model-features fm) i))

(define (feature-model-group fm j)
  (vector-ref (feature-model-groups fm) j))

(define (feature-model-dependency fm k)
  (vector-ref (feature-model-dependencies fm) k))

(define (feature-model-group-members fm j)
  (apply +
    (for/list ([f (feature-model-features fm)])
      (if (= j (feature-group-id f)) 1 0))))

(define (count-enabled-group-members fm cfg j)
  (apply +
    (for/list ([(f i) (in-indexed (feature-model-features fm))])
      (if (and (= j (feature-group-id f)) (vector-ref cfg i)) 1 0))))

(define (all-group-members-disabled fm cfg j)
  (apply &&
    (for/list ([(f i) (in-indexed (feature-model-features fm))])
      (if (= j (feature-group-id f)) (not (vector-ref cfg i)) #t))))

(define (feature-model-num-features fm) (vector-length (feature-model-features fm)))
(define (feature-model-num-groups fm) (vector-length (feature-model-groups fm)))
(define (feature-model-num-dependencies fm) (vector-length (feature-model-dependencies fm)))


; Feature model validity checks

(define (valid-feature-id fm i)
  (&&
    (>= i -1)
    (< i (feature-model-num-features fm))))

(define (valid-group-id fm i)
  (&&
    (>= i -1)
    (< i (feature-model-num-groups fm))))

(define (valid-feature fm f)
  (&&
    (valid-feature-id fm (feature-parent-id f))
    (valid-group-id fm (feature-group-id f))
    (if (>= (feature-parent-id f) 0)
      (let
        ([pf (feature-model-feature fm (feature-parent-id f))])
        (= (feature-depth f) (+ 1 (feature-depth pf))))
      (= 0 (feature-depth f)))
    (if (>= (feature-group-id f) 0)
      (let
        ([g (feature-model-group fm (feature-group-id f))])
        (= (feature-parent-id f) (group-parent-id g)))
      #t)
    (! (&& (feature-force-on f) (feature-force-off f)))
    (=> (feature-force-on f)
        (= -1 (feature-parent-id f) (feature-group-id f)))
    (=> (feature-force-off f)
        (= -1 (feature-parent-id f) (feature-group-id f)))
    ))

(define (valid-group fm j g)
  (&&
    (valid-feature-id fm (group-parent-id g))
    (let
      ([n (feature-model-group-members fm j)])
      (||
        ; Disabled/unused group
        (= 0 n (group-min-card g) (group-max-card g))
        ; XOR group
        (&&
          (= 1 (group-min-card g) (group-max-card g))
          (>= n 2))
        ; OR group
        (&&
          (= 1 (group-min-card g))
          (= n (group-max-card g))
          (>= n 2))
        ))))

(define (valid-dependency fm d)
  (&&
    (valid-feature-id fm (dependency-a d))
    (valid-feature-id fm (dependency-b d))
    (||
      (= -1 (dependency-a d) (dependency-b d))
      (let ([f (feature-model-feature fm (dependency-a d))])
        (&&
          (<= 0 (dependency-a d))
          (<= 0 (dependency-b d))
          (not (= (dependency-a d) (dependency-b d)))
          (not (= (feature-parent-id f)
                  (dependency-b d)))
          ; Prefer making A a child of B, over making A a child of the root
          ; with a dependency on B.
          (not (= -1 (feature-parent-id f)))
        )))))

(define (valid-feature-model fm)
  (apply &&
    (append
      (for/list ([f (feature-model-features fm)]) (valid-feature fm f))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (valid-group fm j g))
      (for/list ([d (feature-model-dependencies fm)]) (valid-dependency fm d))
      )))


; Feature model evaluator

(define (eval-feature fm i f cfg)
  (let
    ([p (feature-parent-id f)])
    (&&
      (if (>= p 0) (=> (vector-ref cfg i) (vector-ref cfg p)) #t)
      (=> (feature-force-on f) (vector-ref cfg i))
      (=> (feature-force-off f) (! (vector-ref cfg i)))
    )))

(define (eval-group fm j g cfg)
  (let
    ([p (group-parent-id g)])
    (if (or (< p 0) (vector-ref cfg p))
      ; Group's parent is enabled, or group has no parent (always enabled).
      (<=
        (group-min-card g)
        (count-enabled-group-members fm cfg j)
        (group-max-card g))
      ; Group has a parent, and it's disabled.
      (all-group-members-disabled fm cfg j))))

(define (eval-dependency fm d cfg)
  (let
    ([a (dependency-a d)]
     [b (dependency-b d)])
    (if (not (= a -1)) (=> (vector-ref cfg a) (vector-ref cfg b)) #t)))

(define (eval-feature-model fm cfg)
  (apply &&
    (append
      (for/list ([(f i) (in-indexed (feature-model-features fm))])
        (eval-feature fm i f cfg))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (eval-group fm j g cfg))
      (for/list ([d (feature-model-dependencies fm)])
        (eval-dependency fm d cfg))
      )))


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
  (dependency (?*feature-id) (?*feature-id)))

(define (?*feature-model num-features num-groups num-dependencies)
  (feature-model
    (build-vector num-features (lambda (i) (?*feature)))
    (build-vector num-groups (lambda (i) (?*group)))
    (build-vector num-dependencies (lambda (i) (?*dependency)))
  ))

(define (?*config num-features)
  (build-vector num-features (lambda (i) (?*bool))))

(define (clone-symbolic-feature-model fm)
  (?*feature-model
    (vector-length (feature-model-features fm))
    (vector-length (feature-model-groups fm))
    (vector-length (feature-model-dependencies fm))))


; Concrete construction helpers

; These functions operate on "unresolved" features, groups, and dependencies,
; which are instances of the normal feature/group/dependency structs, except
; with names in place of feature/group indexes.

(struct name-map (features groups) #:transparent)

(define (resolve-feature-id nm i)
  (if i
    (hash-ref (name-map-features nm) i)
    -1))

(define (resolve-group-id nm j)
  (if j
    (hash-ref (name-map-groups nm) j)
    -1))

(define (resolve-feature nm f)
  (feature
    (resolve-feature-id nm (feature-parent-id f))
    (resolve-group-id nm (feature-group-id f))
    (feature-depth f)
    (feature-force-on f)
    (feature-force-off f)
  ))

(define (resolve-group nm g)
  (group
    (resolve-feature-id nm (group-parent-id g))
    (group-min-card g)
    (group-max-card g)
  ))

(define (resolve-dependency nm d)
  (dependency
    (resolve-feature-id nm (dependency-a d))
    (resolve-feature-id nm (dependency-b d))
  ))

(define (resolve-feature-model nm fm)
  (feature-model
    (vector-map (lambda (f) (resolve-feature nm f)) (feature-model-features fm))
    (vector-map (lambda (f) (resolve-group nm f)) (feature-model-groups fm))
    (vector-map (lambda (f) (resolve-dependency nm f)) (feature-model-dependencies fm))
  ))

(define (make-feature-model fs gs ds)
  (let*
    ([nm (name-map (make-hash) (make-hash))]
     [fs  ; vector of unresolved features
       (for/vector ([(kv i) (in-indexed fs)])
         (hash-set! (name-map-features nm) (car kv) i)
         (cdr kv))]
     [gs  ; vector of unresolved groups
       (for/vector ([(kv i) (in-indexed gs)])
         (hash-set! (name-map-groups nm) (car kv) i)
         (cdr kv))]
     [ds (for/vector ([d ds]) d)])
    (resolve-feature-model nm (feature-model fs gs ds))))


; Synthesis

; Like `(evaluate cfg M)`, but for any entries of the config that remain
; symbolic, it flips a coin.
(define (evaluate-config cfg M)
  (for/vector ([x (evaluate cfg M)])
    (if (term? x)
      (let ([y (= 1 (random 2))])
        (displayln (list "concretize" x "->" y))
        y)
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
(define (oracle-guided-synthesis+ symbolic-fm)
  (define solver (z3))
  (define symbolic-config (?*config (feature-model-num-features symbolic-fm)))
  (define tests '())

  (define (synthesize)
    (printf "synthesizing from ~a tests (~a positive)~n"
            (length tests) (length (filter cdr tests)))
    (if-let ([concrete-fm (try-evaluate symbolic-fm (solver-check solver))])
      ; Found a program - now check if it's unique
      (distinguish concrete-fm)
      ; Didn't find a program
      #f))
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

  (solver-assert solver (list (valid-feature-model symbolic-fm)))

  (lambda args
    (match args
      [(list 'test (cons inp out))
       (solver-assert solver
         (list (<=> out (eval-feature-model symbolic-fm inp))))
       (set! tests (cons (cons inp out) tests))
       (void)]
      [(list 'synthesize) (synthesize)]
      [(list 'get-tests) tests])))

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


(define (check-unique solver symbolic-fm concrete-fm tests filt)
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
    (unsat? (solver-check solver))))

(define (minimize-tests symbolic-fm concrete-fm tests)
  (define tests* (for/vector ([t tests]) t))
  (define solver (z3))
  (define removed (mutable-set))

  (define (check-without lo hi)
    (define (filt i)
      (not (or (and (<= lo i) (< i hi)) (set-member? removed i))))
    (check-unique solver symbolic-fm concrete-fm tests filt))
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


; Deserialization of feature models

(define (basic-deserializer ctor)
  (lambda (v)
    (apply ctor (cdr (vector->list v)))))

(define (vector->feature-model v)
  (feature-model
    (vector-map (basic-deserializer feature) (vector-ref v 1))
    (vector-map (basic-deserializer group) (vector-ref v 2))
    (vector-map (basic-deserializer dependency) (vector-ref v 3))))
