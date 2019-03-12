#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/match)
(require rosette/lib/synthax)
(current-bitwidth #f)


(struct feature (parent-id group-id depth) #:transparent)
(struct group (parent-id min-card max-card) #:transparent)
(struct dependency (a b) #:transparent)
(struct fforce (feature-id value) #:transparent)
(struct feature-model (features groups dependencies fforces) #:transparent)

(define (feature-model-feature fm i)
  (vector-ref (feature-model-features fm) i))

(define (feature-model-group fm j)
  (vector-ref (feature-model-groups fm) j))

(define (feature-model-dependency fm k)
  (vector-ref (feature-model-dependencies fm) k))

(define (feature-model-fforce fm k)
  (vector-ref (feature-model-fforces fm) k))

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

(define (valid-fforce fm idx ff)
  (&&
    (valid-feature-id fm (fforce-feature-id ff))
    (if (> idx 0)
      (let ([prev-feature-id
              (fforce-feature-id (feature-model-fforce fm (- idx 1)))])
        (||
          (= -1 (fforce-feature-id ff) prev-feature-id)
          (> (fforce-feature-id ff) prev-feature-id)))
      #t)
    (if (= -1 (fforce-feature-id ff)) #t
      (let ([f (feature-model-feature fm (fforce-feature-id ff))])
        (&&
          (= (feature-parent-id f) -1)
          (= (feature-group-id f) -1)
          (boolean? (fforce-value ff))
          )))))

(define (valid-feature-model fm)
  (apply &&
    (append
      (for/list ([f (feature-model-features fm)]) (valid-feature fm f))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (valid-group fm j g))
      (for/list ([d (feature-model-dependencies fm)]) (valid-dependency fm d))
      (for/list ([(ff idx) (in-indexed (feature-model-fforces fm))])
        (valid-fforce fm idx ff))
      )))


; Feature model evaluator

(define (eval-feature fm i f cfg)
  (let
    ([p (feature-parent-id f)])
    (if (>= p 0) (=> (vector-ref cfg i) (vector-ref cfg p)) #t)))

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

(define (eval-fforce fm ff cfg)
  (let
    ([i (fforce-feature-id ff)])
    (if (= -1 i) #t
      (<=> (vector-ref cfg i) (fforce-value ff)))))

(define (eval-feature-model fm cfg)
  (apply &&
    (append
      (for/list ([(f i) (in-indexed (feature-model-features fm))])
        (eval-feature fm i f cfg))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (eval-group fm j g cfg))
      (for/list ([d (feature-model-dependencies fm)])
        (eval-dependency fm d cfg))
      (for/list ([ff (feature-model-fforces fm)])
        (eval-fforce fm ff cfg))
      )))


; Symbolic construction helpers

; (?*) is a dynamic version of (??) - it generates a distinct constant on
; every call.

(define (?*) (define-symbolic* i integer?) i)
(define (?*bool) (define-symbolic* b boolean?) b)

(define (?*feature-id) (define-symbolic* fid integer?) fid)
(define (?*group-id) (define-symbolic* gid integer?) gid)

(define (?*feature)
  (feature (?*feature-id) (?*group-id) (?*)))

(define (?*group)
  (group (?*feature-id) (?*) (?*)))

(define (?*dependency)
  (dependency (?*feature-id) (?*feature-id)))

(define (?*fforce)
  (fforce (?*feature-id) (?*bool)))

(define (?*feature-model num-features num-groups num-dependencies num-fforces)
  (feature-model
    (build-vector num-features (lambda (i) (?*feature)))
    (build-vector num-groups (lambda (i) (?*group)))
    (build-vector num-dependencies (lambda (i) (?*dependency)))
    (build-vector num-fforces (lambda (i) (?*fforce)))
  ))

(define (?*config num-features)
  (build-vector num-features (lambda (i) (?*bool))))


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

(define (resolve-fforce nm ff)
  (fforce
    (resolve-feature-id nm (fforce-feature-id ff))
    (fforce-value ff)
  ))

(define (resolve-feature-model nm fm)
  (feature-model
    (vector-map (lambda (f) (resolve-feature nm f)) (feature-model-features fm))
    (vector-map (lambda (f) (resolve-group nm f)) (feature-model-groups fm))
    (vector-map (lambda (f) (resolve-dependency nm f)) (feature-model-dependencies fm))
    (vector-map (lambda (f) (resolve-fforce nm f)) (feature-model-fforces fm))
  ))

(define (make-feature-model fs gs ds ffs)
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
     [ds (for/vector ([d ds]) d)]
     [ffs (for/vector ([ff ffs]) ff)])
    (resolve-feature-model nm (feature-model fs gs ds ffs))))


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

(define-syntax-rule (if-let ([x e]) f1 f2)
  (let ([x e])
    (if x f1 f2)))

(define (try-evaluate e M)
  (if (unsat? M) #f (evaluate e M)))

(define (try-evaluate-config e M)
  (if (unsat? M) #f (evaluate-config e M)))


(define (count-set cfg)
  (for/sum ([x cfg]) (if x 1 0)))

(define (config-distance cfg1 cfg2)
  (for/sum ([x cfg1] [y cfg2]) (if (not (<=> x y)) 1 0)))

(define (config-min-distance cfg1 cfgs2)
  (for/fold ([acc (vector-length cfg1)]) ([cfg2 cfgs2])
    (let ([dist (config-distance cfg1 cfg2)])
      (if (< dist acc) dist acc))))

(define (choose-input tests inp1 inp2)
  ; Pick the one that's most different from all previous tests.
  (if (> (config-min-distance inp1 tests) (config-min-distance inp2 tests)) inp1 inp2))
  ; Pick the one with fewer features enabled.  This should generally choose the
  ; one that's more likely to succeed.
  ;(if (< (count-set inp1) (count-set inp2)) inp1 inp2))


(define (distinguishing-input solver-func symbolic-fm concrete-fm
                               symbolic-config tests)
  (let*
    ([pops 0]
     [get-input
       (lambda (constraint)
         (set! pops (+ pops 1))
         (try-evaluate-config symbolic-config (solver-func constraint)))]
     [base-constraint
       (not (<=> (eval-feature-model symbolic-fm symbolic-config)
                 (eval-feature-model concrete-fm symbolic-config)))]
     [alt-constraint
       (lambda (concrete-config)
         (not (equal? symbolic-config concrete-config)))])

    (begin0
      (if-let ([inp1 (get-input base-constraint)])
        ;(if-let ([inp2 (get-input (alt-constraint inp1))])
        ;  (choose-input tests inp1 inp2)
        ;  inp1)
        inp1
        #f)
      (when (> pops 0) (solver-func pops)))))

(define (oracle-guided-synthesis symbolic-fm oracle tests)
  (letrec
    ([solver (solve+)]
     [symbolic-config (?*config (feature-model-num-features symbolic-fm))]
     [synthesize
       (lambda (tests M)
         (displayln (list "synthesizing from" (length tests) "tests"))
         (if-let ([concrete-fm (try-evaluate symbolic-fm M)])
           (distinguish tests concrete-fm)
           #f))]
     [distinguish
       (lambda (tests concrete-fm)
         (if-let
           ([input
              (distinguishing-input solver symbolic-fm concrete-fm
                                    symbolic-config tests)])
           (let ([output (oracle input)])
             (synthesize (cons (cons input output) tests)
                         (solver (<=> output (eval-feature-model symbolic-fm input)))))
           concrete-fm))])
    (solver (valid-feature-model symbolic-fm))
    (for ([t tests])
      (solver (<=> (cdr t) (eval-feature-model symbolic-fm (car t)))))
    (synthesize tests (solver #t))))


; Demo

(define example-fm-2
  (make-feature-model
    (list
      (cons 'a1 (feature #f #f 0))
      (cons 'a2 (feature #f #f 0))
      (cons 'b1 (feature 'a1 'gb 1))
      (cons 'b2 (feature 'a1 'gb 1))
      (cons 'c1 (feature 'a2 'gc 1))
      (cons 'c2 (feature 'a2 'gc 1))
    )
    (list
      (cons 'gb (group 'a1 1 1))
      (cons 'gc (group 'a2 1 1))
    )
    (list
      (dependency 'b1 'c2)
    )
    (list
      (fforce 'a2 #t)
    )
  ))

(define secure-cpu-isa-fm
  (make-feature-model
    (list
      (cons 'riscv  (feature #f 'g-isa 0))
      (cons 'intel  (feature #f 'g-isa 0))
      (cons 'arm    (feature #f 'g-isa 0))
      (cons 'rv32   (feature 'riscv 'g-riscv-width 1))
      (cons 'rv64   (feature 'riscv 'g-riscv-width 1))
      (cons 'rv128  (feature 'riscv 'g-riscv-width 1))
      (cons 'rv-m   (feature 'riscv #f 1))
      (cons 'rv-a   (feature 'riscv #f 1))
      (cons 'rv-d   (feature 'riscv #f 1))
      (cons 'rv-c   (feature 'riscv #f 1))
      (cons 'rv-f   (feature 'riscv #f 1))
      (cons 'ia32   (feature 'intel 'g-intel-arch 1))
      (cons 'x86-64 (feature 'intel 'g-intel-arch 1))
      (cons 'aarch32 (feature 'arm 'g-arm-arch 1))
      (cons 'aarch64 (feature 'arm 'g-arm-arch 1))
    )
    (list
      (cons 'g-isa (group #f 1 1))
      (cons 'g-riscv-width (group 'riscv 1 1))
      (cons 'g-intel-arch (group 'intel 1 1))
      (cons 'g-arm-arch (group 'arm 1 1))
    )
    (list
      (dependency 'rv-d 'rv-f)
    )
    (list
    )
  ))

(random-seed 12345)
(define symbolic-fm (?*feature-model 15 4 1 0))
(define (oracle inp) (eval-feature-model secure-cpu-isa-fm inp))
;(define symbolic-fm (?*feature-model 6 2 1 0))
;(define (oracle inp) (eval-feature-model example-fm-2 inp))
(define synth-fm (oracle-guided-synthesis symbolic-fm oracle '()))
(pretty-write (list "synthesis result" synth-fm))
