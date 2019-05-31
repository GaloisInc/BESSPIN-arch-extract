#lang rosette

(provide
  feature-model-bexp
  feature-model-bdd
  robdd-nth-sat)

(require bdd/robdd)
(require "util.rkt")
(require "types.rkt")


(define (feature-bexp fm i f cfg)
  (let
    ([p (feature-parent-id f)])
    (list 'and
      (if (>= p 0) (list 'imp (vector-ref cfg i) (vector-ref cfg p)) #t)
      (list 'imp (feature-force-on f) (vector-ref cfg i))
      (list 'imp (feature-force-off f) (list 'not (vector-ref cfg i)))
    )))

; Construct a formula asserting that the number of #t values in `cfg` is >= 1
(define (card-ge-1 is cfg)
  (cons 'or (for/list ([i is]) (vector-ref cfg i))))

; Construct a formula asserting that the number of #t values in `cfg` is >= 2
(define (card-ge-2 is cfg)
  (cons 'or (for/list ([i1 is] #:when #t
                       [i2 is] #:when (< i1 i2))
              (list 'and
                (vector-ref cfg i1)
                (vector-ref cfg i2)))))

(define (ite c t e)
  `(and (imp ,c ,t) (imp (not ,c) ,e)))

(define (group-bexp fm j g cfg)
  (let*
    ([p (group-parent-id g)]
     [n (feature-model-group-members fm j)]
     [is (feature-model-group-member-indices fm j)]
     [card-ok
       (list 'and
         (match (group-min-card g)
           [0 #t]
           [1 (card-ge-1 is cfg)]
           [2 (card-ge-2 is cfg)]
           [_ (raise "can't handle min-card > 2")])
         (match (group-max-card g)
           [0 (list 'not (card-ge-1 is cfg))]
           [1 (list 'not (card-ge-2 is cfg))]
           [m #:when (eq? m n) #t]
           ['* #t]
           [_ (raise "can't handle 1 < max-card < *")]))])
    (if (>= p 0)
      (ite (vector-ref cfg p) card-ok (list 'not (card-ge-1 is cfg)))
      card-ok)))

(define (dependency-bexp fm d cfg)
  (let
    ([a (dependency-a d)]
     [b (dependency-b d)]
     [val (dependency-val d)])
    (if (not (= a -1))
      (list 'imp (vector-ref cfg a) (list 'eqv val (vector-ref cfg b)))
      #t)))

(define (constraint-bexp c cfg)
  (define (loop c) (constraint-bexp c cfg))
  (match c
    [(? integer?) (vector-ref cfg c)]
    [(? boolean?) c]
    [(cons '&& args) (cons 'and (map loop args))]
    [(cons '|| args) (cons 'or (map loop args))]
    [(cons '! args) (cons 'not (map loop args))]
    [(cons '=> args) (cons 'imp (map loop args))]
    [(cons '<=> args) (cons 'eqv (map loop args))]
    ))

(define (feature-model-bexp fm cfg)
  (cons 'and
    (append
      (for/list ([(f i) (in-indexed (feature-model-features fm))])
        (feature-bexp fm i f cfg))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (group-bexp fm j g cfg))
      (for/list ([d (feature-model-dependencies fm)])
        (dependency-bexp fm d cfg))
      (list (constraint-bexp (feature-model-constraint fm) cfg))
      )))


; Get the `idx`'th (0-based) satisfying solution to `bdd`.
(define (robdd-nth-sat bdd nvars idx)
  (for/vector ([i (in-range nvars)])
    (define bdd-f (robdd-restrict bdd (+ 1 i) #f))
    (define count-f (robdd-sat-count bdd-f (- nvars (add1 i))))
    (if (< idx count-f)
      (begin
        (set! bdd bdd-f)
        #f)
      (begin
        (set! bdd (robdd-restrict bdd (+ 1 i) #t))
        (set! idx (- idx count-f))
        #t))))

(define (feature-model-bdd fm)
  (define var-config
    (for/vector ([i (in-range (feature-model-num-features fm))])
      (string->symbol (format "v~a" i))))
  (define bexp (feature-model-bexp fm var-config))
  (make-robdd bexp (vector->list var-config)))
