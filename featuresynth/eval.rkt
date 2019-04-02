#lang rosette

(provide
  eval-claim
  eval-feature-model
  )

(require "types.rkt")


; Feature model evaluation

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
     [b (dependency-b d)]
     [val (dependency-val d)])
    (if (not (= a -1))
      (=> (vector-ref cfg a) (<=> val (vector-ref cfg b)))
      #t)))

; Evaluate a feature model to determine the validity of `cfg`.  Returns `#t` if
; `cfg` is allowed by model `fm`, and `#f` if it is forbidden.
(define (eval-feature-model fm cfg)
  (apply &&
    (append
      (for/list ([(f i) (in-indexed (feature-model-features fm))])
        (eval-feature fm i f cfg))
      (for/list ([(g j) (in-indexed (feature-model-groups fm))])
        (eval-group fm j g cfg))
      (for/list ([d (feature-model-dependencies fm)])
        (eval-dependency fm d cfg))
      (list (eval-constraint (feature-model-constraint fm) cfg))
      )))


; Claim evaluation

(define (eval-claim-fixed c cfg)
  (<=> (vector-ref cfg (claim-fixed-a c))
       (claim-fixed-val c)))

(define (eval-claim-dep c cfg)
  (=> (vector-ref cfg (claim-dep-a c))
      (vector-ref cfg (claim-dep-b c))))

(define (eval-claim-antidep c cfg)
  (=> (vector-ref cfg (claim-antidep-a c))
      (! (vector-ref cfg (claim-antidep-b c)))))

; Evaluate a claim to determine if it is compatible with the valid
; configuration `cfg`.  Returns `#f` if the validity of `cfg` disproves `c`,
; and `#t` otherwise.
(define (eval-claim c cfg)
  (cond
    [(claim-fixed? c) (eval-claim-fixed c cfg)]
    [(claim-dep? c) (eval-claim-dep c cfg)]
    [(claim-antidep? c) (eval-claim-antidep c cfg)]))


; Constraint evaluation

(define (eval-constraint c cfg)
  (define (loop c) (eval-constraint c cfg))
  (match c
    [(? integer?) (vector-ref cfg c)]
    [(? boolean?) c]
    [(cons '&& args) (apply && (map loop args))]
    [(cons '|| args) (apply || (map loop args))]
    [(cons '! args) (apply ! (map loop args))]
    [(cons '=> args) (apply => (map loop args))]
    [(cons '<=> args) (apply <=> (map loop args))]
    ))
