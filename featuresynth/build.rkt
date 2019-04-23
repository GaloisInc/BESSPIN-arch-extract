#lang racket

(provide
  make-feature-model
  vector->feature-model
  feature-name-list->name-map
  name-map->func

  map-feature
  map-group
  map-dependency
  map-constraint
  map-feature-model

  resolve-feature
  resolve-group
  resolve-dependency
  resolve-constraint
  resolve-feature-model
  )

(require "types.rkt")


; Concrete construction helpers

; These functions operate on "unresolved" features, groups, and dependencies,
; which are instances of the normal feature/group/dependency structs, except
; with names in place of feature/group indexes.

(struct name-map (features groups) #:transparent)

(define (name-map->func nm)
  (lambda (kind loc val)
    (match kind
      ['feature (if val (hash-ref (name-map-features nm) val) -1)]
      ['group (if val (hash-ref (name-map-groups nm) val) -1)]
      [else (raise "expected 'feature or 'group")])))


(define (map-feature func f)
  (feature
    (func 'feature 'feature (feature-parent-id f))
    (func 'group 'feature (feature-group-id f))
    (feature-depth f)
    (feature-force-on f)
    (feature-force-off f)
  ))

(define (map-group func g)
  (group
    (func 'feature 'group (group-parent-id g))
    (group-min-card g)
    (group-max-card g)
  ))

(define (map-dependency func d)
  (dependency
    (func 'feature 'dependency (dependency-a d))
    (func 'feature 'dependency (dependency-b d))
    (dependency-val d)
  ))

(define (map-constraint func c)
  (define (loop want c)
    (match c
      [(? boolean?) c]
      [(cons '&& args) (cons '&& (map (lambda (c) (loop want c)) args))]
      [(cons '|| args) (cons '|| (map (lambda (c) (loop want c)) args))]
      [(cons '! args) (cons '! (map (lambda (c) (loop (not want) c)) args))]
      [(list '=> a b) (list '=> (loop (not want) a) (loop want b))]
      [(list '<=> a b) (loop want `(&& (=> ,a ,b) (=> ,b ,a)))]
      [else
        (match (func 'feature 'constraint c)
          ['want want]
          [x x])]
      ))
  (loop #t c))

(define (map-feature-model func fm)
  (feature-model
    (vector-map (lambda (f) (map-feature func f)) (feature-model-features fm))
    (vector-map (lambda (f) (map-group func f)) (feature-model-groups fm))
    (vector-map (lambda (f) (map-dependency func f)) (feature-model-dependencies fm))
    (map-constraint func (feature-model-constraint fm))
  ))


(define (resolve-feature nm f)
  (map-feature (name-map->func nm) f))

(define (resolve-group nm g)
  (map-group (name-map->func nm) g))

(define (resolve-dependency nm d)
  (map-dependency (name-map->func nm) d))

(define (resolve-constraint nm c)
  (map-constraint (name-map->func nm) c))

(define (resolve-feature-model nm fm)
  (map-feature-model (name-map->func nm) fm))


(define (make-feature-model fs gs ds c)
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
    (resolve-feature-model nm (feature-model fs gs ds c))))

(define (feature-name-list->name-map names)
  (define features (make-hash))
  (for ([(n i) (in-indexed names)])
    (define sym (if (string? n) (string->symbol n) n))
    (define str (if (symbol? n) (symbol->string n) n))
    (hash-set! features sym i)
    (hash-set! features str i))
  (name-map features (hash)))


; Deserialization of feature models

(define (basic-deserializer ctor)
  (lambda (v)
    (apply ctor (cdr (vector->list v)))))

(define (vector->feature-model v)
  (feature-model
    (vector-map (basic-deserializer feature) (vector-ref v 1))
    (vector-map (basic-deserializer group) (vector-ref v 2))
    (vector-map (basic-deserializer dependency) (vector-ref v 3))
    (vector-ref v 4)))
