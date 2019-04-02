#lang racket

(provide
  make-feature-model
  vector->feature-model
  feature-name-list->name-map
  resolve-constraint
  )

(require "types.rkt")


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
    (dependency-val d)
  ))

(define (resolve-constraint nm c)
  (define (loop want c)
    (match c
      [(? symbol?) (hash-ref (name-map-features nm) c want)]
      [(? integer?) c]
      [(? boolean?) c]
      [(cons '&& args) (cons '&& (map (lambda (c) (loop want c)) args))]
      [(cons '|| args) (cons '|| (map (lambda (c) (loop want c)) args))]
      [(cons '! args) (cons '! (map (lambda (c) (loop (not want) c)) args))]
      [(list '=> a b) (list '=> (loop (not want) a) (loop want b))]
      [(list '<=> a b) (loop want `(&& (=> ,a ,b) (=> ,b ,a)))]
      ))
  (loop #t c))

(define (resolve-feature-model nm fm)
  (feature-model
    (vector-map (lambda (f) (resolve-feature nm f)) (feature-model-features fm))
    (vector-map (lambda (f) (resolve-group nm f)) (feature-model-groups fm))
    (vector-map (lambda (f) (resolve-dependency nm f)) (feature-model-dependencies fm))
    (resolve-constraint nm (feature-model-constraint fm))
  ))

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
