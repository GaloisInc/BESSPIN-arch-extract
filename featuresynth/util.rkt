#lang rosette

(provide
  if-let
  )

(define-syntax-rule (if-let ([x e]) f1 f2)
  (let ([x e])
    (if x f1 f2)))
