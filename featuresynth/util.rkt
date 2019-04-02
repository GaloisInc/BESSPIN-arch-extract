#lang rosette

(provide
  if-let when-let
  pretty-write-to-file read-from-file
  read-many-from-port read-many-from-file
  struct->vector*

  in-place-channel
  place-channel-get-chunk
  in-place-channel-chunks
  )

(require racket/place)

(define-syntax-rule (if-let ([x e]) f1 f2)
  (let ([x e])
    (if x f1 f2)))

(define-syntax-rule (when-let ([x e]) f ...)
  (let ([x e])
    (when x f ...)))

(define (pretty-write-to-file v path)
  (call-with-output-file* path
    (lambda (f) (pretty-write v f))
    #:exists 'truncate))

(define (read-from-file path)
  (call-with-input-file* path
    (lambda (f) (read f))))

(define (read-many-from-port port)
  (define (loop)
    (define inp (read port))
    (if (eof-object? inp)
      '()
      (cons inp (loop))))
  (loop))

(define (read-many-from-file path)
  (call-with-input-file* path read-many-from-port))

(define (struct->vector* s)
  (cond
    [(struct? s)
     (let
       ([v (struct->vector s)])
       (vector-map! struct->vector* v)
       v)]
    [(vector? s) (vector-map struct->vector* s)]
    [(list? s) (map struct->vector* s)]
    [(term? s) 'symbolic]
    [else s]))


(define (in-place-channel chan)
  (in-producer (lambda () (place-channel-get chan))))

; Retrieve all values that are currently available in `chan`.  If no values are
; available, block until at least one value arrives.
(define (place-channel-get-chunk chan)
  (define evt0 (sync chan))
  (define evts
    (for/list ([evt (in-producer (lambda () (sync/timeout 0 chan)))]
               #:break (not evt))
      evt))
  (cons evt0 evts))

(define (in-place-channel-chunks chan)
  (in-producer (lambda () (place-channel-get-chunk chan))))

