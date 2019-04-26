#lang rosette

(provide
  if-let when-let
  pretty-write-to-file read-from-file
  read-many-from-port read-many-from-file
  struct->vector*

  in-place-channel
  place-channel-get-chunk
  in-place-channel-chunks

  test-parts

  solver-debug2
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


(define (test-parts ts)
  (make-do-sequence
    (lambda ()
      (values
        (lambda (st)
          (match-define `(,inp ,out ,meta) (stream-first st))
          (values inp out meta))
        stream-rest
        (sequence->stream ts)
        (lambda (st) (not (stream-empty? st)))
        #f
        #f
        ))))


; solver-debug2 doesn't work properly when defined in `#lang rosette` context.
(module util-racket racket
  (provide solver-debug2)

  (require rosette/solver/smt/base-solver)
  (require rosette/solver/smt/cmd)
  (require rosette/solver/smt/server)
  (require (only-in rosette/solver/smt/smtlib2 check-sat))

  ; This function is a terrible hack used to get unsat cores from Z3.  We'd like
  ; to set up some initial constraints, add additional constraints, and obtain an
  ; unsat core that contains only additional constraints (under the assumption
  ; that all the initial constraints hold).  Z3 can do this, but Rosette's normal
  ; `solver-debug` interface resets the solver first.  This hacked version
  ; doesn't do that.
  (define (solver-debug2 self)
    (define asserts (remove-duplicates (solver-asserts self)))
    (define server (solver-server self))
    (server-write
      server
      (begin (encode-for-proof (solver-env self) asserts)
             (check-sat)))
    (solver-clear-stacks! self)
    (read-solution server (solver-env self) #:unsat-core? #t))
  )

(require 'util-racket)
