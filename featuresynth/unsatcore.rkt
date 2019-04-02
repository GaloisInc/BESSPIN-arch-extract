#lang racket

(provide
  solver-debug2
  delta-minimize
  )

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


; Given a list of inputs `inp`, find a 1-minimal sublist that still satisfies
; `check`.
(define (delta-minimize inp check)
  ; Divide `inp` into chunks of size `chunk-size`, and try deleting each one.
  ; Returns a sublist of `inp` that passes `check`.
  (define (go chunk-size inp)
    (fprintf (current-error-port) "delta-minimize: chunk size = ~a, input size = ~a~n"
             chunk-size (length inp))
    (define chunks (list->vector (list-chunks inp chunk-size)))
    (define active (make-vector (vector-length chunks) #t))
    (define (build-input) (for/list ([c chunks] [a active] #:when a [x c]) x))
    (for ([i (in-range (vector-length chunks))])
      (vector-set! active i #f)
      (when (not (check (build-input)))
        (vector-set! active i #t)))
    (build-input))

  (let loop ([chunk-size (quotient (add1 (length inp)) 2)] [inp inp])
    (let
      ([inp* (go chunk-size inp)])
      (if (> chunk-size 1)
        (loop (quotient (add1 chunk-size) 2) inp*)
        inp*))))

(define (list-chunks l size)
  (if (<= (length l) size)
    (list l)
    (begin
      (let-values ([(chunk l*) (split-at l size)])
        (cons chunk (list-chunks l* size))))))
