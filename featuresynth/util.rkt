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

  thread-name
  z3-named z3-named*

  solver-debug2
  )

(require racket/place)
(require ffi/unsafe)
(require rosette/solver/smt/z3)

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


(define thread-name
  (make-parameter
    "racket-thread"
    (lambda (name)
      (set-os-thread-name name)
      name)))

(define (set-os-thread-name name)
  ; Only works on amd64 linux, for now.
  (when (equal? (path->string (system-library-subpath #f)) "x86_64-linux")
    (define lib (ffi-lib #f))
    (define prctl (get-ffi-obj "prctl" lib (_fun _int _string -> _void)))
    (define PR_SET_NAME 15)
    (prctl PR_SET_NAME name)))

(define name-filter-regexp
  (regexp "['\000-\037]+"))

(define (z3-named name
                  #:logic [logic #f]
                  #:options [options (hash)])
  (set! name (string-replace name name-filter-regexp " "))
  (define runner-script (make-temporary-file))
  (call-with-output-file*
    runner-script
    #:exists 'truncate
    (lambda (f)
      (fprintf f "#!/bin/sh~n")
      (fprintf f "exec nametag '~a' z3 \"$@\"~n" name)))
  (file-or-directory-permissions runner-script #o700)
  (define solver (z3 #:path runner-script #:logic logic #:options options))
  ; Make a transaction to ensure the solver is actually running before deleting
  ; the runner-script.
  (solver-check solver)
  (delete-file runner-script)
  solver)

(define has-nametag-storage 'unset)

(define (has-nametag)
  (when (eq? has-nametag-storage 'unset)
    (set! has-nametag-storage (not (not (find-executable-path "nametag")))))
  has-nametag-storage)

; Create a solver instance using `z3-named` if the `nametag` helper program is
; available, and otherwise create it using ordinary `z3`.
(define (z3-named* #:logic [logic #f]
                   #:options [options (hash)])
  (if (has-nametag)
    (z3-named (thread-name) #:logic logic #:options options)
    (z3 #:logic logic #:options options)
    ))


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
