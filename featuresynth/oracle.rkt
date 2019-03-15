#lang racket

(provide
  run-oracle
  )

(require "build.rkt")
(require "eval.rkt")
(require "util.rkt")


(define (run-oracle-command cmd feature-names inp)
  (define-values (proc p-out p-in p-err)
    (subprocess #f #f (current-error-port) "/bin/sh" "-c" cmd))
  (close-input-port p-out)
  (for ([name feature-names]
        [val inp])
    (fprintf p-in "~a ~a~n" (if val 1 0) name))
  (close-output-port p-in)
  (subprocess-wait proc)
  (= 0 (subprocess-status proc)))


(define (oracle-eval-fm fm chan)
  (for ([msg (in-place-channel chan)])
    (match-define `(,inp ,meta) msg)
    (define out (eval-feature-model fm inp))
    (place-channel-put chan `(,inp ,out ,meta))))

(define (oracle-command cmd feature-names chan)
  (for ([msg (in-place-channel chan)])
    (match-define `(,inp ,meta) msg)
    (define out (run-oracle-command cmd feature-names inp))
    (place-channel-put chan `(,inp ,out ,meta))))

(define (oracle-multi-cached num-threads cache-file oracle-spec chan)
  (define cache
    (make-hash
      (if (and cache-file (file-exists? cache-file))
        (for/list ([l (file->lines cache-file)]) (read (open-input-string l)))
        '())))
  (define save-entry
    (if cache-file
      (let
        ([out-file (open-output-file cache-file #:exists 'append)])
        (lambda (inp out) (writeln `(,inp ,out) out-file)))
      (lambda (inp out) (void))))
  (define (handle-result inp out meta)
    (hash-set! cache inp out)
    (save-entry inp out)
    (place-channel-put chan `(,inp ,out ,meta)))

  (define backends
    (build-vector num-threads
      (lambda (i)
        (place/context backend-chan (run-oracle oracle-spec backend-chan)))))
  (define backend-idle (make-vector num-threads #t))

  (define backend-evt
    (apply choice-evt
      (for/list ([(chan i) (in-indexed backends)])
        (handle-evt chan (lambda (x) (list i x))))))
  (define any-evt
    (choice-evt
      (handle-evt backend-evt (lambda (x) (cons 'backend x)))
      (handle-evt chan (lambda (x) (list 'client x)))))

  ; Get the index of an idle backend.  Blocks (and processes backend results)
  ; if no backend is currently idle.
  (define (get-idle-backend)
    (or
      (for/first ([(idle i) (in-indexed backend-idle)] #:when idle) i)
      (begin
        ; No backend is idle.  Process a backend event: the backend that
        ; produced the event is now idle.
        (match-let
          ([`(,idx (,inp ,out ,meta)) (sync backend-evt)])
          (handle-result inp out meta)
          (vector-set! backend-idle idx #t)
          idx))))

  (define (handle-request inp meta)
    (if (hash-has-key? cache inp)
      (let
        ([out (hash-ref cache inp)])
        (place-channel-put chan `(,inp ,out ,meta)))
      (let
        ([idx (get-idle-backend)])
        (vector-set! backend-idle idx #f)
        (place-channel-put (vector-ref backends idx) `(,inp ,meta)))))

  (define (loop)
    (match (sync any-evt)
      [`(client (,inp ,meta))
        (handle-request inp meta)
        (loop)]
      [`(backend ,idx (,inp ,out ,meta))
        (vector-set! backend-idle idx #t)
        (handle-result inp out meta)
        (loop)]))

  (loop))

(define (run-oracle spec chan)
  (match spec
    [`(eval-fm ,fm)
     (oracle-eval-fm (vector->feature-model fm) chan)]
    [`(command ,cmd ,feature-names)
     (oracle-command cmd feature-names chan)]
    [`(multi-cached ,num-threads ,cache-file ,oracle-spec)
      (oracle-multi-cached num-threads cache-file oracle-spec chan)]
    ))
