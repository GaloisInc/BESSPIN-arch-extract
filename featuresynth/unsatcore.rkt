#lang racket

(provide
  delta-minimize

  get-unsat-core
  minimize-unsat-core
  slice-symbolic-fm slice-test
  minimize-features
  )

(require "build.rkt")
(require "synthesis.rkt")
(require "types.rkt")
(require "util.rkt")

(require rosette/base/core/reflect)


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
      (fprintf (current-error-port) "delta-minimize: try without chunk ~a~n" i)
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


(define (get-unsat-core symbolic-fm tests)
  (define synth (oracle-guided-synthesis+ symbolic-fm #:unsat-cores #t))
  (define min-tests (synth 'unsat-core tests))
  (fprintf (current-error-port)
           "get-unsat-core: found unsat core with ~a tests~n"
           (length min-tests))
  min-tests)

(define (unsat-core? symbolic-fm tests)
  (define synth (oracle-guided-synthesis+ symbolic-fm))
  (for ([t tests])
    (match-define `(,inp ,out ,meta) t)
    (synth 'test (cons inp out)))
  (not (synth 'check-sat)))

(define (minimize-unsat-core symbolic-fm tests)
  (define min-tests (delta-minimize tests (lambda (ts) (unsat-core? symbolic-fm ts))))
  (fprintf (current-error-port) "minimize-unsat-core: reduced ~a tests to ~a~n"
                                (length tests) (length min-tests))
  min-tests)

(define (minimal-unsat-core? symbolic-fm tests)
  (for/and ([(unsat-test i) (in-indexed tests)])
    (fprintf (current-error-port) "minimal-unsat-core?: try without test ~a~n" i)
    (define synth (oracle-guided-synthesis+ symbolic-fm))

    (for/list ([(t j) (in-indexed tests)] #:when (not (= i j)))
      (match-define `(,inp ,out ,meta) t)
      (synth 'test (cons inp out)))
    (define sat1 (synth 'check-sat))

    (match-define `(,inp ,out ,meta) unsat-test)
    (synth 'test (cons inp out))
    (define sat2 (synth 'check-sat))

    (and sat1 (not sat2))))

; Delete features from `symbolic-fm`, leaving only those whose indices appear
; in the list `vs`.
(define (slice-symbolic-fm symbolic-fm vs)
  (define feature-id-map (make-vector (feature-model-num-features symbolic-fm) #f))
  (for ([(old-id new-id) (in-indexed vs)])
    (vector-set! feature-id-map old-id new-id))

  (define (func kind loc val)
    (match kind
      ['feature
        (if-let ([new-id (and (not (term? val)) (vector-ref feature-id-map val))])
          new-id
          (match loc
            ['constraint 'want]
            [else (?*)]))]
      ['group val]
      [else (raise "expected 'feature or 'group")]))

  (define symbolic-fm2 (map-feature-model func symbolic-fm))

  (feature-model
    (for/vector ([(f i) (in-indexed (feature-model-features symbolic-fm2))]
                 #:when (vector-ref feature-id-map i))
      f)
    (feature-model-groups symbolic-fm2)
    (feature-model-dependencies symbolic-fm2)
    (feature-model-constraint symbolic-fm2)))

; Delete entries from the inputs of `test`, leaving only those whose indices
; appear in the list `vs`.
(define (slice-test test vs)
  (match-define `(,inp ,out ,meta) test)
  `(,(for/vector ([v vs]) (vector-ref inp v)) ,out ,meta))

; Minimize the number of features in `symbolic-fm`, ensuring that the list
; `tests` remains a valid unsat core.  Returns a list containing the indices of
; the remaining features after minimization, which can be passed as the `vs`
; argument of `slice-symbolic-fm` or `slice-test`.
(define (minimize-features symbolic-fm tests)
  (define all-vs
    (sequence->list (in-range (feature-model-num-features symbolic-fm))))
  (define min-vs
    (delta-minimize
      all-vs
      (lambda (vs)
        (minimal-unsat-core?
          (slice-symbolic-fm symbolic-fm vs)
          (for/list ([t tests]) (slice-test t vs))))))

  (fprintf (current-error-port)
           "slice-tests: sliced ~a variables, leaving only ~a~n"
           (length all-vs) (length min-vs))
  min-vs)




