#lang racket

(require json)
(require threading)
(require bdd/robdd)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "fmjson.rkt")
(require "sample.rkt")
(require "simplify.rkt")

(define args (current-command-line-arguments))

(define (read-fmjson-from-file path)
  ; TODO run `clafer -m fmjson` if file extension is .cfr
  (define j (call-with-input-file* path read-json))
  (fmjson->feature-model j))

(define (parse-int s)
  (define n (string->number s))
  (when (not (integer? n))
    (raise (format "failed to parse `~a` as an integer" s)))
  n)

(define (check-range desc idx lo hi)
  (when (< idx lo) (raise (format "~a out of range: ~a < ~a" desc idx lo)))
  (when (>= idx hi) (raise (format "~a out of range: ~a >= ~a" desc idx hi)))
)

(define (config->json-string names cfg)
  (define parts
    (for/list ([n names] [v cfg])
      (format "  ~a: ~a~n" (jsexpr->string n) (if v "true" "false"))))
  (string-join
    parts
    ""
    #:before-first "{\n"
    #:after-last "}\n"))

(define (config-map names cfg)
  (for/hash ([n names] [v cfg]) (values (string->symbol n) v)))


(define (count-configs fm)
  (define n (feature-model-num-features fm))
  (define bdd (feature-model-bdd fm))
  (robdd-sat-count bdd n))

(define (nth-config fm idx)
  (define n (feature-model-num-features fm))
  (define bdd (feature-model-bdd fm))
  (check-range "config index" idx 0 (robdd-sat-count bdd n))
  (robdd-nth-sat bdd n idx))

(define (do-count-configs path)
  (define-values (fm names) (read-fmjson-from-file path))
  (displayln (count-configs fm)))

(define (do-nth-config path idx)
  (define-values (fm names) (read-fmjson-from-file path))
  (display (config->json-string names (nth-config fm idx))))

(define (do-random-config path)
  (define-values (fm names) (read-fmjson-from-file path))
  (display (config->json-string names (nth-config fm (random (count-configs fm))))))

(define (do-print-clafer path)
  (define j (call-with-input-file* path read-json))
  (display (fmjson->clafer j)))

(define (do-simplify path)
  (define-values (fm names) (read-fmjson-from-file path))
  (define fm2 (simplify fm))
  ; TODO - would be nice to preserve group names, but it's tricky because
  ; groups may be removed or modified during simplification.
  (define names2 (struct-copy name-list names [groups #f]))
  (define j (feature-model->fmjson names2 fm2))
  (write-json j))

(define (do-test-roundtrip-fmjson path)
  (define j (call-with-input-file* path read-json))
  (define-values (fm names) (fmjson->feature-model j))
  (define j2 (feature-model->fmjson names fm))
  (write-json j2))

(define (usage desc)
  (printf "usage: racket fmtool.rkt ~a ~a~n" (vector-ref args 0) desc)
  (exit 1))

(match args
  [`#("count-configs" ,path)
    (do-count-configs path)]
  [`#("count-configs" _ ...) (usage "<path>")]

  [`#("nth-config" ,path ,idx)
    (do-nth-config path (parse-int idx))]
  [`#("nth-config" _ ...) (usage "<path> <index>")]

  [`#("random-config" ,path)
    (do-random-config path)]
  [`#("random-config" _ ...) (usage "<path>")]

  [`#("print" ,path)
    (pretty-write (read-fmjson-from-file path))]
  [`#("print" _ ...) (usage "<path>")]

  [`#("print-clafer" ,path)
    (do-print-clafer path)]
  [`#("print-clafer" _ ...) (usage "<path>")]

  [`#("simplify" ,path)
    (do-simplify path)]
  [`#("simplify" _ ...) (usage "<path>")]

  [`#("test-roundtrip-fmjson" ,path)
    (do-test-roundtrip-fmjson path)]
  [`#("test-roundtrip-fmjson" _ ...) (usage "<path>")]

  [else
    (printf "usage: racket fmtool.rkt <subcommand...>~n")
    (exit 1)]
)
