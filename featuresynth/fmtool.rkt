#lang racket

(require json)
(require threading)
(require bdd/robdd)
(require racket/random)
(require "types.rkt")
(require "util.rkt")
(require "eval.rkt")
(require "fmjson.rkt")
(require "sample.rkt")
(require "simplify.rkt")
(require "check_sat.rkt")

(define (collect-json-fm-values jss)
  (define (unzip-and-accum js fm-values)
    (define-values (fm names) (fmjson->feature-model js))
    (cons (cons fm    (car fm-values))
          (cons names (cdr fm-values))))
  (define fm-values (foldr unzip-and-accum (cons '() '()) jss))
  (values (car fm-values) (cdr fm-values)))

(define (read-fmjson-from-file path)
  ; TODO run `clafer -m fmjson` if file extension is .cfr
  (define j (call-with-input-file* path read-json))
  (if (list? j)
      (collect-json-fm-values j)
      (fmjson->feature-model j)))

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
    (for/list ([n (name-list-features names)] [v cfg])
      (format "  ~a: ~a" (jsexpr->string (name-str n)) (if v "true" "false"))))
  (string-join
    parts
    ",\n"
    #:before-first "{\n"
    #:after-last "\n}\n"))

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

(define (lone-config fm)
  (define n (feature-model-num-features fm))
  (define bdd (feature-model-bdd fm))
  (define sat-count (robdd-sat-count bdd n))
  (unless (= 1 sat-count)
    (raise
      (format "expected feature model to have exactly 1 configuration, but it has ~a"
              sat-count)))
  (robdd-nth-sat bdd n 0))

(define (do-count-configs path)
  (define-values (fm names) (read-fmjson-from-file path))
  (displayln (count-configs fm)))

(define (do-check-sat path)
  (define-values (fm names) (read-fmjson-from-file path))
  (let ((line (if (list? fm)
                  (map check-sat fm)
                  (check-sat fm))))
    (displayln (jsexpr->string line))))

(define (do-check-req path fs)
  (define-values (fm names) (read-fmjson-from-file path))
  (displayln (jsexpr->string (check-sat-must fm names fs))))

(define (do-nth-config path idx)
  (define-values (fm names) (read-fmjson-from-file path))
  (display (config->json-string names (nth-config fm idx))))

(define (do-random-config path)
  (define-values (fm names) (read-fmjson-from-file path))
  (display (config->json-string names (nth-config fm (random (count-configs fm))))))

(define (list->json-list lst)
  (string-join lst
               ",\n"
               #:before-first "[\n"
               #:after-last "]\n"))

(define (configs-by-idx-list fm names idxs)
  (let ((nth-cfg (lambda (i) (config->json-string names (nth-config fm i)))))
    (list->json-list (map nth-cfg idxs))))

(define (do-n-random-configs path n)
  (define-values (fm names) (read-fmjson-from-file path))
  (let ((idxs (random-sample (range (count-configs fm))
                             n)))
    (display (configs-by-idx-list fm names idxs))))

(define (do-all-configs path)
  (define-values (fm names) (read-fmjson-from-file path))
  (let ((idxs (range 0 (count-configs fm))))
    (display (configs-by-idx-list fm names idxs))))

(define (do-print-clafer path)
  (define j (call-with-input-file* path read-json))
  (display (fmjson->clafer j)))

(define (do-list-enabled path)
  (define-values (fm names) (read-fmjson-from-file path))
  (define cfg (lone-config fm))
  (for ([n (name-list-features names)] [v cfg] #:when v)
    (printf "~a~n" (name-str n))))

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

(define (usage args desc)
  (printf "usage: racket fmtool.rkt ~a ~a~n" (vector-ref args 0) desc)
  (exit 1))

(module+ main
  (let ((args (current-command-line-arguments)))
  (match args
    [`#("count-configs" ,path)
      (do-count-configs path)]
    [`#("count-configs" _ ...) (usage args "<path>")]

    [`#("check-sat" ,path)
     (do-check-sat path)]
    [`#("check-sat" _ ...) (usage args "<path>")]

    [`#("check-req" ,path ,fs ...)
     (do-check-req path fs)]
    [`#("check-req" _ ...) (usage args "<path>")]

    [`#("all-configs" ,path)
     (do-all-configs path)]
    [`#("all-configs" _ ...) (usage args "<path>")]
  
    [`#("nth-config" ,path ,idx)
      (do-nth-config path (parse-int idx))]
    [`#("nth-config" _ ...) (usage args "<path> <index>")]
  
    [`#("random-config" ,path)
      (do-random-config path)]
    [`#("random-config" ,path ,num)
     (do-n-random-configs path (parse-int num))]
    [`#("random-config" _ ...) (usage args "<path> [num]")]
  
    [`#("print" ,path)
      (pretty-write (read-fmjson-from-file path))]
    [`#("print" _ ...) (usage args "<path>")]
  
    [`#("print-clafer" ,path)
      (do-print-clafer path)]
    [`#("print-clafer" _ ...) (usage args "<path>")]
  
    [`#("list-enabled" ,path)
      (do-list-enabled path)]
    [`#("list-enabled" _ ...) (usage args "<path>")]
  
    [`#("simplify" ,path)
      (do-simplify path)]
    [`#("simplify" _ ...) (usage args "<path>")]
  
    [`#("test-roundtrip-fmjson" ,path)
      (do-test-roundtrip-fmjson path)]
    [`#("test-roundtrip-fmjson" _ ...) (usage args "<path>")]
  
    [else
      (printf "usage args: racket fmtool.rkt <subcommand...>~n")
      (exit 1)
    ]
  ))
)
