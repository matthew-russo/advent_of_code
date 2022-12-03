#lang racket

(define base-pattern '(0 1 0 -1))

(define (repeat-each-el lst n)
  (flatten (map (lambda (i) (make-list n i)) lst)))

(define (repeat-list-n-times lst n)
  (let ([n-list (stream->list (in-range 1 n))])
    (foldl (lambda (i res) (append res lst)) lst n-list)))

(define (repeat-list-to-length lst len)
  (if (>= (length lst) len)
      (take lst len)
      (let* ([times-to-repeat (floor (/ len (length lst)))]
             [to-take         (remainder len (length lst))]
             [head            (repeat-list-n-times lst times-to-repeat)]
             [tail (take lst to-take)]
             [result (append head tail)])
        result)))

(define (find-output-el src el-index n-list)
  (let* ([out-lst (map (lambda (x i) (* x (get-pattern-index el-index i))) src n-list)]
         [sum (apply + out-lst)])
  (abs (remainder sum 10))))

(define (transform src n-list-0 n-list-1)
  (map (lambda (el-index)
         (find-output-el src el-index n-list-0))
       n-list-1))

(define (run-phases-n-times input n)
  (let ([n-list-0 (stream->list (in-range length input))]
        [n-list-1 (stream->list (in-range 1 (+ (length input) 1)))]
        [run-list (stream->list (in-range n))])
    (println "beginning the run process")
    (foldl (lambda (run-iter res)
             (begin
               (println (string-append "iter num: " (number->string run-iter)))
               (transform res n-list-0 n-list-1)))
           input
           run-list)))

(define (get-input)
  (let* ([num-str  (read-line)]
         [char-lst (string->list num-str)]
         [str-lst  (map string char-lst)]
         [input    (map string->number str-lst)])
    input))

(define (solution)
  (let* ([input          (get-input)]
         [signal         (repeat-list-n-times input 10000)]
         [message-offset (string->number (string-join (map number->string (take input 7)) ""))]
         [output         (run-phases-n-times signal 100)]
         [result         (list first second third fourth fifth sixth seventh eighth)])
    (println result)))

(define (get-pattern-index repeat-count n)
  (let* ([repeat-pattern (repeat-each-el base-pattern repeat-count)]
         [index-group    (floor (/ n (length repeat-pattern)))]
         [actual-index   (remainder n (length repeat-pattern))])
    (list-ref repeat-pattern actual-index)))

(solution)


