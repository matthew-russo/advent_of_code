#lang racket

(define base-pattern '(0 1 0 -1))

(define (repeat-each-el lst n)
  (flatten (map (lambda (i) (make-list n i)) lst)))

(define (repeat-list-to-length lst len)
  (if (>= (length lst) len)
      (take lst len)
      (let* ([times-to-repeat (floor (/ len (length lst)))]
             [to-take         (remainder len (length lst))]
             [head (foldl (lambda (i res) (append res lst)) lst (stream->list (in-range 1 times-to-repeat)))]
             [tail (take lst to-take)]
             [result (append head tail)])
        result)))

(define (find-output-el src el-index)
  (let* ([pattern (repeat-each-el base-pattern el-index)]
         [repeat-pattern (repeat-list-to-length pattern (+ 1 (length src)))]
         [out-lst (map * src (cdr repeat-pattern))]
         [sum (apply + out-lst)])
  (abs (remainder sum 10))))

(define (transform src)
  (let ([n-list (stream->list (in-range 1 (+ (length src) 1)))])
    (map (lambda (el-index) (find-output-el src el-index)) n-list)))

(define (run-phases-n-times input n)
  (let ([n-list (stream->list (in-range n))])
    (foldl (lambda (i res) (transform res))
           input
           n-list)))

(define (get-input)
  (let* ([num-str  (read-line)]
         [char-lst (string->list num-str)]
         [str-lst  (map string char-lst)]
         [input (map string->number str-lst)])
    input))

(define (solution)
  (let* ([input  (get-input)]
         [output (run-phases-n-times input 100)]
         [first-8-digits (take output 8)])
    (println first-8-digits)))

(solution)
