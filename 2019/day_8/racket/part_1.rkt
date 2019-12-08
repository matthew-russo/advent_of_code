#lang racket

(define (pic-width)  25)
(define (pic-height) 6)

(define (chunks-of lst chunk-size)
  (if (empty? lst)
      '()
      (let* ([actual-size (min (length lst) chunk-size)]
             [chunk       (take lst actual-size)]
             [rest        (drop lst actual-size)])
        (cons chunk (chunks-of rest chunk-size)))))

(define (count-of-n-in layer n)
  (length (filter (lambda (x) (equal? n x)) layer)))

(define (solution)
  (let* ([input-str                  (map string (string->list (read-line)))]
         [input-nums                 (map string->number input-str)]
         [layer-size                 (* (pic-width) (pic-height))]
         [layers                     (chunks-of input-nums layer-size)]
         [zero-counts                (map (lambda (l) (count-of-n-in l 0)) layers)]
         [smallest-num-zeroes        (apply min zero-counts)]
         [layer-with-smallest-zeroes (car (filter (lambda (l) (equal? (count-of-n-in l 0) smallest-num-zeroes)) layers))]
         [one-counts                 (count-of-n-in layer-with-smallest-zeroes 1)]
         [two-counts                 (count-of-n-in layer-with-smallest-zeroes 2)])
    (println (number->string (* one-counts two-counts)))))

(solution)

