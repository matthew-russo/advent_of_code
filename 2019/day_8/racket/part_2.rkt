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

;; '((1 2 3 4) (5 6 7 8))
;; '((1 5) (2 6) (3 7) (4 8))
(define (transpose lst-of-lsts)
  (apply map list lst-of-lsts))

;; 0 black
;; 1 white
;; 2 transparent
(define (get-color color-lst)
  (let ([head (car color-lst)]
        [tail (cdr color-lst)]) 
    (if (equal? head 2)
        (get-color tail)
        head)))

(define (solution)
  (let* ([input-str                  (map string (string->list (read-line)))]
         [input-nums                 (map string->number input-str)]
         [layer-size                 (* (pic-width) (pic-height))]
         [layers                     (chunks-of input-nums layer-size)]
         [transposed-layers          (transpose layers)]
         [final-layer                (map get-color transposed-layers)]
         [final-layer-chunks         (chunks-of final-layer (pic-width))])
    (for-each println final-layer-chunks)))

(solution)

