#lang racket

(define (slope-and-quadrant point-one point-two)
  (list (slope-of point-one point-two) (quadrant-of point-one point-two)))

(define (slope-of point-one point-two)
  (let ([y-diff (- (cdr point-two) (cdr point-one))]
        [x-diff (- (car point-two) (car point-one))])
    (if (equal? x-diff 0) 'divide-by-zero (/ y-diff x-diff))))

; (1, 2) -> ( 3,  7) = ('+ '+)
; (1, 2) -> (-3,  7) = ('+ '-)
; (1, 2) -> (-3, -7) = ('- '-)
; (1, 2) -> ( 3, -7) = ('- '+)
(define (quadrant-of point-one point-two)
  (let* ([y-diff (- (cdr point-two) (cdr point-one))]
         [x-diff (- (car point-two) (car point-one))]
         [y-sym (if (< y-diff 0) '- '+)]
         [x-sym (if (< x-diff 0) '- '+)])
    (list x-sym y-sym)))

(define (num-visible-asteroids grid src-pos src-occupied-by)
  (if (eq? src-occupied-by 'asteroid)
      (let ([visible-asteroids (make-hash)])
        (hash-for-each grid (lambda (dst-pos dst-occupied-by)
                              (if (and (not (equal? src-pos dst-pos)) (eq? dst-occupied-by 'asteroid))
                                  (hash-set! visible-asteroids (slope-and-quadrant src-pos dst-pos) dst-pos)
                                  (void))))
        (hash-count visible-asteroids))
      0))

(define (find-best-asteroid grid)
  (let* ([max-count 0]
         [max-pos   (list -1 -1)])
    (hash-for-each grid
                   (lambda (pos occupied-by)
                     (cond [(eq? occupied-by 'asteroid)
                            (let ([num-visible (num-visible-asteroids grid pos occupied-by)])
                              (cond [(> num-visible max-count)
                                     (begin (set! max-count num-visible) (set! max-pos pos))]))])))
    (println max-pos)
    (println max-count)))

(define (read-until-eof lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        lines
        (read-until-eof (append lines (list line))))))


;; to-insert :: [[((x, y), sym)]]
(define (parse-input)
  (let* ([lines     (read-until-eof (list))]
         [n-list    (stream->list (in-range (length lines)))]
         [to-insert (apply append (map (lambda (i line) (parse-line i line)) n-list lines))]
         [grid      (make-hash)])
    (map (lambda (kvp) (hash-set! grid (car kvp) (cdr kvp))) to-insert)
    grid))

(define (char-to-occupied-by c)
  (match c 
         [#\. 'empty]
         [#\# 'asteroid]
         [_   'unknown]))

;; String -> [((x,y), sym)]
(define (parse-line height line)
  (let* ([chars (string->list line)]
         [n-list (stream->list (in-range (length chars)))])
    (map (lambda (width c) (cons (cons width height) (char-to-occupied-by c)))
         n-list
         chars)))

(define (solution)
  (let ([grid (parse-input)])
    (find-best-asteroid grid)))

(solution)

