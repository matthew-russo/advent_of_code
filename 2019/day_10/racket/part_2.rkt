#lang racket

; d = sqrt(pow(x2 - x1) + pow(y2-y1))
(define (distance-between point-one point-two)
  (let ([y-diff (- (cdr point-two) (cdr point-one))]
        [x-diff (- (car point-two) (car point-one))])
    (sqrt (+ (expt x-diff 2) (expt y-diff 2)))))

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

(define (visible-asteroids grid src-pos)
  (let ([visible-asteroids (make-hash)])
    (hash-for-each grid (lambda (dst-pos dst-occupied-by)
                          (if (and (not (equal? src-pos dst-pos)) (eq? dst-occupied-by 'asteroid))
                              (let* ([slope-and-quad (slope-and-quadrant src-pos dst-pos)]
                                     [previous (hash-ref visible-asteroids slope-and-quad (lambda () 'nothing))])
                                (if (not (equal? 'nothing previous))
                                    (if (< (distance-between src-pos dst-pos) (distance-between src-pos previous))
                                        (hash-set! visible-asteroids slope-and-quad dst-pos)
                                        (void))
                                    (hash-set! visible-asteroids slope-and-quad dst-pos)))
                              (void))))
    (hash-values visible-asteroids)))

(define (run-laser grid src-pos num-destroyed desired-num-destroyed)
  (let* ([visible (visible-asteroids grid src-pos)]
         [num-to-be-destroyed (+ (length visible) num-destroyed)])
    (if (>= num-to-be-destroyed desired-num-destroyed)
        (sort-and-find-nth src-pos num-destroyed desired-num-destroyed visible)
        (begin
          (for-each (lambda (pos) (hash-set! grid pos 'empty)) visible)
          (run-laser grid src-pos num-to-be-destroyed desired-num-destroyed)))))

(define (sort-and-find-nth src-pos num-destroyed desired-num-destroyed candidates)
  (let ([num-to-destroy (- desired-num-destroyed num-destroyed)]
        [sorted (sort candidates (lambda (x y) (clockwise-comparator src-pos x y)))])
    (list-ref sorted (- num-to-destroy 1))))

(define (clockwise-comparator center-pos point-one point-two)
  (let ([center-x (car center-pos)]
        [center-y (cdr center-pos)]
        [x-one (car point-one)]
        [x-two (car point-two)]
        [y-one (cdr point-one)]
        [y-two (cdr point-two)])
    (cond [(and (>= (- x-one center-x) 0) (< (- x-two center-x) 0)) #t]
          [(and (< (- x-one center-x) 0) (>= (- x-two center-x) 0)) #f]
          [(and (equal? (- x-one center-x) 0) (equal? (- x-two center-x) 0)) (> y-one y-two)]
          [else (let ([det (- (* (- x-one center-x) (- center-y y-two)) (* (- x-two center-x) (- center-y y-one)))])
                  (cond [(< det 0) #t]
                        [(>= det 0) #f]))])))

(define (read-until-eof lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        lines
        (read-until-eof (append lines (list line))))))


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

(define (parse-line height line)
  (let* ([chars (string->list line)]
         [n-list (stream->list (in-range (length chars)))])
    (map (lambda (width c) (cons (cons width height) (char-to-occupied-by c)))
         n-list
         chars)))

(define (solution)
  (let ([grid (parse-input)])
    (run-laser grid (cons 22 25) 0 200)))

(solution)

