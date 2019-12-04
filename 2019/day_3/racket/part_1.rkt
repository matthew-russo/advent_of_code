#lang racket/base

(require racket/match)
(require racket/string)
(require racket/set)
(require racket/stream)

(struct command (direction distance))

(define (parse-command input)
  (let* ([head (string-ref input 0)]
         [tail (substring input 1)]
         [direction (parse-direction head)]
         [distance (string->number tail)])
    (command direction distance)))

(define (parse-direction input)
  (match input
         [#\U 'UP]
         [#\D 'DOWN]
         [#\L 'LEFT]
         [#\R 'RIGHT]))

(define (execute-command pos command)
  (let ([row (car pos)]
        [col (cdr pos)])
    (match (command-direction command)
           ['UP    (move-up pos    (command-distance command))]
           ['DOWN  (move-down pos  (command-distance command))]
           ['LEFT  (move-left pos  (command-distance command))]
           ['RIGHT (move-right pos (command-distance command))])))

(define (move-up pos distance)
  (cons (car pos) (+ (cdr pos) distance)))

(define (move-down pos distance)
  (cons (car pos) (- (cdr pos) distance)))

(define (move-left pos distance)
  (cons (+ (car pos) distance) (cdr pos)))

(define (move-right pos distance)
  (cons (- (car pos) distance) (cdr pos)))

(define (points-between point-one point-two)
  (if (equal? (car point-one) (car point-two))
    (map (lambda (i) (cons (car point-one) i))
         (stream->list (in-range (min (cdr point-one) (cdr point-two)) (+ 1 (max (cdr point-one) (cdr point-two))))))
    (map (lambda (i) (cons i (cdr point-one)))
         (stream->list (in-range (min (car point-one) (car point-two)) (+ 1 (max (car point-one) (car point-two))))))))

(define (generate-full-paths-from-commands commands)
  (foldl (lambda (comm last-pos-and-all-points-visited)
           (let* ([last-pos (car last-pos-and-all-points-visited)]
                  [all-points (cdr last-pos-and-all-points-visited)]
                  [new-pos (execute-command last-pos comm)]
                  [new-points (points-between last-pos new-pos)])
             (cons new-pos (append all-points new-points))))
         (cons (cons 0 0) '())
         commands))

; manhattan-distanace = |x1 - x2| + |y1 - y2|
(define (manhattan-distance point-one point-two)
  (let ([x-diff (abs (- (car point-two) (car point-one)))]
        [y-diff (abs (- (cdr point-two) (cdr point-one)))])
    (+ x-diff y-diff)))

(define (read-input-to-commands)
  (let* ([all-commands-strings (string-split (read-line) ",")]
         [all-commands (map parse-command all-commands-strings)])
    all-commands))

(define (read-wire-paths)
  (cons (generate-full-paths-from-commands (read-input-to-commands))
        (generate-full-paths-from-commands (read-input-to-commands))))

(define (solution)
  (let* ([wire-paths (read-wire-paths)]
         [path-one (car wire-paths)]
         [path-two (cdr wire-paths)]
         [intersections (set-intersect path-one path-two)]
         [intersection-distances (map (lambda (point) (manhattan-distance (cons 0 0) point)) intersections)])
    (println "path-one")
    (println path-one)
    (println "path-two")
    (println path-two)
    (println "intersections")
    (println intersections)
    (println "min-distance")
    (println (min intersection-distances))))

(solution)
