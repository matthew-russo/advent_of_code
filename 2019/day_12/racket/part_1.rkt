#lang racket

(struct vec-3 (x y z))

(define (vec-3-zero) (vec-3 0 0 0))

(define (vec-3-abs-sum vec3)
  (+ (abs (vec-3-x vec3))
     (abs (vec-3-y vec3))
     (abs (vec-3-z vec3))))

(define (vec-3->string v)
  (let ([x-str (number->string (vec-3-x v))]
        [y-str (number->string (vec-3-y v))]
        [z-str (number->string (vec-3-z v))])
    (string-append "<x=" x-str ", y=" y-str ", z=" z-str ">")))
  
(struct moon (name pos vel))

(define (potential-energy m)
  (vec-3-abs-sum (moon-pos m)))

(define (kinetic-energy m)
  (vec-3-abs-sum (moon-vel m)))

(define (total-energy m)
  (* (potential-energy m) (kinetic-energy m)))

(define (apply-velocity moons)
  (map (lambda (m) (let* ([pos (moon-pos m)]
                          [vel (moon-vel m)]
                          [pos-x (vec-3-x pos)]
                          [pos-y (vec-3-y pos)]
                          [pos-z (vec-3-z pos)]
                          [vel-x (vec-3-x vel)]
                          [vel-y (vec-3-y vel)]
                          [vel-z (vec-3-z vel)]
                          [new-pos (vec-3 (+ pos-x vel-x) (+ pos-y vel-y) (+ pos-z vel-z))])
                     (moon (moon-name m) new-pos (moon-vel m))))
       moons))

(define (how-much-to-add p1 p2)
  (cond 
    [(< p1 p2)      (cons 1 -1)]
    [(> p1 p2)      (cons -1 1)]
    [(equal? p1 p2) (cons 0 0)]))

(define (apply-gravity moons)
  (hash-values (foldl (lambda (p1-p2 moon-hash)
                        (let* ([p1 (hash-ref moon-hash (moon-name (list-ref p1-p2 0)) (list-ref p1-p2 0))]
                               [p2 (hash-ref moon-hash (moon-name (list-ref p1-p2 1)) (list-ref p1-p2 1))]
                               [p1-pos (moon-pos p1)]
                               [p2-pos (moon-pos p2)]
                               [p1-vel (moon-vel p1)]
                               [p2-vel (moon-vel p2)]
                               [x-to-add (how-much-to-add (vec-3-x p1-pos) (vec-3-x p2-pos))]
                               [p1-x-vel (+ (vec-3-x p1-vel) (car x-to-add))]
                               [p2-x-vel (+ (vec-3-x p2-vel) (cdr x-to-add))]
                               [y-to-add (how-much-to-add (vec-3-y p1-pos) (vec-3-y p2-pos))]
                               [p1-y-vel (+ (vec-3-y p1-vel) (car y-to-add))]
                               [p2-y-vel (+ (vec-3-y p2-vel) (cdr y-to-add))]
                               [z-to-add (how-much-to-add (vec-3-z p1-pos) (vec-3-z p2-pos))]
                               [p1-z-vel (+ (vec-3-z p1-vel) (car z-to-add))]
                               [p2-z-vel (+ (vec-3-z p2-vel) (cdr z-to-add))]
                               [new-p1-vel (vec-3 p1-x-vel p1-y-vel p1-z-vel)]
                               [new-p2-vel (vec-3 p2-x-vel p2-y-vel p2-z-vel)]
                               [new-p1 (moon (moon-name p1) (moon-pos p1) new-p1-vel)]
                               [new-p2 (moon (moon-name p2) (moon-pos p2) new-p2-vel)])
                          (hash-set! moon-hash (moon-name p1) new-p1)
                          (hash-set! moon-hash (moon-name p2) new-p2)
                          moon-hash))
                      (make-hash)
                      (combinations moons 2))))

(define (step moons)
  (let* ([moons-with-new-vels (apply-gravity moons)]
         [moons-with-new-pos (apply-velocity moons-with-new-vels)])
    moons-with-new-pos))

(define (step-n-times initial-moons n)
  (foldl (lambda (i moons)
           (step moons))
         initial-moons
         (stream->list (in-range n))))

(define (parse-input)
  (let ([io (parse-line "io")]
        [europa (parse-line "europa")]
        [ganymede (parse-line "ganymede")]
        [callisto (parse-line "callisto")])
    (list io europa ganymede callisto)))

(define (parse-line name)
  (let* ([line      (read-line)]
         [reg-match (regexp-match #rx"<x=([-]?[0-9]+), y=([-]?[0-9]+), z=([-]?[0-9]+)>" line)]
         [x (string->number (list-ref reg-match 1))]
         [y (string->number (list-ref reg-match 2))]
         [z (string->number (list-ref reg-match 3))])
    (moon name (vec-3 x y z) (vec-3-zero))))

(define (solution)
  (let* ([initial-moons (parse-input)]
         [final-moons   (step-n-times initial-moons 1000)]
         [all-energies  (map total-energy final-moons)]
         [total         (apply + all-energies)])
    (println (number->string total))))

(solution)
