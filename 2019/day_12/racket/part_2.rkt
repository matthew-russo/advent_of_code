#lang racket

(define (vec-3=? v1 v2 recursive-equal?)
  (and (= (vec-3-x v1) (vec-3-x v2))
       (= (vec-3-y v1) (vec-3-y v2))
       (= (vec-3-z v1) (vec-3-z v2))))

(define (vec-3-hash-1 v recursive-equal-hash)
  (apply + (map (lambda (x) (* 31 x))
                (list (recursive-equal-hash (vec-3-x v))
                      (recursive-equal-hash (vec-3-y v))
                      (recursive-equal-hash (vec-3-z v))))))

(define (vec-3-hash-2 v recursive-equal-hash)
  (apply + (map (lambda (x) (* 31 x))
                (list (recursive-equal-hash (vec-3-x v))
                      (recursive-equal-hash (vec-3-y v))
                      (recursive-equal-hash (vec-3-z v))))))

(struct vec-3 (x y z)
        #:methods gen:equal+hash
        [(define equal-proc vec-3=?)
         (define hash-proc  vec-3-hash-1)
         (define hash2-proc vec-3-hash-2)])

(define (vec-3-zero) (vec-3 0 0 0))

(define (vec-3-dim v dim)
  (cond
    [(equal? dim 'x) (vec-3-x v)]
    [(equal? dim 'y) (vec-3-y v)]
    [(equal? dim 'z) (vec-3-z v)]))

(define (vec-3-change-dim v dim new-value)
  (let ([x (vec-3-x v)]
        [y (vec-3-y v)]
        [z (vec-3-z v)])
    (cond
      [(equal? dim 'x) (vec-3 new-value y z)]
      [(equal? dim 'y) (vec-3 x new-value z)]
      [(equal? dim 'z) (vec-3 x y new-value)])))


(define (vec-3-abs-sum vec3)
  (+ (abs (vec-3-x vec3))
     (abs (vec-3-y vec3))
     (abs (vec-3-z vec3))))

(define (vec-3->string v)
  (let ([x-str (number->string (vec-3-x v))]
        [y-str (number->string (vec-3-y v))]
        [z-str (number->string (vec-3-z v))])
    (string-append "<x=" x-str ", y=" y-str ", z=" z-str ">")))
 
(define (moon=? m1 m2 recursive-equal?)
  (and (recursive-equal? (moon-name m1) (moon-name m2))
       (recursive-equal? (moon-pos m1) (moon-pos m2))
       (recursive-equal? (moon-vel m1) (moon-vel m2))))

(define (moon-hash-1 m recursive-equal-hash)
  (apply + (map (lambda (x) (* 31 x))
                (list (recursive-equal-hash (moon-name m))
                      (recursive-equal-hash (moon-pos m))
                      (recursive-equal-hash (moon-vel m))))))

(define (moon-hash-2 m recursive-equal-hash)
  (apply + (map (lambda (x) (* 31 x))
                (list (recursive-equal-hash (moon-name m))
                      (recursive-equal-hash (moon-pos m))
                      (recursive-equal-hash (moon-vel m))))))

(struct moon (name pos vel)
        #:methods gen:equal+hash
        [(define equal-proc moon=?)
         (define hash-proc  moon-hash-1)
         (define hash2-proc moon-hash-2)])

(define (potential-energy m)
  (vec-3-abs-sum (moon-pos m)))

(define (kinetic-energy m)
  (vec-3-abs-sum (moon-vel m)))

(define (total-energy m)
  (* (potential-energy m) (kinetic-energy m)))

(define (moon->string m)
  (string-append "<name=" (moon-name m) ", pos=" (vec-3->string (moon-pos m)) ", vel=" (vec-3->string (moon-vel m)) ">"))

(define (how-much-to-add pos1 pos2 dim)
  (let ([pos1-dim (vec-3-dim pos1 dim)]
        [pos2-dim (vec-3-dim pos2 dim)])
    (cond 
      [(< pos1-dim pos2-dim)      (cons 1 -1)]
      [(> pos1-dim pos2-dim)      (cons -1 1)]
      [(equal? pos1-dim pos2-dim) (cons 0 0)])))

(define (apply-gravity-between m1 m2 dim)
  (let* ([to-add (how-much-to-add (moon-pos m1) (moon-pos m2) dim)]
         [m1-dim-vel (+ (vec-3-dim (moon-vel m1) dim) (car to-add))]
         [m2-dim-vel (+ (vec-3-dim (moon-vel m2) dim) (cdr to-add))]
         [new-m1-vel (vec-3-change-dim (moon-vel m1) dim m1-dim-vel)]
         [new-m2-vel (vec-3-change-dim (moon-vel m2) dim m2-dim-vel)]
         [new-m1     (moon (moon-name m1) (moon-pos m1) new-m1-vel)]
         [new-m2     (moon (moon-name m2) (moon-pos m2) new-m2-vel)])
    (cons new-m1 new-m2)))

(define (apply-velocity-1-dim m dim)
  (let* ([dim-vel (vec-3-dim (moon-vel m) dim)]
         [old-pos (vec-3-dim (moon-pos m) dim)]
         [new-pos (vec-3-change-dim (moon-pos m) dim (+ dim-vel old-pos))])
    (moon (moon-name m) new-pos (moon-vel m))))

(define (apply-gravity-1-dim moons dim)
  (hash-values (foldl (lambda (m1-m2 moon-hash)
                        (let* ([m1 (hash-ref moon-hash (moon-name (list-ref m1-m2 0)) (list-ref m1-m2 0))]
                               [m2 (hash-ref moon-hash (moon-name (list-ref m1-m2 1)) (list-ref m1-m2 1))]
                               [new-m1-m2 (apply-gravity-between m1 m2 dim)])
                          (hash-set! moon-hash (moon-name (car new-m1-m2)) (car new-m1-m2))
                          (hash-set! moon-hash (moon-name (cdr new-m1-m2)) (cdr new-m1-m2))
                          moon-hash))
                      (make-hash)
                      (combinations moons 2))))

(define (find-cycle-1-dim moons dim seen-states num-steps)
  (let* ([with-grav-applied (apply-gravity-1-dim moons dim)]
         [with-vel-applied  (map (lambda (m) (apply-velocity-1-dim m dim)) with-grav-applied)]
         [next-steps        (+ num-steps 1)]
         [next-state        (hash-set seen-states with-vel-applied #t)])
    (if (hash-has-key? seen-states with-vel-applied)
        num-steps
        (find-cycle-1-dim with-vel-applied dim next-state next-steps))))

(define (find-cycle-all-dims moons)
  (let ([x-cycle-length (begin (println "X") (find-cycle-1-dim moons 'x (make-immutable-hash) 0))]
        [y-cycle-length (begin (println "Y") (find-cycle-1-dim moons 'y (make-immutable-hash) 0))]
        [z-cycle-length (begin (println "Z") (find-cycle-1-dim moons 'z (make-immutable-hash) 0))])
    (println x-cycle-length)
    (println y-cycle-length)
    (println z-cycle-length)
    (lcm x-cycle-length y-cycle-length z-cycle-length)))

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
         [result        (find-cycle-all-dims initial-moons)])
    (println (number->string result))))

(solution)
