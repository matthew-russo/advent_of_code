;; Fuel required to launch a given module is based on its mass.
;; Specifically, to find the fuel required for a module:
;; take its mass, divide by three, round down, and subtract 2.

;; usage: `cat input.txt | 2.rkt`

#lang racket/base

(define (solution)
  (let* ([fuel-list (generate-required-fuel-list (list))]
        [fuel-sum (foldl + 0 fuel-list)])
    (println fuel-sum)))

(define (generate-required-fuel-list current-list)
  (let ([mass-str (read-line)])
    (if (equal? mass-str eof)
        current-list
        (let* ([mass (string->number mass-str)]
              [required-fuel (calculate-required-fuel mass)]
              [new-list (append current-list (list required-fuel))])
          (generate-required-fuel-list new-list)))))

(define (calculate-required-fuel mass)
  (let* ([fuel-mass (- (floor (/ mass 3)) 2)]
         [divide-by-three (/ mass 3)]
         [floored (floor divide-by-three)]
         [fuel-mass (- floored 2)])
    (if (< fuel-mass 0)
        0
        (+ fuel-mass (calculate-required-fuel fuel-mass)))))

(solution)
