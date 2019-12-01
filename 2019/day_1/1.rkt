;; Fuel required to launch a given module is based on its mass.
;; Specifically, to find the fuel required for a module:
;; take its mass, divide by three, round down, and subtract 2.

;; usage: `cat 1.input | 1.rkt`

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
  (- (floor (/ mass 3)) 2))

(solution)
