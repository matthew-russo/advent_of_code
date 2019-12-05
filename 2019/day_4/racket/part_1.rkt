
;; a few key facts about the password:

;;  -  It is a six-digit number.
;;  -  The value is within the range given in your puzzle input.
;;  -  Two adjacent digits are the same (like 22 in 122345).
;;  -  Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

#lang racket/base

(require racket/list)
(require racket/stream)

(define (lower-bound) 178416)
(define (upper-bound) 676461)

(define (is-password? number)
  (and (two-adjacent-digits-same? number)
       (digits-never-decrease? number)))

(define (two-adjacent-digits-same? number)
  (let ([char-digit-list (string->list (number->string number))])
    (if (< (length char-digit-list) 2)
      #f
      (let ([first  (first char-digit-list)]
            [second (second char-digit-list)])
        (if (equal? first second)
            #t
            (if (equal? (length char-digit-list) 2)
                #f
                (two-adjacent-digits-same? (string->number (list->string (append (list second) (list-tail char-digit-list 2)))))))))))

(define (digits-never-decrease? number)
  (let ([char-digit-list (string->list (number->string number))])
    (if (< (length char-digit-list) 2)
      #t
      (let ([first  (first char-digit-list)]
            [second (second char-digit-list)])
        (if (char>? first second)
            #f
            (if (equal? (length char-digit-list) 2)
                #t
                (digits-never-decrease? (string->number (list->string (append (list second) (list-tail char-digit-list 2)))))))))))

(define (solution)
  (let* ([password-range (stream->list (in-range (lower-bound) (upper-bound)))]
         [valid-passwords (filter is-password? password-range)])
    (println valid-passwords)
    (println (length valid-passwords))))

(solution)
