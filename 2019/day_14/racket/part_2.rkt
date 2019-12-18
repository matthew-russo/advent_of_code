#lang racket

;; right side of equation is the hash node
;; left side is the list of values
;; build a graph and traverse from FUEL back to ORE,
;; each step will require multiplying by the number needed and dividing by the number produced

(struct node (id num requires)) ;; requires is a list of nodes

(define (node->string n)
  (string-append "node<id="
                 (node-id n)
                 ", num="
                 (number->string (node-num n))
                 ", requires=["
                 (string-join (map (lambda (r) (string-append "{id=" (car r) ", needed=" (number->string (cdr r)) "}")) (node-requires n)))
                 "]>"))

(define reserves (make-hash))

(define (repeater f count) (for ((i (in-range count))) (f)))

(define (traverse-node g n-id num-needed)
  (let* ([n (hash-ref g n-id)]
         [in-reserves  (hash-ref reserves n-id 0)]
         [actual-num-needed (- num-needed in-reserves)]
         [num-produced (node-num n)]
         [recipe-times (ceiling (/ actual-num-needed num-produced))]
         [total-num    (* num-produced recipe-times)]
         [rem          (- total-num actual-num-needed)])
    (if (<= actual-num-needed 0)
        (begin (hash-set! reserves n-id (- in-reserves num-needed))
               0)
        (begin (hash-set! reserves n-id rem)
               (apply + (map (lambda (c-id-count)
                               (let* ([c-id         (car c-id-count)]
                                      [c-num-needed (cdr c-id-count)])
                                 (if (equal? c-id "ORE")
                                     (let ([ore-left (hash-ref reserves "ORE")]
                                           [ore-needed (* recipe-times c-num-needed)])
                                       (if (< ore-left ore-needed)
                                           (error "out of ore")
                                           (begin (hash-set! reserves "ORE" (- ore-left ore-needed))
                                                  ore-needed)))
                                     (apply + (map (lambda (i) (traverse-node g c-id c-num-needed)) (stream->list (in-range recipe-times)))))))
                             (node-requires n)))))))
      
(define (parse-ingredient ing)
  (let* ([num-id (string-split ing)]
         [num    (string->number (list-ref num-id 0))]
         [id     (list-ref num-id 1)])
    (cons id num)))

(define (parse-line l)
  (let* ([lhs-rhs (string-split l " => ")]
         [requires-strs (string-split (list-ref lhs-rhs 0) ", ")]
         [requires (map parse-ingredient requires-strs)]
         [produces-ing (parse-ingredient (list-ref lhs-rhs 1))])
    (node (car produces-ing) (cdr produces-ing) requires)))

(define (read-until-eof lines)
  (let ([line (read-line)])
    (if (eof-object? line)
        lines
        (read-until-eof (append lines (list line))))))

(define (parse-input)
  (let* ([lines (read-until-eof (list))])
    (foldl (lambda (n g) (hash-set g (node-id n) n))
           (make-immutable-hash)
           (map parse-line lines))))

(define num-fuel 0)

(define (traverse-forever graph)
  ; (println (string-append "beginning traversal with " (number->string (hash-ref reserves "ORE")) " ORE left"))
  (traverse-node graph "FUEL" 1)
  (set! num-fuel (+ num-fuel 1))
  (traverse-forever graph))

(define (solution)
  (let ([graph (parse-input)])
    (hash-set! reserves "ORE" 1000000000000)
    (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
      (traverse-forever graph))
    (println (number->string num-fuel))))

(solution)
