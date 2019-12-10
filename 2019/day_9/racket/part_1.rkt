#lang racket

(require racket/list)
(require racket/match)
(require racket/string)
(require racket/stream)

(struct interpreter (in out src pc relative-base))
(struct opcode (instruction arguments))
(struct argument (param-mode value))

(define (run-interpreter interp)
  (let* ([opcode      (parse-opcode interp)]
         [instruction (opcode-instruction opcode)]
         [arguments   (opcode-arguments opcode)])
    (match (opcode-instruction opcode)
         ['addition             (run-interpreter (interpret-addition interp arguments))]
         ['multiplication       (run-interpreter (interpret-multiplication interp arguments))]
         ['input                (run-interpreter (interpret-input interp arguments))]
         ['output               (run-interpreter (interpret-output interp arguments))]
         ['jump-if-true         (run-interpreter (interpret-jump-if-true interp arguments))]
         ['jump-if-false        (run-interpreter (interpret-jump-if-false interp arguments))]
         ['less-than            (run-interpreter (interpret-less-than interp arguments))]
         ['equals               (run-interpreter (interpret-equals interp arguments))]
         ['adjust-relative-base (run-interpreter (interpret-adjust-relative-base interp arguments))]
         ['exit                 interp]
         ['unknown              (println (string-append "got unknown opcode: "
                                                        (number->string (at-pc interp))
                                                        " at index: "
                                                        (number->string (interpreter-pc interp))))
                                interp])))

(define (parse-opcode interp)
  (let* ([instruction (parse-instruction interp)]
         [arguments   (parse-arguments interp instruction)])
    (opcode instruction arguments)))

(define (parse-instruction interp)
  (match (remainder (at-pc interp) 100)
         [1  'addition]
         [2  'multiplication]
         [3  'input]
         [4  'output]
         [5  'jump-if-true]
         [6  'jump-if-false]
         [7  'less-than]
         [8  'equals]
         [9  'adjust-relative-base]
         [99 'exit]
         [_  'unknown]))

(define (parse-arguments interp instruction)
  (let* ([param-modes-num (truncate (/ (at-pc interp) 100))]
         [arg-values (match instruction
                          ['addition             (next-n-args interp 3)]
                          ['multiplication       (next-n-args interp 3)]
                          ['input                (next-n-args interp 1)]
                          ['output               (next-n-args interp 1)]
                          ['jump-if-true         (next-n-args interp 2)]
                          ['jump-if-false        (next-n-args interp 2)]
                          ['less-than            (next-n-args interp 3)]
                          ['equals               (next-n-args interp 3)]
                          ['adjust-relative-base (next-n-args interp 1)]
                          ['exit                 '()]
                          [_                     '()])]
         [parameter-modes (match instruction
                                ['addition             (next-n-param-modes param-modes-num 3)]
                                ['multiplication       (next-n-param-modes param-modes-num 3)]
                                ['input                (next-n-param-modes param-modes-num 1)]
                                ['output               (next-n-param-modes param-modes-num 1)]
                                ['jump-if-true         (next-n-param-modes param-modes-num 2)]
                                ['jump-if-false        (next-n-param-modes param-modes-num 2)]
                                ['less-than            (next-n-param-modes param-modes-num 3)]
                                ['equals               (next-n-param-modes param-modes-num 3)]
                                ['adjust-relative-base (next-n-param-modes param-modes-num 1)]
                                ['exit                 '()]
                                [_                     '()])])
    (map (lambda (arg-value param-mode) (argument param-mode arg-value))
         arg-values parameter-modes)))

(define (next-n-args interp n)
  (let ([pc  (interpreter-pc interp)]
        [src (interpreter-src interp)])
    (map (lambda (i) (hash-ref src (+ i pc)))
       (m->n-list 1 (+ n 1)))))

(define (next-n-param-modes param-modes n)
  (let* ([ordered-param-modes (map string->number (map string (reverse (string->list (number->string param-modes)))))])
    (map (lambda (i) (if (< i (length ordered-param-modes)) (num->param-mode (list-ref ordered-param-modes i)) 'position))
         (m->n-list 0 n))))

(define (num->param-mode n)
  (match n
         [0 'position]
         [1 'immediate]
         [2 'relative]))

;; Instructions

(define (interpret-arithmetic-operation interp args op)
  (let* ([lhs (load-arg interp (list-ref args 0))]
         [rhs (load-arg interp (list-ref args 1))]
         [result (op lhs rhs)]
         [new-interp (store-arg interp (list-ref args 2) result)])
     (if (equal? (interpreter-pc interp) (value-of-arg interp (list-ref args 2)))
         new-interp
         (increment-pc new-interp 4))))

(define (interpret-addition interp args)
  (interpret-arithmetic-operation interp args +))

(define (interpret-multiplication interp args)
  (interpret-arithmetic-operation interp args *))

(define (interpret-input interp args)
  (let* ([in-port    (interpreter-in interp)]
         [in         (string->number (read-line in-port))]
         [new-interp (store-arg interp (list-ref args 0) in)])
    (increment-pc new-interp 2)))

(define (interpret-output interp args)
  (let ([out-port (interpreter-out interp)]
        [out      (load-arg interp (list-ref args 0))])
    (println out out-port)
    (increment-pc interp 2)))

(define (interpret-jump-if-true interp args)
  (let ([in         (interpreter-in interp)]
        [out        (interpreter-out interp)]
        [src        (interpreter-src interp)]
        [rel-base   (interpreter-relative-base interp)]
        [to-check   (load-arg interp (list-ref args 0))]
        [jump-to-pc (load-arg interp (list-ref args 1))])
    (if (not (equal? to-check 0))
        (interpreter in out src jump-to-pc rel-base)
        (increment-pc interp 3))))

(define (interpret-jump-if-false interp args)
  (let ([in         (interpreter-in interp)]
        [out        (interpreter-out interp)]
        [src        (interpreter-src interp)]
        [rel-base   (interpreter-relative-base interp)]
        [to-check   (load-arg interp (list-ref args 0))]
        [jump-to-pc (load-arg interp (list-ref args 1))])
    (if (equal? to-check 0)
        (interpreter in out src jump-to-pc rel-base)
        (increment-pc interp 3))))

(define (interpret-less-than interp args)
  (let* ([lhs        (load-arg interp (list-ref args 0))]
         [rhs        (load-arg interp (list-ref args 1))]
         [new-interp (if (< lhs rhs)
                         (store-arg interp (list-ref args 2) 1)
                         (store-arg interp (list-ref args 2) 0))])
    (if (equal? (interpreter-pc interp) (value-of-arg interp (list-ref args 2)))
         new-interp
         (increment-pc new-interp 4))))

(define (interpret-equals interp args)
  (let* ([lhs        (load-arg interp (list-ref args 0))]
         [rhs        (load-arg interp (list-ref args 1))]
         [new-interp (if (equal? lhs rhs)
                         (store-arg interp (list-ref args 2) 1)
                         (store-arg interp (list-ref args 2) 0))])
    (if (equal? (interpreter-pc interp) (value-of-arg interp (list-ref args 2)))
         new-interp
         (increment-pc new-interp 4))))

(define (interpret-adjust-relative-base interp args)
  (let* ([in           (interpreter-in  interp)]
         [out          (interpreter-out interp)]
         [src          (interpreter-src interp)]
         [pc           (interpreter-pc  interp)]
         [rel-base     (interpreter-relative-base interp)]
         [arg          (load-arg interp (list-ref args 0))]
         [new-rel-base (+ rel-base arg)]
         [new-interp   (interpreter in out src pc new-rel-base)])
    (increment-pc new-interp 2)))

;; Interpreter Helpers

(define (at-pc interp)
  (let ([src (interpreter-src interp)]
        [pos (interpreter-pc interp)])
    (hash-ref src pos)))

(define (load-arg interp arg)
  (let ([mode     (argument-param-mode arg)]
        [num      (argument-value arg)]
        [src      (interpreter-src interp)]
        [rel-base (interpreter-relative-base interp)])
    (match mode
           ['immediate num]
           ['position  (hash-ref src num 0)]
           ['relative  (hash-ref src (+ num rel-base) 0)])))

(define (store-arg interp arg value)
  (let* ([mode     (argument-param-mode arg)]
         [num      (argument-value arg)]
         [in       (interpreter-in interp)]
         [out      (interpreter-out interp)]
         [pc       (interpreter-pc interp)]
         [src      (interpreter-src interp)]
         [rel-base (interpreter-relative-base interp)])
    (match mode
           ['immediate (error "trying to store in arg with immediate mode")]
           ['position  (hash-set! src num value)]
           ['relative  (hash-set! src (+ num rel-base) value)])
    (interpreter in out src pc rel-base)))

(define (value-of-arg interp arg)
  (let ([mode     (argument-param-mode arg)]
        [value    (argument-value arg)]
        [rel-base (interpreter-relative-base interp)])
    (match mode
         ['immediate value]
         ['position  value]
         ['relative  (+ value rel-base)])))

(define (increment-pc interp amount-to-increment)
  (let* ([in     (interpreter-in interp)]
         [out    (interpreter-out interp)]
         [src    (interpreter-src interp)]
         [pc     (interpreter-pc interp)]
         [rel-base (interpreter-relative-base interp)]
         [new-pc (+ pc amount-to-increment)])
    (interpreter in out src new-pc rel-base)))

;; Generic helpers

(define (m->n-list m n)
  (stream->list (in-range m n)))

;; Solution / IO functions

(define (interpreter->string interp)
  (let* ([src         (interpreter-src interp)]
         [src-strings (map number->string src)])
    (string-join src-strings ",")))

(define (read-src)
  (let* ([src           (make-hash (list))]
         [input         (read-line)]
         [input-strings (string-split input ",")]
         [input-numbers (map string->number input-strings)]
         [input-indices (stream->list (stream-take (in-naturals) (length input-numbers)))])
    (for-each (lambda (i x) (hash-set! src i x)) input-indices input-numbers)
    src))

(define (solution)
  (let* ([src     (read-src)]
         [interp  (interpreter (current-input-port) (current-output-port) src 0 0)])
    (run-interpreter interp)))

(solution)
