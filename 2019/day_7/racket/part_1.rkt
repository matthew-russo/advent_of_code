#lang racket

(require racket/list)
(require racket/match)
(require racket/string)
(require racket/stream)

(struct interpreter (in out src pc))
(struct opcode (instruction arguments))
(struct argument (param-mode value))

(define (run-interpreter interp)
  (let* ([opcode      (parse-opcode interp)]
         [instruction (opcode-instruction opcode)]
         [arguments   (opcode-arguments opcode)])
    (match (opcode-instruction opcode)
         ['addition       (run-interpreter (interpret-addition interp arguments))]
         ['multiplication (run-interpreter (interpret-multiplication interp arguments))]
         ['input          (run-interpreter (interpret-input interp arguments))]
         ['output         (run-interpreter (interpret-output interp arguments))]
         ['jump-if-true   (run-interpreter (interpret-jump-if-true interp arguments))]
         ['jump-if-false  (run-interpreter (interpret-jump-if-false interp arguments))]
         ['less-than      (run-interpreter (interpret-less-than interp arguments))]
         ['equals         (run-interpreter (interpret-equals interp arguments))]
         ['exit           interp]
         ['unknown        (println (string-append "got unknown opcode: "
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
         [99 'exit]
         [_  'unknown]))

(define (parse-arguments interp instruction)
  (let* ([param-modes-num (truncate (/ (at-pc interp) 100))]
         [arg-values (match instruction
                          ['addition       (next-n-args interp 3)]
                          ['multiplication (next-n-args interp 3)]
                          ['input          (next-n-args interp 1)]
                          ['output         (next-n-args interp 1)]
                          ['jump-if-true   (next-n-args interp 2)]
                          ['jump-if-false  (next-n-args interp 2)]
                          ['less-than      (next-n-args interp 3)]
                          ['equals         (next-n-args interp 3)]
                          ['exit           '()]
                          [_               '()])]
         [parameter-modes (match instruction
                                ['addition       (next-n-param-modes param-modes-num 3)]
                                ['multiplication (next-n-param-modes param-modes-num 3)]
                                ['input          (next-n-param-modes param-modes-num 1)]
                                ['output         (next-n-param-modes param-modes-num 1)]
                                ['jump-if-true   (next-n-param-modes param-modes-num 2)]
                                ['jump-if-false  (next-n-param-modes param-modes-num 2)]
                                ['less-than      (next-n-param-modes param-modes-num 3)]
                                ['equals         (next-n-param-modes param-modes-num 3)]
                                ['exit           '()]
                                [_               '()])])
    ;; (display "\n\n")
    ;; (println (string-append "PC: " (number->string (interpreter-pc interp))))
    ;; (println (string-append "PARAMMODENUM: " (number->string param-modes-num)))
    ;; (println (string-append "PARAMMODES: " (string-join (map symbol->string parameter-modes))))
    ;; (println (string-append "INSTRUCTION: " (symbol->string instruction)))
    ;; (println (string-append "ARG-VALUES: " (string-join (map number->string arg-values))))
    (map (lambda (arg-value param-mode) (argument param-mode arg-value))
         arg-values parameter-modes)))

(define (next-n-args interp n)
  (let ([pc (interpreter-pc interp)])
    (map (lambda (i) (get-at-index interp (+ i pc)))
       (m->n-list 1 (+ n 1)))))

(define (next-n-param-modes param-modes n)
  (let* ([ordered-param-modes (map string->number (map string (reverse (string->list (number->string param-modes)))))])
    (map (lambda (i) (if (< i (length ordered-param-modes)) (num->param-mode (list-ref ordered-param-modes i)) 'position))
         (m->n-list 0 n))))

(define (num->param-mode n)
  (match n
         [0 'position]
         [1 'immediate]))

(define (value-of-arg interp arg)
  (let ([mode (argument-param-mode arg)]
        [num  (argument-value arg)])
    (match mode 
           ['position (get-at-index interp num)]
           ['immediate num])))

;; Instructions

(define (interpret-arithmetic-operation interp args op)
  (let* ([lhs (value-of-arg interp (list-ref args 0))]
         [rhs (value-of-arg interp (list-ref args 1))]
         [result (op lhs rhs)]
         [result-pos (argument-value (list-ref args 2))]
         [new-interp (store-at-index interp result-pos result)])
     (if (equal? (interpreter-pc interp) result-pos)
         new-interp
         (increment-pc new-interp 4))))

(define (interpret-addition interp args)
  (interpret-arithmetic-operation interp args +))

(define (interpret-multiplication interp args)
  (interpret-arithmetic-operation interp args *))

(define (interpret-input interp args)
  (let* ([in-port    (interpreter-in interp)]
         [in         (string->number (read-line in-port))]
         [new-interp (store-at-index interp (argument-value (list-ref args 0)) in)])
    (increment-pc new-interp 2)))

(define (interpret-output interp args)
  (let ([out-port (interpreter-out interp)]
        [out      (value-of-arg interp (list-ref args 0))])
    (println out out-port)
    (increment-pc interp 2)))

(define (interpret-jump-if-true interp args)
  (let ([in         (interpreter-in interp)]
        [out        (interpreter-out interp)]
        [src        (interpreter-src interp)]
        [to-check   (value-of-arg interp (list-ref args 0))]
        [jump-to-pc (value-of-arg interp (list-ref args 1))])
    (if (not (equal? to-check 0))
        (interpreter in out src jump-to-pc)
        (increment-pc interp 3))))

(define (interpret-jump-if-false interp args)
  (let ([in         (interpreter-in interp)]
        [out        (interpreter-out interp)]
        [src        (interpreter-src interp)]
        [to-check   (value-of-arg interp (list-ref args 0))]
        [jump-to-pc (value-of-arg interp (list-ref args 1))])
    (if (equal? to-check 0)
        (interpreter in out src jump-to-pc)
        (increment-pc interp 3))))

(define (interpret-less-than interp args)
  (let* ([lhs        (value-of-arg interp (list-ref args 0))]
         [rhs        (value-of-arg interp (list-ref args 1))]
         [result-pos (argument-value (list-ref args 2))]
         [new-interp (if (< lhs rhs)
                         (store-at-index interp result-pos 1)
                         (store-at-index interp result-pos 0))])
    (if (equal? (interpreter-pc interp) result-pos)
         new-interp
         (increment-pc new-interp 4))))

(define (interpret-equals interp args)
  (let* ([lhs        (value-of-arg interp (list-ref args 0))]
         [rhs        (value-of-arg interp (list-ref args 1))]
         [result-pos (argument-value (list-ref args 2))]
         [new-interp (if (equal? lhs rhs)
                         (store-at-index interp result-pos 1)
                         (store-at-index interp result-pos 0))])
    (if (equal? (interpreter-pc interp) result-pos)
         new-interp
         (increment-pc new-interp 4))))

;; Interpreter Helpers

(define (at-pc interp)
  (let ([src (interpreter-src interp)]
        [pos (interpreter-pc interp)])
    (list-ref src pos)))

(define (get-at-index interp index)
  (let ([src (interpreter-src interp)])
    (list-ref src index)))

(define (store-at-index interp index value)
  (let* ([in     (interpreter-in interp)]
         [out    (interpreter-out interp)]
         [pc       (interpreter-pc interp)]
         [src      (interpreter-src interp)]
         [new-src  (list-set src index value)])
    (interpreter in out new-src pc)))

(define (increment-pc interp amount-to-increment)
  (let* ([in     (interpreter-in interp)]
         [out    (interpreter-out interp)]
         [src    (interpreter-src interp)]
         [pc     (interpreter-pc interp)]
         [new-pc (+ pc amount-to-increment)])
    (interpreter in out src new-pc)))

;; Generic helpers

(define (m->n-list m n)
  (stream->list (in-range m n)))

;; Solution / IO functions

(define (interpreter->string interp)
  (let* ([src         (interpreter-src interp)]
         [src-strings (map number->string src)])
    (string-join src-strings ",")))

(define (read-src)
  (let* ([input         (read-line)]
         [input-strings (string-split input ",")]
         [input-numbers (map string->number input-strings)])
    input-numbers))

(define (run-simulation src a-setting b-setting c-setting d-setting e-setting)
  (let*-values ([(a-in init-out)  (make-pipe)]
                [(b-in a-out)     (make-pipe)]
                [(c-in b-out)     (make-pipe)]
                [(d-in c-out)     (make-pipe)]
                [(e-in d-out)     (make-pipe)]
                [(final-in e-out) (make-pipe)]
                [(a-interp)       (interpreter a-in a-out src 0)]
                [(b-interp)       (interpreter b-in b-out src 0)]
                [(c-interp)       (interpreter c-in c-out src 0)]
                [(d-interp)       (interpreter d-in d-out src 0)]
                [(e-interp)       (interpreter e-in e-out src 0)])
    (println a-setting init-out)
    (println b-setting a-out)
    (println c-setting b-out)
    (println d-setting c-out)
    (println e-setting d-out)
    (println 0 init-out)
    (run-interpreter a-interp) 
    (run-interpreter b-interp) 
    (run-interpreter c-interp) 
    (run-interpreter d-interp) 
    (run-interpreter e-interp)
    (read final-in)))

(define (solution)
  (let* ([src     (read-src)]
         [perms   (permutations '(0 1 2 3 4))]
         [run-sim (lambda (p) (apply run-simulation (append (list src) p)))]
         [results (map run-sim perms)])
    (apply max results)))

(solution)
