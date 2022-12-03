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
         arg-values
         parameter-modes)))

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
  (let* ([lhs        (value-of-arg interp (list-ref args 0))]
         [rhs        (value-of-arg interp (list-ref args 1))]
         [result     (op lhs rhs)]
         [new-interp (store-cell interp (list-ref args 2) result)])
    (increment-pc new-interp 4)))

(define (interpret-addition interp args)
  (interpret-arithmetic-operation interp args +))

(define (interpret-multiplication interp args)
  (interpret-arithmetic-operation interp args *))

(define (interpret-input interp args)
  (let* ([in-port    (interpreter-in interp)]
         [in         (string->number (read-line in-port))]
         [new-interp (store-cell interp (list-ref args 0) in)])
    (increment-pc new-interp 2)))

(define (interpret-output interp args)
  (let ([out-port (interpreter-out interp)]
        [out      (value-of-arg interp (list-ref args 0))])
    (println out out-port)
    (increment-pc interp 2)))

(define (interpret-jump interp args pred)
  (let ([in         (interpreter-in interp)]
        [out        (interpreter-out interp)]
        [src        (interpreter-src interp)]
        [rel-base   (interpreter-relative-base interp)]
        [to-check   (value-of-arg interp (list-ref args 0))]
        [jump-to-pc (value-of-arg interp (list-ref args 1))])
    (if (pred to-check)
        (interpreter in out src jump-to-pc rel-base)
        (increment-pc interp 3))))

(define (interpret-jump-if-true interp args)
  (interpret-jump interp args (lambda (x) (not (equal? x 0)))))

(define (interpret-jump-if-false interp args)
  (interpret-jump interp args (lambda (x) (equal? x 0))))

(define (interpret-comp interp args cmp) 
  (let* ([lhs          (value-of-arg interp (list-ref args 0))]
         [rhs          (value-of-arg interp (list-ref args 1))]
         [to-store     (if (cmp lhs rhs) 1 0)]
         [new-interp   (store-cell interp (list-ref args 2) to-store)])
    (increment-pc new-interp 4)))

(define (interpret-less-than interp args)
  (interpret-comp interp args <))

(define (interpret-equals interp args)
  (interpret-comp interp args =))

(define (interpret-adjust-relative-base interp args)
  (let* ([in           (interpreter-in  interp)]
         [out          (interpreter-out interp)]
         [src          (interpreter-src interp)]
         [pc           (interpreter-pc  interp)]
         [rel-base     (interpreter-relative-base interp)]
         [arg          (value-of-arg interp (list-ref args 0))]
         [new-rel-base (+ rel-base arg)]
         [new-interp   (interpreter in out src pc new-rel-base)])
    (increment-pc new-interp 2)))

;; Interpreter Helpers

(define (at-pc interp)
  (let ([src (interpreter-src interp)]
        [pos (interpreter-pc interp)])
    (hash-ref src pos)))

(define (store-cell interp arg value)
  (let* ([mode     (argument-param-mode arg)]
         [pos      (argument-value arg)]
         [in       (interpreter-in interp)]
         [out      (interpreter-out interp)]
         [pc       (interpreter-pc interp)]
         [src      (interpreter-src interp)]
         [rel-base (interpreter-relative-base interp)]
         [new-src  (match mode
                          ['immediate (error "cannot store with immediate mode")]
                          ['position  (hash-set (interpreter-src interp) pos value)]
                          ['relative  (hash-set (interpreter-src interp) (+ pos rel-base) value)])])
    (interpreter in out new-src pc rel-base)))

(define (value-of-arg interp arg)
  (let ([mode     (argument-param-mode arg)]
        [value    (argument-value arg)]
        [rel-base (interpreter-relative-base interp)])
    (match mode
         ['immediate value]
         ['position  (hash-ref (interpreter-src interp) value 0)]
         ['relative  (hash-ref (interpreter-src interp) (+ value rel-base) 0)])))

(define (increment-pc interp amount-to-increment)
  (let* ([in     (interpreter-in interp)]
         [out    (interpreter-out interp)]
         [src    (interpreter-src interp)]
         [pc     (interpreter-pc interp)]
         [rel-base (interpreter-relative-base interp)]
         [new-pc (+ pc amount-to-increment)])
    (interpreter in out src new-pc rel-base)))

;; Board

(struct board (grid curr-pos dir-facing))

(define (get-input brd)
  (let* ([grid     (board-grid brd)]
         [curr-pos (board-curr-pos brd)]
         [curr-on  (hash-ref grid curr-pos 'unpainted)])
    (color->number curr-on)))

(define (process-output brd output)
  (let* ([grid       (board-grid brd)]
         [curr-pos   (board-curr-pos brd)]
         [dir-facing (board-dir-facing brd)]
         [out-color  (number->color (car output))]
         [out-dir    (number->dir   (cdr output))]
         [new-grid   (hash-set grid curr-pos out-color)]
         [new-dir    (change-dir dir-facing out-dir)]
         [new-pos    (move curr-pos new-dir)])
    ;; (println (string-append "PRINTING " (symbol->string out-color) " to spot (" (number->string (car curr-pos)) (number->string (cdr curr-pos)) ")"))
    (board new-grid new-pos new-dir)))

(define (change-dir facing new-dir)
  (match facing
         ['north (match new-dir ['left 'west]  ['right 'east])]
         ['east  (match new-dir ['left 'north] ['right 'south])]
         ['south (match new-dir ['left 'east]  ['right 'west])]
         ['west  (match new-dir ['left 'south] ['right 'north])]))

(define (move curr-pos dir)
  (let ([x (car curr-pos)]
        [y (cdr curr-pos)])
    (match dir
           ['north (cons x       (+ y 1))]
           ['east  (cons (+ x 1) y)]
           ['south (cons x       (- y 1))]
           ['west  (cons (- x 1) y)])))

(define (color->number color)
  (match color
         ['unpainted 0]
         ['black 0]
         ['white 1]))

(define (number->color number)
  (match number
         [0 'black]
         [1 'white]))

(define (number->dir number)
  (match number
         [0 'left]
         [1 'right]))


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
    (make-immutable-hash (hash->list src))))

(define should-keep-running #t)
(define num-iters 0)

(define (run-robot stop-semaphore brd board-read-from board-write-to)
  ; (println "robotty about to block for input")
  (semaphore-wait stop-semaphore)
  (if should-keep-running
      (begin (semaphore-post stop-semaphore)
             (println (number->string (get-input brd)) board-write-to)
             (let* ([out-color (string->number (read-line board-read-from))]
                    [out-dir   (string->number (read-line board-read-from))]
                    [new-board (process-output brd (cons out-color out-dir))])
               (set! num-iters (+ num-iters 1))
               (run-robot stop-semaphore new-board board-read-from board-write-to)))
      (begin (semaphore-post stop-semaphore)
             (calculate-answer brd))))

(define (calculate-answer brd)
  (let* ([grid            (board-grid brd)]
         [all-values      (hash-values grid)]
         [filtered-values (filter is-painted all-values)])
    (println (number->string (length filtered-values)))))

(define (is-painted tile-color)
  (not (equal? tile-color 'unpainted)))

(define (solution)
  (let*-values ([(interp-read-from board-write-to) (make-pipe)]
                [(board-read-from interp-write-to) (make-pipe)]
                [(src)    (read-src)]
                [(interp) (interpreter interp-read-from interp-write-to src 0 0)]
                [(brd)    (board (make-immutable-hash) (cons 0 0) 'north)]
                [(stop-semaphore) (make-semaphore 1)])
    (thread (lambda () (begin (run-interpreter interp)
                              (semaphore-wait stop-semaphore)
                              (set! should-keep-running #f)
                              (semaphore-post stop-semaphore))))
    (thread-wait (thread (lambda () (run-robot stop-semaphore brd board-read-from board-write-to))))))

(solution)
