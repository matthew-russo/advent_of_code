;; Opcodes: 1, 2, 99
;;
;; Opcode 99 immediately exits the program
;;
;; Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.
;;
;; For example, if your Intcode computer encounters 1,10,20,30, it should read the values at positions 10 and 20, add those values, and then overwrite the value at position 30 with their sum.
;;
;; Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.
;;
;; Once you're done processing an opcode, move to the next one by stepping forward 4 positions.

;; before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.

;; What value is left at position 0 after the program halts?

#lang racket/base

(require racket/list)
(require racket/match)
(require racket/string)

(struct intcode-interpreter (src curr-pos))

(define (run-interpreter interpreter)
  (match (interpret-opcode (get-current-opcode interpreter))
         ['addition       (run-interpreter (interpret-addition interpreter))]
         ['multiplication (run-interpreter (interpret-multiplication interpreter))]
         ['exit           interpreter]
         ['unknown        (println (string-append "got unknown opcode: " (number->string get-current-opcode interpreter))) interpreter]))

(define (interpret-opcode opcode)
  (match opcode
         [1  'addition]
         [2  'multiplication]
         [99 'exit]
         [_  'unknown]))

(define (interpret-operation interpreter operation)
  (let* ([curr-pos (intcode-interpreter-curr-pos interpreter)]
         [lhs-index (+ curr-pos 1)]
         [rhs-index (+ curr-pos 2)]
         [result-pos-index (+ curr-pos 3)]
         [lhs-pos (get-number-at-index interpreter lhs-index)]
         [rhs-pos (get-number-at-index interpreter rhs-index)]
         [lhs (get-number-at-index interpreter lhs-pos)]
         [rhs (get-number-at-index interpreter rhs-pos)]
         [result-pos (get-number-at-index interpreter result-pos-index)]
         [result (operation lhs rhs)]
         [new-interp (store-number-at-index interpreter result-pos result)])
     (increment-curr-pos new-interp 4)))

(define (interpret-addition interpreter)
  (interpret-operation interpreter +))

(define (interpret-multiplication interpreter)
  (interpret-operation interpreter *))

(define (get-current-opcode interpreter)
  (let ([src (intcode-interpreter-src      interpreter)]
        [pos (intcode-interpreter-curr-pos interpreter)])
    (list-ref src pos)))

(define (get-number-at-index interpreter index)
  (let ([src (intcode-interpreter-src interpreter)])
    (list-ref src index)))

(define (store-number-at-index interpreter index value)
  (let* ([curr-pos (intcode-interpreter-curr-pos interpreter)]
         [src      (intcode-interpreter-src interpreter)]
         [new-src  (list-set src index value)])
    (intcode-interpreter new-src curr-pos)))

(define (increment-curr-pos interpreter amount-to-increment)
  (let* ([src          (intcode-interpreter-src interpreter)]
         [curr-pos     (intcode-interpreter-curr-pos interpreter)]
         [new-curr-pos (+ curr-pos amount-to-increment)])
    (intcode-interpreter src new-curr-pos)))

(define (read-input)
  (let* ([one-big-opcode-string (read-line)]
         [opcode-strings        (string-split one-big-opcode-string ",")])
    (map string->number opcode-strings)))

(define (interpreter->string interpreter)
  (let* ([src         (intcode-interpreter-src interpreter)]
         [src-strings (map number->string src)])
    (string-join src-strings ",")))

(define (replace-numbers interpreter noun verb)
  (let* ([with-one-replaced (store-number-at-index interpreter 1 noun)]
         [with-two-replaced (store-number-at-index with-one-replaced 2 verb)])
    with-two-replaced))

(define (run-interpreter-with-input src noun verb)
  (let* ([interpreter (intcode-interpreter src 0)]
         [modified-interpreter (replace-numbers interpreter noun verb)])
    (run-interpreter modified-interpreter)))

(define (find-solution-with-noun-and-verb src noun verb)
  (let* ([result (run-interpreter-with-input src noun verb)]
        [value-at-zero (get-number-at-index result 0)])
    (if (equal? value-at-zero 19690720)
        (+ (* 100 noun) verb)
        (if (equal? verb 99)
             (find-solution-with-noun-and-verb src (+ noun 1) 0)
             (find-solution-with-noun-and-verb src noun (+ verb 1))))))

(define (find-solution)
  (let* ([input (read-line)]
         [input-strings (string-split input ",")]
         [input-numbers (map string->number input-strings)])
    (find-solution-with-noun-and-verb input-numbers 0 0)))

(find-solution)
