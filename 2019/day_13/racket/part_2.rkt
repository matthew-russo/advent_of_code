#lang racket

(require racket/list)
(require racket/match)
(require racket/string)
(require racket/stream)

(struct interpreter (in out src pc relative-base))
(struct opcode (instruction arguments))
(struct argument (param-mode value))

(define (run-interpreter interp ch)
  (let* ([opcode      (parse-opcode interp)]
         [instruction (opcode-instruction opcode)]
         [arguments   (opcode-arguments opcode)])
    (match (opcode-instruction opcode)
         ['addition             (run-interpreter (interpret-addition interp arguments)       ch)]
         ['multiplication       (run-interpreter (interpret-multiplication interp arguments) ch)]
         ['input                (begin (channel-put ch 'input) (run-interpreter (interpret-input interp arguments) ch))]
         ['output               (begin (channel-put ch 'output) (run-interpreter (interpret-output interp arguments) ch))]
         ['jump-if-true         (run-interpreter (interpret-jump-if-true interp arguments)   ch)]
         ['jump-if-false        (run-interpreter (interpret-jump-if-false interp arguments)  ch)]
         ['less-than            (run-interpreter (interpret-less-than interp arguments)      ch)]
         ['equals               (run-interpreter (interpret-equals interp arguments)         ch)]
         ['adjust-relative-base (run-interpreter (interpret-adjust-relative-base interp arguments) ch)]
         ['exit                 (begin (channel-put ch 'exit) interp)]
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

(define (modify-src src)
  (hash-set src 0 2))

(struct output-state (id fst snd))
(struct game (screen score paddle-pos ball-pos out-state read-from write-to))

(define (game-swap-score g score)
  (game (game-screen g)
        score
        (game-paddle-pos g)
        (game-ball-pos g)
        (game-out-state g)
        (game-read-from g)
        (game-write-to g)))

(define (game-swap-screen g screen)
  (game screen
        (game-score g)
        (game-paddle-pos g)
        (game-ball-pos g)
        (game-out-state g)
        (game-read-from g)
        (game-write-to g)))

(define (game-swap-paddle-pos g pos)
  (game (game-screen g)
        (game-score g)
        pos
        (game-ball-pos g)
        (game-out-state g)
        (game-read-from g)
        (game-write-to g)))

(define (game-swap-ball-pos g pos)
  (game (game-screen g)
        (game-score g)
        (game-paddle-pos g)
        pos
        (game-out-state g)
        (game-read-from g)
        (game-write-to g)))

(define (game-swap-out-state g out-state)
  (game (game-screen g)
        (game-score g)
        (game-paddle-pos g)
        (game-ball-pos g)
        out-state
        (game-read-from g)
        (game-write-to g)))

(define (process-output g)
  (let ([out-state (game-out-state g)])
    (match (output-state-id out-state)
          ['none-read  (game-swap-out-state g (output-state 'first-read
                                                          (string->number (read-line (game-read-from g)))
                                                          (void)))]
          ['first-read (game-swap-out-state g (output-state 'second-read
                                                          (output-state-fst out-state)
                                                          (string->number (read-line (game-read-from g)))))]
          ['second-read (begin
                          (let ([fst (output-state-fst out-state)]
                                [snd (output-state-snd out-state)]
                                [thrd (string->number (read-line (game-read-from g)))]
                                [new-game (game-swap-out-state g (output-state 'none-read (void) (void)))])
                            (if (equal? fst -1)
                                (game-swap-score new-game thrd)
                                (process-draw new-game fst snd thrd))))])))

(define (process-draw g dist-from-left dist-from-top tile-id)
  (let* ([tile-type  (tile-id->tile-type tile-id)]
         [pos        (cons dist-from-left dist-from-top)]
         [new-screen (hash-set (game-screen g) pos tile-type)]
         [new-game   (game-swap-screen g new-screen)])
    (match tile-type
      ['ball   (game-swap-ball-pos new-game (car pos))]
      ['paddle (game-swap-paddle-pos new-game (car pos))]
      [_       new-game])))

(define (write-input g)
  (let* ([paddle-x (game-paddle-pos g)]
         [ball-x   (game-ball-pos g)]
         [joystick (cond
                    [(< ball-x paddle-x) -1]
                    [(> ball-x paddle-x) 1]
                    [(= ball-x paddle-x) 0])])
    (println joystick (game-write-to g))))

(define (tile-id->tile-type tile-id)
  (match tile-id
    [0 'empty]
    [1 'wall]
    [2 'block]
    [3 'paddle]
    [4 'ball]))

(define (run-game ch g)
  (let ([channel-input (channel-get ch)])
    (match channel-input
           ['input (begin (write-input g) (run-game ch g))]
           ['output (run-game ch (process-output g))]
           ['exit (begin (println (game-score g)) g)])))

(define (solution)
  (let*-values ([(interp-read-from game-write-to) (make-pipe)]
                [(game-read-from interp-write-to) (make-pipe)]
                [(src)    (modify-src (read-src))]
                [(interp) (interpreter interp-read-from interp-write-to src 0 0)]
                [(game) (game (make-immutable-hash)
                              0
                              -1
                              -1
                              (output-state 'none-read (void) (void))
                              game-read-from
                              game-write-to)]
                [(ch) (make-channel)])
    (thread (lambda () (run-interpreter interp ch)))
    (thread-wait (thread (lambda () (run-game ch game))))))

(solution)
