#lang racket

(struct graph (nodes))
(struct node (id children))

(define (init-graph)
  (let* ([init-node (node "COM" (list))]
         [nodes (make-hash (list (cons "COM" init-node)))])
    (graph nodes)))

(define (add-node-to-graph g n)
  (let ([nodes (graph-nodes g)]
        [id    (node-id n)])
    (if (hash-has-key? nodes id)
        (let* ([prev-node    (hash-ref nodes id)]
               [new-children (append (node-children prev-node) (node-children n))]
               [new-node (node id new-children)])
          (hash-set! nodes id new-node)
          (graph nodes))
        (begin
          (hash-set! nodes id n)
          (graph nodes)))))

(define (count-orbits-graph g)
  (let ([init-node (hash-ref (graph-nodes g) "COM")])
    (count-orbits-node g init-node 0)))

(define (count-orbits-node g n current-depth)
  (let* ([next-depth         (+ current-depth 1)]
         [children           (node-children n)]
         [count-child-orbits (lambda (n1)
                               (if (hash-has-key? (graph-nodes g) n1)
                                   (count-orbits-node g (hash-ref (graph-nodes g) n1) next-depth)
                                   next-depth))]
         [children-orbits    (map count-child-orbits children)]
         [full-list          (append (list current-depth) children-orbits)])
    (apply + full-list)))

(define (parse-line-to-node line)
  (let* ([split (string-split line ")")]
         [id    (list-ref split 0)]
         [child (list-ref split 1)])
    (node id (list child))))

(define (add-line-to-graph g line)
  (add-node-to-graph g (parse-line-to-node line)))

(define (graph-from-input)
  (foldl (lambda (line g) (add-line-to-graph g line))
         (init-graph)
         (sequence->list (in-lines))))

(define (solution)
  (let ([g (graph-from-input)])
    (println (count-orbits-graph g))))

(solution)
