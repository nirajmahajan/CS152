#lang racket
(struct node (ltree val rtree) #:transparent)
(struct leaf(val) #:transparent)

(define (abs-max t)
  (max (abs (car (node-val t))) (cdr (node-val t))))

(define (val-tree t)
  (match t
    [(node (leaf v1) v (leaf v2)) (node (leaf (cons 0 0)) (cons -1 1) (leaf (cons 0 0)))]
    [(node (leaf v1) v rt) (node (leaf (cons 0 0)) (cons -1 (+ 1 (abs-max (val-tree rt)))) (val-tree rt))]
    [(node lt v (leaf v1)) (node (val-tree lt) (cons (- (+ 1 (abs-max (val-tree lt)))) 1) (leaf (cons 0 0)))]
    [(node lt v rt) (node (val-tree lt) (cons (- (+ 1 (abs-max (val-tree lt)))) (+ 1 (abs-max (val-tree rt)))) (val-tree rt))]))

(define (dia t)
  (define (dia-h t)
    (match t
      [(node (leaf v1) v (leaf v2)) 2]
      [(node (leaf v1) v rt) (max (+ (cdr v) 1) (dia rt))]
      [(node lt v (leaf v1)) (max (- 1 (car v)) (dia lt))]
      [(node lt v rt) (max (- (cdr v) (car v)) (dia lt) (dia rt))]))
  (+ 1 (dia-h (val-tree t))))

; example
(define example (node
                 (node (leaf 0) 0
                       (node (leaf 0) 0
                             (leaf 0))) 0
                 (node (leaf 0) 0
                       (node (leaf 0) 0
                             (node (node (leaf 0) 0
                                         (leaf 0)) 0
                                   (leaf 0))))))
                  