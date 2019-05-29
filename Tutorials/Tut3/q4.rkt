#lang racket
(require "list-comprehension.rkt")

(struct node (ltree rtree) #:transparent)
(struct leaf () #:transparent)

(define (all-sym-trees n)
  ;;returns a list of all random trees of n nodes
  (define (all-rand-trees n)
    
    (define (helper c)
      (cond [(= c n) '()]
            [else (append (lc (node x y) : x <- (all-rand-trees c) y <- (all-rand-trees (- n (+ c 1))))
                          (helper (+ 1 c)))]))
    (cond [(= n 0) (list (leaf))]
          [else (helper 0)]))

  (define (mirror tr)
    (match tr
      [(leaf) (leaf)]
      [(node lt rt) (node rt lt)]))

  (map (lambda (x) (node x (mirror x))) (all-rand-trees (/ (- n 1) 2))))