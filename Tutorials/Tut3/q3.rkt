#lang racket
(struct node (value ltree rtree) #:transparent)
(struct nulltree () #:transparent)

(define (list-within bst lb ub)

  (define (in-range val)
    (and (<= val ub) (>= val lb)))
  
  (define (helper tree)
    (match tree
      [(node val (nulltree) (nulltree)) (cond [(in-range val) (list val)]
                                              [else '()])]
      [(node val (nulltree) rt) (cond [(in-range val) (cons val (helper rt))]
                                    [else '()])]
      [(node val lt (nulltree)) (cond [(in-range val) (append (helper lt) (list val))]
                                    [else '()])]
      [(node val lt rt) (cond [(in-range val) (append (helper lt) (list val) (helper rt))]
                              [else '()])]))

  (helper bst))

(define t1
  (node 10
        (node 10
              (node 5 (nulltree) (nulltree))
              (nulltree))
        (node 15 (nulltree) (nulltree))))