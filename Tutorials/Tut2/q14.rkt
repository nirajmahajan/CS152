#lang racket
(define (my-expt x lst)
  (define (op x1 y1)
    (cond [(= x1 1) (* x y1 y1)]
          [else (* y1 y1)]))
  (foldl op 1 lst))