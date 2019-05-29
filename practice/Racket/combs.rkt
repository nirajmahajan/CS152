#lang racket
(define (combs l)
  (match l
    ['() '(())]
    [(cons x rest) (append (map (lambda (y) (cons x y)) (combs rest))
                           (combs rest))]))