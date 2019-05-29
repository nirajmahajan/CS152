#lang racket
(define (subsets l)
  (match l
    ['() '(())]
    [(cons first rest) (append (map (lambda (y) (cons first y)) (subsets rest))
                               (subsets rest))]))