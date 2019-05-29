#lang racket
(define (inits l)
  (define (op x y)
    (cons '()
          (map (lambda (y) (cons x y)) y)))
  (foldr op '(()) l))

(define (inits-m l)
  (match l
    ['() '()]
    [(cons first rest) (cons '()
                             (map (lambda (y) (cons first y)) (inits-m rest)))]))
                             