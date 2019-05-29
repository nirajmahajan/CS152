#lang racket
(define (tails l)
  (define (op x y)
    (displayln (list x y))
    (append (map (lambda (y) (append y (list x))) y)
            '(())))
  (foldr op '(()) l))

(define (tails-m l)
  (match l
    ['() '(())]
    [(cons first rest) (cons l (tails-m (cdr l)))]))