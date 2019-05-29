#lang racket
(define (remove-consecutive l)
  (define (op x y)
    (displayln (list x y))
    (cond [(null? y) (list x)]
          [(equal? x (car y)) y]
          [else (cons x y)]))
  (foldr op '() l))