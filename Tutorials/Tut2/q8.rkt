#lang racket
(define (pack l)
  (define (op x y)
    (cond [(null? (car y)) (list (list x))]
          [(equal? x (car (car y))) (cons (cons x (car y)) (cdr y))]
          [else (cons (list x) y)]))
  (foldr op '(()) l))