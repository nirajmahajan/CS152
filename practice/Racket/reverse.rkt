#lang racket
(define (reverse l)
  (define (op x y)
    (append y (list x)))
  (foldr op '() l))