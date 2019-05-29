#lang racket
(define (is-palindrome l)
  (equal? l (reverse l)))

(define (reverse l)
  (define (op x y)
    (append y (list x)))
  (foldr op '() l))
(define (append x y)
  (foldr cons y x))