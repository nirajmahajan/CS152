#lang racket
(define (fib n)
  (fib-iter n 0 0 1))

(define (fib-iter n c a b)
  (cond [(= c n) a]