#lang racket

(define (fib-tr n)
  (fib-iter n 0 0 1))

(define (fib-iter n c a b)
  (cond [(= c n) a]
        [(fib-iter n (+ c 1) (+ a b) a)])) 