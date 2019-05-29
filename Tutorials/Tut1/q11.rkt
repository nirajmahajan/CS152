#lang racket
(define (inverse e n)
  (inverse-iter e n 1))

(define (inverse-iter e n c)
  (cond [(> c n) -1]
        [(= 1 (remainder (* e c) n)) c]
        [else (inverse-iter e n (+ 1 c))]))
   