#lang racket
(define (has-solution a b c)
  (= 0 (remainder c (gcd a b))))

(define (gcd a b)
  (cond [(= a 0) b]
        [(= b 0) a]
        [(> a b) (gcd (remainder a b) b)]
        [else (gcd (remainder b a) a)]))