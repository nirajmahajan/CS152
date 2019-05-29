#lang racket
(define (isprime n)
  (isprimec n 2))

(define (isprimec n divisor)
  (cond [(> divisor (sqrt n)) #t]
        [(= 0 (remainder n divisor)) #f]
        [else (isprimec n (+ 1 divisor))]))

(define (goldbach m)
  (gold-find 2 (- m 2)))

(define (gold-find a b)
  (cond [(and (isprime a) (isprime b)) (cons a b)]
        [else (gold-find (+ a 1) (- b 1))]))