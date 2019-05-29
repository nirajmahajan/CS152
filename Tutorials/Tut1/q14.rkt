#lang racket
(define (inverse e n)
  (inverse-iter e n 1))

(define (inverse-iter e n c)
  (cond [(> c n) -1]
        [(= 1 (remainder (* e c) n)) c]
        [else (inverse-iter e n (+ 1 c))]))

(define (square x)
  (* x x))

(define (modexp x y n)
  (cond [(= 0 y) 1]
        [(even? y) (modulo (square (modexp x (/ y 2) n)) n)]
        [else (modulo (* (modulo (square (modexp x (/ (- y 1) 2) n)) n)
                          (modulo x n)) n)]))

(define (decode p q e c)
  (let ([key (inverse e (* (- p 1) (- q 1)))])
    (modexp c key (* p q))))