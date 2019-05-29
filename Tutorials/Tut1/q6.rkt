#lang racket
(define (iseven a)
  (= 0 (remainder a 2)))
(define (square a)
  (* a a))
  

(define (modexp x y n)
  (cond [(= 0 y) 1]
        [(iseven y) (modulo (square (modexp x (/ y 2) n)) n)]
        [else (modulo (* (modulo (square (modexp x (/ (- y 1) 2) n)) n)
                          (modulo x n)) n)]))

