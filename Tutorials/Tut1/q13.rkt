#lang racket
(define (is-prime n)
  (is-primec n 10))

(define (square x)
  (* x x))

(define (modexp x y n)
  (cond [(= 0 y) 1]
        [(even? y) (modulo (square (modexp x (/ y 2) n)) n)]
        [else (modulo (* (modulo (square (modexp x (/ (- y 1) 2) n)) n)
                          (modulo x n)) n)]))

; c is the number of numbers to check
(define (is-primec n c)
  (cond [(= c 0) #t]
        [(not (= (modexp (+ 1(random (- n 1))) (- n 1) n) 1)) #f]
        [else (is-primec n (- c 1))]))