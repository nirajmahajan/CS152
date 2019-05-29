#lang racket
(define (is-carmicheal n)
  (if (is-prime n) #f
  (and (prime-condition-check n)
       (square-free n))))

(define (prime-condition-check n)
  (prime-cond-checker n 2))

(define (prime-cond-checker n c)
  (cond[(>= c n) #t]
       [(and (is-prime c) (= 0 (remainder n c)) (not (= 0 (remainder (- n 1) (- c 1))))) #f]
       [else (prime-cond-checker n (+ 1 c))]))

(define (square-free n)
  (square-free-helper n 2))

(define (square-free-helper n test)
  (cond [(> (* test test) n) #t]
        [(= 0 (remainder n (* test test))) #f]
        [else (square-free-helper n (+ test 1))]))


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