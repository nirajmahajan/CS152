#lang racket
; matrix represented as (list a11 a12 a21 a22) where each row is a cons
(define (mtrx-mult a b)
  (let* ([a11 (car a)]
        [a12 (car (cdr a))]
        [a21 (car (cdr (cdr a)))]
        [a22 (car (cdr (cdr (cdr a))))]
        [b11 (car b)]
        [b12 (car (cdr b))]
        [b21 (car (cdr (cdr b)))]
        [b22 (car (cdr (cdr (cdr b))))]
        [c11 (+ (* a11 b11) (* a12 b21))]
        [c12 (+ (* a11 b12) (* a12 b22))]
        [c21 (+ (* a21 b11) (* a22 b21))]
        [c22 (+ (* a21 b12) (* a22 b22))])
    (list c11 c12 c21 c22)))

(define (fib-lightning n)
  (let ([a (list 1 1 1 0)])
    (car (fast-exp a (- n 1))))) 
        

(define (fast-exp a b)
  (cond [(= 0 b) (list 1 0 0 1)]
        [(even? b) (mtrx-mult (fast-exp a (/ b 2)) (fast-exp a (/ b 2)))]
        [else (mtrx-mult a (fast-exp a (- b 1)))]))