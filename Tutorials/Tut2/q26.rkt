#lang racket
; checks the elements of l and divides the first divisible entry by n
(define (drain l n)  
  (let ([gcdd (gcd n (car l))])
    (cond [(= n 1) l]
          [(> gcdd 1) (cons (/ (car l) gcdd) (drain (cdr l) (/ n gcdd)))]
          [else (cons (car l) (drain (cdr l) n))])))

(define (evaluate la lb)
  (define (drain-all la lb)
    (cond [(null? lb) la]
          [else (drain-all (drain la (car lb)) (cdr lb))]))

  (define (prod l)
    (foldr * 1 l))

  (prod (drain-all la lb)))