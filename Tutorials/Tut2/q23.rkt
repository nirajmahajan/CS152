#lang racket

(define (shuffle l1 l2)
  (match (list l1 l2)
    [(list any '()) (list any)]
    [(list '() any) (list any)]
    [(list (cons first1 rest1) (cons first2 rest2)) (let* ([leftl (map (lambda (y) (cons first1 y)) (shuffle rest1 l2))]
                                                           [rightl (map (lambda (y) (cons first2 y)) (shuffle l1 rest2))])
                                                      (append leftl rightl))]))