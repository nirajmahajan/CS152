#lang racket
(define (zip l1 l2)
  (match (list l1 l2)
    [(list any '()) '()]
    [(list '() any) '()]
    [(list (cons first1 rest1) (cons first2 rest2)) (cons (cons first1 first2) (zip rest1 rest2))]))