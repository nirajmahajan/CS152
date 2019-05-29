#lang racket
(define (kth-element l k)
  (define (helper counter l)
    (cond [(= counter k) (car l)]
          [else (helper (+ counter 1) (cdr l))]))
  (helper 1 l))