#lang racket

(define (last-but-one l)
  (last-but-one-helper 0 l))
(define (last-but-one-helper prev l)
  (cond [(null? (cdr l)) prev]
        [else (last-but-one-helper (car l) (cdr l))]))