#lang racket

(define (last l)
  (if (null? (cdr l)) (car l)
      (last (cdr l))))

(define (last-fr l)
  (define (op x y)
    (cond [(null? y) x]
          [else y]))
  (foldr op '() l))

(define (last-fl l)
  (define (op x y)
    x)
  (foldl op '() l))