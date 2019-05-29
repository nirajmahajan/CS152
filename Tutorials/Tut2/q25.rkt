#lang racket
(define (listify l f base)
  (define (op x y) (map (lambda (x1 y1) (f x1 y1)) x y))
  (foldr op (list base base base) l))

(define (list-+ l)
 (listify l + 0))

(define (list-* l)
  (listify l * 1))