#lang racket
(require "lc.rkt")

(define (perms l)
  (define (g x)
    (map (lambda (y) (cons x y))
         (perms (remove x l))))
  (match l
    ['() '(())]
    [else (append* (map g l))]))

(define (perms-lc l)
  (match l
    ['() '(())]
    [else (lc (cons x y) : x <- l y <- (perms-lc (remove x l)))]))