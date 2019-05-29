#lang racket
(require "lc.rkt")

(define (cprod l)
  (define (op x y)
    (lc (cons first second): first <- x second <- y))
  (foldr op '(()) l))