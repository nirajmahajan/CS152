#lang racket
(require "lc.rkt")

(define (gc n)
  (cond [(= n 0) '(())]
        [else (append (lc (append '(0) lst) : lst <- (gc (- n 1)))
                      (lc (append '(1) lst) : lst <- (gc (- n 1))))]))