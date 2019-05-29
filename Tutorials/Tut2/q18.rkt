#lang racket
(require "lc.rkt")

(define (summands n)
  (define (helper c)
    (cond [(= c (+ n 1)) '()]
          [else (append (map (lambda (y) (cons c y)) (summands (- n c)))
                        (helper (+ c 1)))]))
  (cond [(= n 0) '(())]
        [else (helper 1)]))