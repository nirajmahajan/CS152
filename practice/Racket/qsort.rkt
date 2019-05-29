#lang racket
(require "lc.rkt")

(define (qsort l)
  (cond [(null? l) '()]
        [else (let* ([pivot (car l)]
                     [lows (qsort (lc x : x <- (cdr l) @ (< x pivot)))]
                     [highs (qsort (lc x : x <- (cdr l) @ (>= x pivot)))])
                (append lows (list pivot) highs))]))