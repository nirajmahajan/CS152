#lang racket
(define (can-surv pos n)
  (cond [(< n 3) #t]
        [(> pos 3) (can-surv (- pos 3) (- n 1))]
        [(= pos 3) #f]
        [else (can-surv (- n (- 3 pos)) (- n 1))]))