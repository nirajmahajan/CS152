#lang racket
(define (is-perfect? n)
  (= n (isperf n 1 0)))

(define (isperf n divisor ans)
  (cond [(= n divisor) ans]
        [(= 0 (remainder n divisor)) (isperf n (+ divisor 1) (+ ans divisor))] 
        [else (isperf n (+ divisor 1) ans)]))

