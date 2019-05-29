#lang racket
(require "lc.rkt")

(define (add x y)
  (addc x y 0))

(define (convert x y)
  (+ (* x 10) y))
 
(define (addc x y c)
  (cond [(and (= x 0) (= y 0)) c]
         [else (let* ([rx10 (remainder x 10)]
                     [ry10 (remainder y 10)]
                     [qx10 (quotient x 10)]
                     [qy10 (quotient y 10)]
                     [remainder-sum (+ rx10 ry10 c)]
                     [next_carry (quotient remainder-sum 10)])
                 (convert (addc qx10 qy10 next_carry)
                          (remainder remainder-sum 10)))]))