#lang racket
(define (ak-47 x y)
  (cond [(or (= x 0) (= y 0)) 0]
        [else (let ([rx2 (remainder x 2)]
                    [qx2 (quotient x 2)])
                (+ (* (ak-47 qx2 y) 2) (* rx2 y)))]))