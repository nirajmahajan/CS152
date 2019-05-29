#lang racket
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

(define (multn x y)
  (cond [(or (= x 0) (= y 0)) 0]
        [else (let ([ry10 (remainder y 10)]
                    [qy10 (quotient y 10)])
                (convert (multn x qy10)
                         (multn1c x ry10 0)))]))

(define (multn1c x y c)
  (if (= x 0) c
   (let* ([rx10 (remainder x 10)]
         [qx10 (quotient x 10)]
         [next-carry (quotient (* rx10 y) 10)])
     (convert (multn1c qx10 y next-carry)
              (remainder (* rx10 y) 10)))))
  
                         
                    