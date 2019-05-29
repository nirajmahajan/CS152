#lang racket
(define (ways-win n)
  (cond [(= 0 n) 0]
        [(= 1 n) 1]
        [(= 2 n) 1]
        [(= 3 n) 0]
        [(= 4 n) 3]
        [else (+ (not-win (- n 1))
                 (not-win (- n 2))
                 (not-win (- n 4)))]))

(define (not-win n)
  (cond [(= 0 n) 1]
        [(= 1 n) 0]
        [(= 2 n) 0]
        [(= 3 n) 2]
        [(= 4 n) 0]
        [else (let ([a (ways-win (- n 1))]
                    [b (ways-win (- n 2))]
                    [c (ways-win (- n 4))])
                (cond[(or (= 0 a) (= 0 b) (= 0 c)) 0]
                     [else (+ a b c)]))]))
        
