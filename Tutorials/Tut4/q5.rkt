#lang racket
(define state (cons 1 0))
(define (f x)
  (cond [(not (or (= x 1) (= x 0))) "Invalid Argument"]
        [else (begin
                (cond [(= (cdr state) 2) (set! state (cons 1 0))])
                (let ([ans (* (car state) x)])
                  (set! state (cons x (+ 1 (cdr state))))
                  ans))]))
