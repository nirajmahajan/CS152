#lang racket
(define first 0)
(define second 0)
(define third 0)
  
(define (reader)
  (let ([new (read)])
    (cond [(equal? 'display new) (begin (displayln (+ first second third)) (reader))]
          [else (begin
                  (set! first second)
                  (set! second third)
                  (set! third new)
                  (reader))])))