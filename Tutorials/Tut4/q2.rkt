#lang racket
(define (to-decimal l)
  (define state 0)
  (define ans 0)
  (define (helper lst)
    (cond [(null? lst) (* ans 1.0)]
          [(char=? (car lst) #\.) (begin (set! state 1) (helper (cdr lst)))]
          [else (set! state (* 10 state))
                (cond [(= state 0) (begin (set! ans (+ (* 10 ans) (f (car lst))))
                                          (helper (cdr lst)))]
                      [else (begin (set! ans (/ (+ (* state ans) (f (car lst))) state))
                                   (helper (cdr lst)))])]))
  (helper l))

(define (f x) (- (char->integer x) 48))