#lang racket

(define (end-skim lst) (reverse (cdr (reverse lst))))

(define (diff lst aman)
  (match lst
    ['() aman]
    [(cons a '()) a]
    [else (let ([first-rm (- (car lst) (diff (cdr lst) 0))]
                [last-rm (- (last lst) (diff (end-skim lst) 0))])
            (begin (displayln (list lst first-rm last-rm))
            (cond [(> first-rm last-rm) first-rm]
                  [else last-rm])))]))