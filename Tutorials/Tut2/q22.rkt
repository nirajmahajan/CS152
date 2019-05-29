#lang racket
(define (lcs l1 l2)
  (cond [(or (null? l1)
            (null? l2)) '()]
        [(= (car l1) (car l2)) (cons (car l1) (lcs (cdr l1) (cdr l2)))]
        [(> (length (right-step l1 l2))
            (length (left-step l1 l2))) (right-step l1 l2)]
        [else (left-step l1 l2)]))

(define (left-step l1 l2)
  (lcs (cdr l1) l2))

(define (right-step l1 l2)
  (lcs l1 (cdr l2)))