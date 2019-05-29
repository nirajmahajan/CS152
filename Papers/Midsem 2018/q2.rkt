#lang racket
(define (max-len l1 l2)
  (cond [(> (length l1) (length l2)) l1]
        [else l2]))

(define (ssm l target)
  (define (helper lst)
    (cond [(null? lst) (list target)]
          [(> (car lst) target) (max-len (ssm (cdr lst) target)
                                  (append (list (car lst)) (ssm (cdr lst) (car lst))))]
          [else (helper (cdr lst))]))

  (cond [(null? l) '()]
        [else (helper (cdr l))]))

(define (my-ssm l)
  (cons (car l) (ssm (cdr l) (car l))))