#lang racket

(define (flatten l)
  (cond [(null? l) '()]
        [(list? l) (append (flatten (car l)) (flatten (cdr l)))]
        [else (list l)]))

(define (flattenfl l)
  (define (op x y)
    (cond [(not (list? x)) (append y (list x))]
          [else (append y (flatten (car x)) (flatten (cdr x)))]))
  (foldl op '() l))

(define (flattenfr l)
  (define (op x y)
    (cond [(not (list x)) (cons x y)]
          [else (append (flatten (car x)) (flatten (cdr x)) y)]))
    (foldr op '() l))