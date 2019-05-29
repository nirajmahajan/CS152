#lang racket
(define (remove-duplicates l)
  (define (op x y)
    (cond [(not-in x y) (append y x)]
          [else y]))
  (foldl op '() l))

(define (not-in y x)
  (cond [(not (list? x)) (not-in y (list x))]
        [(null? x) #t]
        [(equal? y (car x)) #f]
        [else (not-in y (cdr x))]))

(define (append a b)
  (cond [(and (list? a) (list? b)) (foldr cons b a)]
        [(and (list? a) (not (list? b))) (foldr cons (list b) a)]
        [(and (list? b) (not (list? a))) (foldr cons b (list a))]
        [else (foldr cons (list b) (list a))]))