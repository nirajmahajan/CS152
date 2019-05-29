#lang racket
(define (insert-at e l n)
  (define (op x y)
    (cond [(not (= n (cdr y))) (cons (append (car y) (list x)) (+ 1 (cdr y)))]
          [else (cons (append (car y) (list e x)) (+ 1 (cdr y)))]))
  (car (foldl op (cons '() 1) l)))