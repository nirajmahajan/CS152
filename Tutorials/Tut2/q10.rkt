#lang racket
(define (slice l i k)
  (define (op x y)
    (cond [(or (< (cdr y) i) (> (cdr y) k)) (cons (car y) (+ 1 (cdr y)))]
          [else (cons (append (car y) (list x)) (+ 1 (cdr y)))]))
  (car (foldl op (cons '() 1) l)))