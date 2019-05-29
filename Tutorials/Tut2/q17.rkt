#lang racket
(define (rle l)
  (define (op x y)
    (cond [(null? y) (cons (list x 1) y)]
          [(= x (caar y)) (cons (list (caar y) (+ 1(cadar y)))
                                (cdr y))]
          [else (cons (list x 1) y)]))
  (reverse (foldl op '() l)))