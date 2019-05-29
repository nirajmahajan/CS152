#lang racket
(define (dropwhile p l)
  (define (op x y)
    (cond [(and (null? y) (p x)) y]
          [else (append y (list x))]))
  (foldl op '() l))