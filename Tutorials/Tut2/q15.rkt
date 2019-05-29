#lang racket
(define (fewest-moves l)
  (define x (car l))
  (cond [(and (<= (length l) 4) (= x 1)) 1]
        [(and (<= (length l) 2) (= x 0)) 1]
        [(= x 1) (min (+ 1 (fewest-moves (cddddr l)))
                      (+ 1 (fewest-moves (cdr l))))]
        [else (min (+ 1 (fewest-moves (cddr l)))
                   (+ 1 (fewest-moves (cdr l))))]))
