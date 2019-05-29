
#lang racket
(define (wcl str)
  (define (op x y)
    (cond [(char=? #\space x) (cons (+ (car y) 1) (cdr y))]
          [(char=? #\newline x) (cons (car y) (+ 1 (cdr y)))]
          [else y]))
  (foldl op (cons 0 1) (string->list str)))