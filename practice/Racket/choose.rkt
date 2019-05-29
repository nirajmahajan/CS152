#lang racket
(define (choose l n)
  (cond [(or (> n (length l)) (<= n 0)) '(())]
        [(= n (length l)) (list l)]
        [else (append (choose (cdr l) n)
                      (map (lambda (y) (cons (car l) y))
                           (choose (cdr l) (- n 1))))]))