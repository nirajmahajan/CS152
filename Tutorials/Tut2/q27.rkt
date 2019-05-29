#lang racket
(define (pascal n)
  (define (pas-next l)
    (define (helper l ans)
      (cond [(null? (cdr l)) (cons 1 ans)]
            [else (helper (cdr l) (cons (+ (car l) (cadr l)) ans))]))
    (helper l '(1)))
      
  (define (helper l index)
    (cond [(= index 0) l]
          [else (helper (cons (pas-next (car l)) l)
                        (- index 1))]))
  (reverse (helper '((1)) n)))