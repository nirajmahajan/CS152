#lang racket
(define (drop l n)
  (cond [(= n 1) '()]
        [else (define (op x y)
                (cond [(null? (car y)) (cons (list x) 2)] 
                      [(= (cdr y) n) (cons (car y) 1)]
                      [else (cons (append (car y) (list x)) (+ 1 (cdr y)))]))
              (car (foldl op '(()) l))]))
      
