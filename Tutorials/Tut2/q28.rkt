#lang racket
(define (rev-words str)
  (define (compartmentalize l)
    (define (op x y)
      (cond [(null? (car y)) (list (list x))]
            [(or (char=? x #\newline) (char=? x #\space)) (cons (list x) y)]
            [(or (char=? (caar y) #\newline) (char=? (caar y) #\space)) (append (list (list x)) y)]
            [else (cons (cons x (car y)) (cdr y))]))
    (foldr op '(()) l))
  
  (define (rev-helper l)
    (cond [(null? l) '()]
          [(or (char=? (caar l) #\newline) (char=? (caar l) #\space)) (cons (caar l) (rev-helper (cdr l)))]
          [else (cons (reverse (car l)) (rev-helper (cdr l)))]))
    
  (display (list->string (flatten (rev-helper (compartmentalize (string->list str)))))))