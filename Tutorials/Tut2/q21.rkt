#lang racket
(define (check-list l)
  (define (not-in elem list)
    (cond [(null? list) #t]
          [(= elem (car list)) #f]
          [else (not-in elem (cdr list))]))
  
  (if (null? l) #t
      (and (and (>= (car l) 1) (<= (car l) 9))
           (not-in (car l) (cdr l))
           (check-list (cdr l)))))

(define (check-all-row l)
  (cond [(null? l) #t]
        [else (and (check-list (car l))
                   (check-all-row (cdr l)))]))

(define (check-all-column l)
  
  (define (extract-column l)
    (cond [(null? l) '()]
          [else (cons (caar l) (extract-column (cdr l)))]))

  (define (delete-1st-column l)
    (cond [(null? l) '()]
          [else (cons (cdar l)
                      (delete-1st-column (cdr l)))]))

  (cond [(null? (car l)) #t]
        [else (and (check-list (extract-column l))
                   (check-all-column (delete-1st-column l)))]))

  
(define (check-sub-squares l)
  ; returns the list corresponding to x, yth sub square
  (define (sub-square-to-list x y)
    (define x0 (* 3 x))
    (define y0 (* 3 y))
    
    ;returns the x, yth element on grid
    (define (grid x y)
      (list-ref (list-ref l x) y))
    
    (define (helper a)
      (cond [(= a 9) '()]
            [else (cons (grid (+ x0 (quotient a 3))
                              (+ y0 (remainder a 3)))
                        (helper (+ a 1)))]))
    (helper 0))

  
  (define (helper2 a)
    (cond [(= a 9) #t]
          [else (and (check-list (sub-square-to-list (quotient a 3)
                                                     (remainder a 3)))
                     (helper2 (+ a 1)))]))
  (helper2 0))
     

(define (check-c l)
  (and (check-sub-squares l)
       (check-all-row l)
       (check-all-column l)))

(define sample 
 '((7 3 5 6 1 4 8 9 2)
   (8 4 2 9 7 3 5 6 1)
   (9 6 1 2 8 5 3 7 4)
   (2 8 6 3 4 9 1 5 7)
   (4 1 3 8 5 7 9 2 6)
   (5 7 9 1 2 6 4 3 8)
   (1 5 7 4 9 2 6 8 3)
   (6 9 4 7 3 8 2 1 5)
   (3 2 8 5 6 1 7 4 9)))