#lang racket
(struct bnode (ltree rtree) #:transparent)
(struct leaf (val) #:transparent)

(define (to-btree bm)
  (define (second-half lst)
    (define tot-len (length lst))
    (define (helper ans)      
      (if (<= (length ans) (/ tot-len 2)) ans
          (helper (cdr ans))))
    (helper lst))

  (define (first-half lst)
    (define tot-len (length lst))
    (define (helper l)
      (cond [(<= (length l) (/ tot-len 2)) '()]
            [else (cons (car l) (helper (cdr l)))]))
    (helper lst))

  (define (all-one lst)
    (cond [(null? lst) #t]
          [(= 0 (car lst)) #f]
          [else (all-one (cdr lst))]))

  (define (all-zero lst)
    (cond [(null? lst) #t]
          [(= 1 (car lst)) #f]
          [else (all-zero (cdr lst))]))
  
  (let* ([1st (first-half bm)]
         [2nd (second-half bm)])
    (cond [(all-one bm) (leaf 1)]
          [(all-zero bm) (leaf 0)]
          [else (bnode (to-btree 1st) (to-btree 2nd))])))

(define (tree->list btr len)
  (define (depth tr)
    (match tr
      [(leaf val) 0]
      [(bnode lt rt) (max (+ 1 (depth lt)) (+ 1 (depth rt)))]))

  (define tot-len (cond [(= len -1) (expt 2 (depth btr))]
                        [else len]))

  (define (helper tr currlen)
    (match tr
      [(leaf val) (make-list currlen val)]
      [(bnode lt rt) (append (helper lt (/ currlen 2))
                             (helper rt (/ currlen 2)))]))

  (helper btr tot-len))

(define (bitwise-or bt1 bt2)
  (define (f a)
    (cond [(= a 1) #t]
          [(= a 0) #f]))
  (define (g a)
    (cond [a 1]
          [else 0]))
  
  (define (take-or l1 l2)
    (cond [(null? l1) '()]
          [else (cons (g (or (f (car l1)) (f (car l2))))
                      (take-or (cdr l1) (cdr l2)))]))

  (to-btree (take-or (tree->list bt1 -1)
                     (tree->list bt2 -1))))

(define (find-value index n bt)
  (list-ref (tree->list bt n) (- index 1)))