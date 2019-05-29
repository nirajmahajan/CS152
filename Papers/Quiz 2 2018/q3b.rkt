#lang racket
(define (fewest-moves-iter path)
  (define (returner four three two one)
    (define one-min (+ 1 (cdr one)))
    (define two-min (cond [(equal? 1 (car two)) (+ 1 (cdr two))]
                          [else #f]))
    (define four-min (cond [(equal? 0 (car four)) (+ 1 (cdr four))]
                           [else #f]))
    (car (sort (remove* (list #f) (list one-min two-min four-min)) <)))

  (define (finish-returner four three two one)
    (define one-min (+ 1 (cdr one)))
    (define two-min (+ 1 (cdr two)))
    (define three-min (cond [(equal? 0 (car three)) (+ 1 (cdr three))]
                            [else #f]))
    (define four-min (cond [(equal? 0 (car four)) (+ 1 (cdr four))]
                           [else #f]))
    (car (sort (remove* (list #f) (list one-min two-min four-min)) <)))

  (define (fmh four three two one path-left)
    (cond [(equal? 'finish (car path-left)) (finish-returner four three two one)]
          [else (fmh three two one (cons (car path-left) (returner four three two one)) (cdr path-left))]))

  (fmh (cons 1 100) (cons 1 100)  (cons 1 -1) (cons 1 100) (append path (list 'finish))))