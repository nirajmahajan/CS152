#lang racket
(define (code-generator l lst)
  (define (helper lst)
    (foldl (lambda (x y) (append y (permutations x)))
           '() lst))
  (helper (filter (lambda (x) (= (length x) l)) (combinations lst))))