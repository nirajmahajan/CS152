#lang racket
(struct gnode (val  lst) #:transparent)

(define (atlevel t n)
  (define (helper tr height)
    (match tr
      [(gnode val '()) (cond [(= height n) val]
                             [else '()])]
      [(gnode val l) (cond [(= height n) val]
                           [else (map (lambda (y) (helper y (+ height 1))) l)])]))
  (flatten (helper t 1)))

(define mytree (gnode 10
                 (list (gnode 8
                              (list (gnode 7 '())
                                    (gnode 9 '())))
                       (gnode 12
                              (list (gnode 11 '())
                                    (gnode 13 '()))))))
                              
                                           