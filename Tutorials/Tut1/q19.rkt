#lang racket

(define (interest-rate a m n)
  ; define rate function
  (define (f r)
    (- (* a r (expt (+ 1 r) n))
       (* m (- (expt (+ 1 r) n) 1))))
  ; define differential
  (define (diff f x)
    (define delta 0.001)
    (/ (- (f (+ x delta)) (f x)) delta))

  ;define newton raphson
  (define (newton-raphson ans)
    (cond [(< (f ans) 0.001) ans]
          [else (newton-raphson (- ans (/ (f ans) (diff f ans))))]))

  (newton-raphson 100)) 