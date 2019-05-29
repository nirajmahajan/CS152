#lang racket
(define (scaled-random x1 x2)
  (+ x1 (* (random)
           (- x2 x1))))

(define (monte-carl predicate x1 x2 y1 y2 n valid)
  (let ([xrand (scaled-random x1 x2)]
        [yrand (scaled-random y1 y2)])
    (cond [(= 0 n) valid]
          [(predicate xrand yrand) (monte-carl predicate x1 x2 y1 y2
                                               (- n 1)
                                               (+ valid 1))]
          [else (monte-carl predicate x1 x2 y1 y2
                            (- n 1)
                            valid)])))
(define (monte-carlo predicate x1 x2 y1 y2 n)
  (* (/ (monte-carl predicate x1 x2 y1 y2 n 0) n 1.0)
     (* (- x2 x1) (- y2 y1))))

(define (circ1 x y)
  (< (+ (* x x) (* y y)) 1))