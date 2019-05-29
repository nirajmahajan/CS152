#lang racket
(define (coeffs a b)
  (if (> b a) (cons (cdr (coeffs b a))
                    (car (coeffs b a)))
      (cond [(= 0 b) (cons 1 0)]
            [else (let* ([x1 (car (coeffs b (modulo a b)))]
                         [y1 (cdr (coeffs b (modulo a b)))]
                         [y_val (- x1 (* (floor (/ a b)) y1))]
                         [x_val (- y1 (* (floor (/ a b)) x1))])
                    (cons y1 y_val))])))

                    
                  