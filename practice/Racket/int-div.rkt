#lang racket
(define (double x)
  (* x 2))

; divide x by y
; returns 2 ints in the form (q, r)
(define (int-div x y)
  (if (< x y) (cons 0 x)
     (let* ([xis_div2 (= (remainder x 2) 0)]
           [x/2 (quotient x 2)]
           [new_rem_even (double (cdr (int-div x/2 y)))]
           [new_rem_odd (+ (double (cdr (int-div x/2 y))) 1)]
           [new_quo (double (car (int-div x/2 y)))])

       (cond [xis_div2 (cond [(>= new_rem_even y) (cons (+ new_quo 1)
                                                        (- new_rem_even y))]
                             [else (cons new_quo new_rem_even)])]
             ; x is odd
             [else (cond [(>= new_rem_odd y) (cons (+ new_quo 1)
                                                        (- new_rem_odd y))]
                             [else (cons new_quo new_rem_odd)])]))))