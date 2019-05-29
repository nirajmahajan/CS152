#lang racket
(define (my-mod a b)
  (cond [(= (remainder a b) 0) b]
        [else (modulo a b)]))

(define (rotate l n)
  (let ([len (length l)])
    (define (helper k ans)
      (cond [(> k len) ans]
            [else (helper (+ 1 k) (append ans (list (list-ref l (- (my-mod (+ k n) len) 1)))))]))
  (helper 1 '())))