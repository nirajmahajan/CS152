#lang racket
(define (simplify r)
  (simpl (car r) (cdr r) 2))

(define (simpl a b c)
  (cond [(or (and (>= c a) (> c b)) (and (>= c b) (> c a))) (cons a b)]
        [(and (= 0 (remainder a c)) (= 0 (remainder b c))) (simpl (/ a c) (/ b c) c)]
        [else (simpl a b (+ c 1))]))

(define (add r1 r2)
  (simplify (cons (+ (* (car r1) (cdr r2))
                     (* (car r2) (cdr r1)))
                  (* (cdr r1) (cdr r2)))))

(define (multiply r1 r2)
  (simplify (cons (* (car r1) (car r2))
                  (* (cdr r1) (cdr r2)))))

(define (divide r1 r2)
  (simplify (cons (* (car r1) (cdr r2))
                  (* (cdr r1) (car r2)))))
