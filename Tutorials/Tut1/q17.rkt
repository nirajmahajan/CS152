#lang racket
(define (simpson f a b n)
  (simpson-helper f a b n 0 0))

(define (simpson-helper f a b n ans index)
  (define h (/ (- b a ) n))
  (define (y x)
    (f (+ a (* index h ))))
    
  (cond [(> index n) (* (/ h 3) ans)]
        [(or (= index 0) (= index n)) (simpson-helper f a b n (+ ans (y index)) (+ index 1))]
        [(even? index) (simpson-helper f a b n (+ ans (* 2 (y index))) (+ index 1))]
        [else (simpson-helper f a b n (+ ans (* 4 (y index))) (+ index 1))]))

(define (f x)
  (* x x))