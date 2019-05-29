#lang racket

(define (gcd a b)
  (cond [(> b a) (gcd b a)]
        [(= 0 b) a]
        [else (gcd b (remainder a b))]))
      
  
(define (fact a)
  (cond [(= 0 a) 1]
        [else (* a (fact (- a 1)))]))

(define (is-prime n)
  (is-primec n 10))

(define (square x)
  (* x x))

(define (modexp x y n)
  (cond [(= 0 y) 1]
        [(even? y) (modulo (square (modexp x (/ y 2) n)) n)]
        [else (modulo (* (modulo (square (modexp x (/ (- y 1) 2) n)) n)
                          (modulo x n)) n)]))

; c is the number of numbers to check
(define (is-primec n c)
  (cond [(= c 0) #t]
        [(not (= (modexp (+ 1(random (- n 1))) (- n 1) n) 1)) #f]
        [else (is-primec n (- c 1))]))

(define (f0 a b)
  (f0-helper a b 0))

(define (f0-helper a b ans)
  (if (> a b) ans
      (f0-helper (+ a 13) b (+ ans a))))

(define (f1 a b)
  (f1-helper a b 0))

(define (f1-helper a b ans)
  (if (> a b) ans
  (if (even? a) (f1-helper (+ 1 a) b ans)
      
      (f1-helper (+ a 2) b (+ ans (* a a))))))

(define (f2 a b)
  (f2-helper (+ a (- 3 (remainder a 3))) b 1))

(define (f2-helper a b ans)
  (if (> a b) ans
      (f2-helper (+ a 3) b (* ans (fact a)))))

(define (f3 a b)
  (f3-helper a b 0))

(define (f3-helper a b ans)
  (if (> a b) ans
      (cond [(is-prime a) (f3-helper (+ a 1) b (+ ans (* a a)))]
            [else (f3-helper (+a 1) b ans)])))

(define (f4 a b)
  (f4-helper a b 1))

(define (f4-helper a b ans)
  (cond [(> a b) ans]
        [(= 1 (gcd 50 a)) (f4-helper (+ a 1) b (* ans a))]
        [else (f4-helper (+ a 1) b ans)]))

(define (filtered-accumulate f a b)
  (f a b)
