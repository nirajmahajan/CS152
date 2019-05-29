#lang racket
(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))

(define (print-point p)
  (cons (x-of p)
        (y-of p)))

(define (unit-circle)
  (lambda (t)
    (make-point (sin (* 2 pi t))
                (cos (* 2 pi t)))))

(define (unit-line-at y)
  (lambda (t) (make-point t y)))

(define (unit-line)
  (unit-line-at 0))

(define (vertical-line p l)
  (lambda (t)
    (print-point (make-point (+ (x-of p) (* l t))
                (+ (y-of p) (* l t))))))

(define (rotate-pi/2 curve)
  (lambda (t)
    (let ([ct (curve t)])
      (make-point (- (y-of ct))
                  (x-of ct)))))