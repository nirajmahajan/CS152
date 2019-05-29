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

(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ([ct (curve t)])
      (make-point (y-of ct)
                  (x-of ct)))))

(define (translate x y curve)
  (lambda (t)
    (let ([ct (curve t)])
      (make-point (+ x (x-of ct)) 
                  (+ y (y-of ct))))))


(define (scale x y curve)
  (lambda (t)
    (let ([ct (curve t)])
      (make-point (* x (x-of ct)) 
                  (* y (y-of ct))))))

(define (rotate-about-origin radians curve)
  (lambda (t)
    (let* ([ct (curve t)]
           [x (x-of ct)]
           [y (y-of ct)]
           `  [length (sqrt (+ (* x x) (* y y)))])
      (make-point (* length (cos radians)) 
                  (* length (sin radians))))))

(define (put-in-standard-position curve)
  (let* ([p1 (curve 0)]
         [p2 (curve 1)]
         [x1 (x-of p1)]
         [y1 (y-of p1)]
         [x2 (x-of p2)]
         [y2 (y-of p2)]
         [slope (/ (- y2 y1) (x2 x1))]
         [theta (atan slope)]
         [length (sqrt (+ (expt (- x2 x1) 2)
                          (expt (- y2 y1) 2)))]
         [scale-factor (/ 1 length)])
    (translate (- x1) (- y1) curve)
    (rotate-about-origin (- theta) curve)
    (scale 1 scale-factor curve)))

(define (connect-rigidly curve1 curve2)
  (lambda (t)
    (if (< t (/ 1 2))
        (curve1 (* 2 t))
        (curve2 (- (* 2 t) 1)))))

(define (connect-ends curve1 curve2)
  (let* ([p1 (curve2 0)]
         [p2 (curve2 1)]
         [x1 (x-of p1)]
         [y1 (y-of p1)]
         [x2 (x-of p2)]
         [y2 (y-of p2)])
    (translate (- x1 x2) (- y1 y2) curve2)))

(require plot)
(plot-new-window? #t)
(plot-width 600)
(plot-height 600)
(define (draw curve)
  (plot (parametric
         (lambda (t) (vector (x-of (curve t))
                             (y-of (curve t))))
         0 1 #:width 1 #:samples 20000
         #:x-min -0.5 #:x-max 3.0
         #:y-min -0.5 #:y-max 3.0)))


; plot-width and plot-height give the size of the window
; #:width refers to the thickness of the line
; #:samples is the number of sample points. decrease it if
; your program takes too much time
; #:x-min -1 #:x-max 2 says that the graph is plotted in
; the x-axis range -1 to 2