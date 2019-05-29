#lang racket
(define (memo-choose)
  (define 2d-vec (build-vector 5 (lambda (x) (make-vector 5 #\#))))
  2d-vec
;; represent nCr as row Choose column
  (define (2d-set! elem r c)
    (let* ([row (vector-ref 2d-vec r)]
           [new-row (vector-set! row c elem)])
      (vector-set! 2d-vec r row)))

  (define (init)
    (begin
      (2d-set! 1 0 0)
      (2d-set! 1 1 0)
      (2d-set! 0 0 1)
      (2d-set! 1 1 1)))

  (define (f n r)
    (cond [(= 0 r) (begin
                     (2d-set! 1 n 0)
                     1)]
          [(> r n) (begin
                     (2d-set! 0 n r)
                     0)]
          [(= n r) (begin
                     (2d-set! 1 n r)
                     1)]
          [else (let ([ans (vector-ref (vector-ref 2d-vec n) r)])
                  (if (number? ans) ans
                      (begin
                        (2d-set! (+ (f (- n 1) r) (f (- n 1) (- r 1))) n r)
                        (f n r))))]))
  (begin
    (init)
    f))