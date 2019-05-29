#lang racket
(define (minchange n r)
  (cond [(= 0 n) 0]
        [(= 1 r) n]
        [(> r n) (minchange n (next r))]
        [else (min (+ 1 (minchange (- n r) r))
                   (minchange n (next r)))]))

(define (next r)
  (cond [(= r 50) 25]
        [(= r 25) 20]
        [(= r 20) 10]
        [(= r 10) 5]
        [(= r 5) 2]
        [(= r 2) 1]))

(define (index r)
  (cond [(= r 50) 0]
        [(= r 25) 1]
        [(= r 20) 2]
        [(= r 10) 3]
        [(= r 5) 4]
        [(= r 2) 5]
        [(= r 1) 6]))  

(define (make-2d-vec r c val)
  (build-vector r (lambda (x) (make-vector c val))))

(define (2d-vec-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vec-set! vec r c val)
  (let ([old-vec (vector-ref vec r)])
    (begin
      (vector-set! old-vec c val)
      (vector-set! vec r old-vec))))

(define (memo-minchange range)
  (define min-vec (make-2d-vec (+ 1 range) 7 #f))
  (define (minchange n r)
    (let ([ans (2d-vec-ref min-vec n (index r))])
      (cond [(number? ans) ans]
            [(= 0 n) (begin
                       (2d-vec-set! min-vec n (index r) 0)
                       (minchange n r))]
            [(= r 1) (begin
                       (2d-vec-set! min-vec n (index r) n)
                       (minchange n r))]
            [(= 0 (remainder n r)) (begin
                                     (2d-vec-set! min-vec n (index r) (/ n r))
                                     (minchange n r))]
            [(< n r) (begin
                       (2d-vec-set! min-vec n (index r) (minchange n (next r)))
                       (minchange n r))]
            [else (begin
                    (2d-vec-set! min-vec n (index r) (min (+ 1 (minchange (- n r) r))
                                                          (minchange n (next r))))
                    (minchange n r))])))
  minchange)