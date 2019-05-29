#lang racket

(define (make-2d-vec r c val)
  (build-vector r (lambda (x) (make-vector c val))))

(define (2d-vec-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vec-set! vec r c val)
  (let ([old-vec (vector-ref vec r)])
    (begin
      (vector-set! old-vec c val)
      (vector-set! vec r old-vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lcs-memo row-range column-range)
  (define mv (make-2d-vector row-range column-range #F))
  (define (lcsh lst1 lst2 n1 n2)
    (let ([ans (2d-vector-ref mv n1 n2)])
      (cond [(number? ans) ans]
            [else (let ([elem1 (list-ref lst1 n1)]
                        [elem2 (list-ref lst2 n2)])
                    (cond [(= elem1 elem2) (begin
                                             (2d-vector-set! mv n1 n2 (cons elem1 (lcsh lst1 lst2 (+ 1 n1) (+ 1 n2))))
                                             (lcsh lst1 lst2 n1 n2))]