#lang racket
; returns the length of a number
(define (len n)
  (if (= n 0) 1
  (len-helper n 1 0)))

(define (len-helper n c pow-ten)
  (cond [(> c n) pow-ten]
        [else (len-helper n (* c 10) (+ 1 pow-ten))]))


(define (kmult a b n)
  (cond [(and (= 1 (len a)) ( = 1 (len b))) (* a b)]
        [else  (let* ([na n]
                      [nb (len b)]
                      [alen1 (quotient na 2)]
                      [alen2 (- na alen1)]
                      [blen1 (quotient nb 2)]
                      [blen2 (- nb blen1)]
                      [a1pow10 (expt 10 alen1)]
                      [a2pow10 (expt 10 alen2)]
                      [b1pow10 (expt 10 blen1)]
                      [b2pow10 (expt 10 blen2)]
                      [a1 (quotient a a2pow10)]
                      [a2 (remainder a a2pow10)]
                      [b1 (quotient b b2pow10)]
                      [b2 (remainder b b2pow10)])
                 (+ (* a2pow10 b2pow10 (kmult a1 b1 alen1))
                    (kmult a2 b2 alen2)
                    (* a2pow10 (kmult a1 b2 alen1))
                    (* b2pow10 (kmult b1 a2 blen1))))]))
                    
                 