#lang racket
(provide all-defined-out)
(require "list-comprehension.rkt")

(define ALL '(1 2 3 4 5 6 7 8 9 0))

(define (list->number lst)
  (define (op x y)
    (+ x (* 10 y)))
  (foldl op 0 lst))

(define solution (lc (map list (list "D" "E" "M" "N" "O" "R" "S" "Y") (list d e m n o r s y)) :
                     s <- (remove* (list ) ALL)
                     e <- (remove* (list s) ALL)
                     n <- (remove* (list s e) ALL)
                     d <- (remove* (list s e n) ALL)
                     m <- (remove* (list s e n d) ALL)
                     o <- (remove* (list s e n d m) ALL)
                     r <- (remove* (list s e n d m o) ALL)
                     y <- (remove* (list s e n d m o r) ALL)
                     @ (= (list->number (list m o n e y)) (+ (list->number (list m o r e))
                                                             (list->number (list s e n d))))))
                     