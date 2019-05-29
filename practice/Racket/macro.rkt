#lang racket
(define-syntax list-of-three
  (syntax-rules (@ <-)
    [(list-of-three b @ c ... <- d) (list b d c ...)]))

(define x (list-of-three  7 @ 1 2 3 4 5 <- 5))

(define-syntax my-cond
  (syntax-rules (< >)
    [(my-cond < bexp exp > rest ...) (if bexp exp (my-cond rest ...))]
    [(my-cond < exp >) exp]))

(define (fact n)
  (my-cond < (= n 1) 1 >
           < (* n (fact (- n 1))) >))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- drawn-from) (map (lambda (x) exp) drawn-from)]
    [(lc exp : @ guard) (if guard (list exp) '())]
    [(lc exp : @ guard other ...) (append* (lc (lc exp : other ...) : @ guard))]
    [(lc exp : var <- drawn-from other ...) (append* (lc (lc exp : other ...) : var <- drawn-from))]))

(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements)
     (begin
       init
       (define (iterate)
         (if condition (begin
                         statements
                         step
                         (iterate))
             (void)))
       (iterate))]))

(define-syntax while
  (syntax-rules (:)
    [(while bool : statements) (cond [bool (begin statements (while bool : statements))])]))

(define-syntax while2
  (syntax-rules ()
    [(while bool statements) (begin
                               (define (iterate)
                                 (cond [bool (begin
                                               statements
                                               (iterate))]))
                               (iterate))]))