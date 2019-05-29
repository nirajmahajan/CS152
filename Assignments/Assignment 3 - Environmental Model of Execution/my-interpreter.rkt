#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                  (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                  (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
  (begin
    (set! framenumber (+ 1 framenumber))
    (frame (- framenumber 1) hashtable parent)))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
  (define worthless 3)
  (define (helper lst)
    (cond [(null? lst) (set! worthless 4)]
          [else (begin
                  (processdef (car lst) (last stack))
                  (helper (cdr lst)))]))
  (match prog
    [(pgm deflist) (begin (helper deflist)
                          (return-value-of-main (last stack)))]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (let ([curr-bindings (frame-bindings fr)])
                     (begin
                       (hash-set! curr-bindings v/f (eval-exp exp))
                       (set-frame-bindings! fr curr-bindings)))]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (begin
    (set! stack '())
    (set! framenumber 0)
    (push (createframe (make-hash '()) (emptyframe)))
    (hash-ref! (frame-bindings frame) 'main "main not found")))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (eval-exp (my-search exp (top)))]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var _) (closure exp (top))]
                [(app fun explist) (cond [(lam? fun) (apply-lambda fun explist (top))]
                                          [(closure? fun) (let* ([lamd (closure-lambda fun)]
                                                                 [parent (closure-frame fun)])
                                                            (apply-lambda lamd explist parent))]
                                          [(app? fun) (eval-exp (app (eval-exp fun) explist))]
                                          [else (let ([closure (my-search fun (top))])
                                                  (cond [(equal? #f closure) (error "Symbol not found")]
                                                        [else (apply-lambda (closure-lambda closure) explist (closure-frame closure))]))])]
                [(iff cond exp1 exp2) (if (eval-exp cond) (eval-exp exp1) (eval-exp exp2))]
                [(sett var exp1) (process-set! var exp1)]
                [(lett deflist exp2) (process-lett deflist exp2)]
                [(lets deflist exp2) (process-lets deflist exp2)]
                [(beginexp explist) (process-beginexp explist)]
                [(defexp deflist exp) (process-defexp deflist exp)]
                ((closure lambda frame) exp)
                [(debugexp) (begin
                 (vector-set! stacks stacksindex stack)
                 (set! stacksindex (+ 1 stacksindex)))])]))

;; An auxilliary function that processes def exp
(define (process-defexp deflist exp)
  (define (helper lst)
    (match lst
      ['() (eval-exp exp)]
      [(cons a rest) (begin
                       (processdef a (top))
                       (helper (cdr lst)))]))
  (helper deflist))

;; An Auxilliary function that processes let
(define (process-lett deflist exp2)
  (let ([lst-bindings (map (lambda (x) (cons (def-var/fun x) (eval-exp (def-exp x)))) deflist)])
    (begin
      (push (createframe (make-hash lst-bindings) (top)))
      (let ([ans (eval-exp exp2)])
        (pop)
        ans))))

;;An auxilliary function that processes a set! expression
(define (process-set! var exp)
  (define fr (search var (top)))
  (hash-set! (frame-bindings fr) var (eval-exp exp)))

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist
    [(cons a '()) (eval-exp a)]
    [(cons a  b) (begin (eval-exp a)
                        (process-beginexp (cdr explist)))]))

;; pops n frames
(define (npop n)
  (define worthless 3)
  (define (helper x)
    (cond [(= x n) (set! worthless 4)]
          [else (begin
                  (pop)
                  (helper (+ x 1)))]))
  (helper 0))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (cond [(null? deflist) (eval-exp exp)]
        [else (process-lett (list (car deflist)) (lets (cdr deflist) exp))]))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (cond [(emptyframe? fr) (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@"))]
        [else (let ([parent (frame-parent fr)])
                (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
                       (displayln fr)
                       (print-current-environment parent)))]))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
  (cond [(emptyframe? fr) (emptyframe)]
        [else (let ([parent (frame-parent fr)])
                (cond [(hash-has-key? (frame-bindings fr) sym) fr]
                      [else (search sym parent)]))]))


;; returns value stored in symbol
(define (my-search sym fr)
  (let ([result (search sym fr)])
    (cond [(emptyframe? result) (begin (displayln sym) (error (string-append "Symbol" " not found")))]
          [else (hash-ref (frame-bindings result) sym)])))

;; applies a lam on an exp list in the curr frame
(define (apply-lambda lam explst parent)
  (let* ([evaluated-explist (map eval-exp explst)]
         [lambda-parameters (lam-varlist lam)]
         [lambda-exp (lam-exp lam)]
         [new-bindings-list (map (lambda (x y) (cons x y)) lambda-parameters evaluated-explist)]
         [new-hash-tbl (make-hash new-bindings-list)]
         [new-frame (createframe new-hash-tbl parent)])
    (begin
      (push new-frame)
      (define ans (eval-exp lambda-exp))
      (pop)
      ans)))
    

(define (cleanup)
  (set!  stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))


               


  