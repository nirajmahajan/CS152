#lang racket
(require "q1.rkt")
(define (dropwhile lst pred)
  (define (op x y)
    (cond [(not (null? y)) (append y (list x))]
          [(pred x) (list x)]
          [else y]))
  (foldl op '() lst))

(define adv-processor%
  (class processor%
    (init size)
    (init register)
    (init instructions)
    (super-new [mem-size size]
               [reg-names register]
               [inst-set instructions])
    (define all-prog '())
    (define/override (reg-value reg) (super reg-value reg))
    (define/override (mem-print) (super mem-print))
    (define/override (reg-print) (super reg-print))
    (define/override (reg-write reg val) (super reg-write reg val))
    (define/override (execute prog)
      (cond [(null? prog) "Done"]
            [else (begin
                    (cond [(> (length prog) (length all-prog)) (set! all-prog prog)])
                    (let* ([curr-inst (car prog)]
                           [pneumonic (car curr-inst)])
                      (cond [(or (equal? pneumonic 'label) (equal? pneumonic 'goto) (equal? pneumonic 'ifzero) (equal? pneumonic 'decr))
                             (begin (display "------\nInstruction:  ")
                                    (displayln curr-inst)
                                    (cond [(equal? pneumonic 'label) (begin
                                                                       (mem-print)
                                                                       (reg-print)
                                                                       (displayln "------\n\n")
                                                                       (execute (cdr prog)))]
                                          [(equal? pneumonic 'goto) (let ([label-num (cadr curr-inst)])
                                                                      (begin
                                                                        (mem-print)
                                                                        (reg-print)
                                                                        (displayln "------\n\n")
                                                                        (execute (dropwhile all-prog (lambda (x) (and (equal? (car x) 'label) (equal? (cadr x) label-num)))))))]
                                          [(equal? pneumonic 'ifzero) (let* ([reg1 (cadr curr-inst)]
                                                                             [reg1-val (super reg-value reg1)]
                                                                             [label-num (caddr curr-inst)])
                                                                        (cond [(= 0 reg1-val) (begin
                                                                                                (mem-print)
                                                                                                (reg-print)
                                                                                                (displayln "------\n\n")
                                                                                                (execute (cons (list 'goto label-num) (cdr prog))))]
                                                                              [else (begin
                                                                                      (mem-print)
                                                                                      (reg-print)
                                                                                      (displayln "------\n\n")
                                                                                      (execute (cdr prog)))]))]
                                          [(equal? pneumonic 'decr) (begin
                                                                      (let* ([reg (cadr curr-inst)]
                                                                             [reg-val (reg-value reg)])
                                                                        (reg-write reg (- reg-val 1)))
                                                                      (begin
                                                                        (mem-print)
                                                                        (reg-print)
                                                                        (displayln "------\n\n")
                                                                        (execute (cdr prog))))]))]
                            [else (begin (super execute (list (car prog)))
                                         (execute (cdr prog)))])))]))))

(define sample-prog (list
                     (list 'load 'r1 1)
                     (list 'load 'r2 5)
                     (list 'label 1)
                     (list 'ifzero 'r2 2)
                     (list 'mul 'r1 'r2)
                     (list 'decr 'r2)
                     (list 'goto 1)
                     (list 'label 2)
                     (list 'store 0 'r1)))

(define cs152-2019
  (new adv-processor%
       [size 32]
       [register (list `r1 `r2 `r3 `r4)]
       [instructions (list (list 'add (lambda (val1 val2)
                                        (+ val1 val2)))
                           (list 'mul (lambda (val1 val2)
                                        (* val1 val2)))
                           (list 'incr (lambda (val1)
                                         (+ 1 val1))))]))