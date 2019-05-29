#lang racket
(provide processor%)

(define processor%
  (class object%
    (super-new)
    (init mem-size)
    (init reg-names)
    (field [memory (new memory% [size mem-size])])
    (field [reg-bank (new register-bank% [names reg-names])])
    (init-field inst-set)

    (define/public (reg-value reg)
      (send reg-bank read reg))
    (define/public (mem-print)
      (send memory print))
    (define/public (reg-print)
      (send reg-bank print))
    (define/public (reg-write reg val)
      (send reg-bank write reg val))
    
    (define/public (execute prog)
      (cond [(null? prog) "Done"]
            [else (begin
                    (let* ([curr-inst (car prog)]
                           [no-of-operands (- (length curr-inst) 1)]
                           [pneumonic (car curr-inst)])
                      (begin
                        (displayln "------")
                        (display "Instruction :  ")
                        (displayln curr-inst)
                        (cond [(equal? pneumonic 'store) (send memory write (cadr curr-inst) (send reg-bank read (caddr curr-inst)))]
                              [(equal? pneumonic 'load) (send reg-bank write (cadr curr-inst) (caddr curr-inst))]
                              [else (let ([op (cadr (assoc pneumonic inst-set))])
                                      (cond [(= 1 no-of-operands) (let* ([reg1 (cadr curr-inst)]
                                                                         [val1 (op (send reg-bank read reg1))])
                                                                    (send reg-bank write reg1 val1))]
                                            [(= 2 no-of-operands) (let* ([reg1 (cadr curr-inst)]
                                                                         [reg2 (caddr curr-inst)]
                                                                         [val1 (send reg-bank read reg1)]
                                                                         [val2 (send reg-bank read reg2)]
                                                                         [ans (op val1 val2)])
                                                                    (send reg-bank write reg1 ans))]
                                            [else (error "Unexpected instruction")]))])
                        (send reg-bank print)
                        (send memory print)
                        (displayln "------\n\n")))
                    (execute (cdr prog)))]))))

(define storable%
  (class object%
    (super-new)
    (define/public (read) (error "should be overridden"))
    (define/public (write) (error "should be overridden"))
    (define/public (print) (error "should be overridden"))))

(define memory%
  (class storable%
    (super-new)
    (init size)
    (field [mem (make-vector size 0)])
    (define/override (read pos)
      (vector-ref mem pos))
    (define/override (write pos val)
      (vector-set! mem pos val))
    (define/override (print)
      (displayln mem))))

(define register-bank%
  (class storable%
    (super-new)
    (init names)
    (field [register (make-hash (map (lambda (x) (cons x 0)) names))])
    (define/override (read key)
      (hash-ref register key))
    (define/override (write key val)
      (hash-set! register key val))
    (define/override (print)
      (displayln (hash->list register)))))

(define sample-prog (list
                     (list 'load 'r2 15) ;Inst. 1 - Assign to register r2 the number 15
                     (list 'load 'r1 5) ;Inst. 2 - Assign to register r1 the number 5
                     (list 'add 'r1 'r2) ;Inst. 3 - Add r1 and r2, the result goes to r1
                     (list 'load 'r3 2) ;Inst. 4 - Assign to register r3 the number 2
                     (list 'incr 'r3) ;Inst. 5 - Increment contents of register r3 by 1
                     (list 'mul 'r3 'r2) ;Inst. 6 - Multiply r3 and r2, result goes to r3
                     (list 'add 'r1 'r3) ;Inst. 7 - Add r1 and r3, the result goes to r1
                     (list 'store 1 'r1))) ;Inst. 8 - Store r1 in memory location 1

(define cs152-2019
  (new processor%
       [mem-size 32]
       [reg-names (list `r1 `r2 `r3 `r4)]
       [inst-set (list (list 'add (lambda (val1 val2)
                                    (+ val1 val2)))
                       (list 'mul (lambda (val1 val2)
                                    (* val1 val2)))
                       (list 'incr (lambda (val1)
                                     (+ 1 val1))))]))
