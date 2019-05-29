#lang racket
(require racket/mpair)

(define inverter%
  (class object%
    (init-field input)
    (init-field output)
    (define inverter-delay 1)
    
    (super-new)

    (define (logical-not s)
      (cond [(equal? s 0) 1]
            [(equal? s 1) 0]
            [else 'undefined]))

    (define (invert-input)
      (let ([new-value (logical-not (send input get-signal))])
        (send the-queue add-to-queue-after-delay! inverter-delay (lambda () (send output set-signal! new-value)))))

    (send input add-action! invert-input)))

(define and-gate%
  (class object%
    (init-field a1)
    (init-field a2)
    (init-field output)
    (define and-gate-delay 1)
    (super-new)

    (define (logical-and s1 s2)
      (match (list s1 s2)
        ['(0 0) 0]
        ['(0 1) 0]
        ['(1 0) 0]
        ['(1 1) 1]
        [else 'undefined]))
    
    (define (and-action-procedure)
      (let ([new-value (logical-and (send a1 get-signal)
                                    (send a2 get-signal))])
        (send the-queue add-to-queue-after-delay! and-gate-delay
              (lambda () (send output set-signal! new-value)))))

    (send a1 add-action! and-action-procedure)
    (send a2 add-action! and-action-procedure)))

(define or-gate%
  (class object%
    (init-field a1)
    (init-field a2)
    (init-field output)
    (define or-gate-delay 2)
    (super-new)

    (define (logical-or s1 s2)
      (match (list s1 s2)
        ['(0 0) 0]
        ['(0 1) 1]
        ['(1 0) 1]
        ['(1 1) 1]
        [else 'undefined]))
    
    (define (or-action-procedure)
      (let ([new-value (logical-or (send a1 get-signal)
                                    (send a2 get-signal))])
        (send the-queue add-to-queue-after-delay! or-gate-delay
              (lambda () (send output set-signal! new-value)))))

    (send a1 add-action! or-action-procedure)
    (send a2 add-action! or-action-procedure)))

(define probe%
  (class object%
    (init-field wire)
    (init-field queue)
    (super-new)

    (send wire add-action!
          (lambda ()
            (newline)
              (display "time = ")
              (display (send queue get-time ))
              (newline)
              (display "wire-name = ")
              (display (send wire get-name))
              (newline)
              (display "New-value = ")
              (display (send wire get-signal))
              (newline)
              (newline)))))

(define wire%
  (class object%
   (init-field name)
   (init-field [signal-value 'undefined])
   (init-field [actions '()])
   (super-new)

   (define/public (get-signal) signal-value)
   (define/public (get-name) name)

   (define/public (add-action! act)
     (set! actions (cons act actions)))
   
   (define/public (set-signal! val)
     (cond [(not (equal? signal-value val))
            (begin (set! signal-value val)
                   (execute-all actions))]))

   (define/public (execute-all action-lst)
     (cond [(null? action-lst) (void)]
           [else (begin
                   ((car action-lst))
                   (execute-all (cdr action-lst)))]))))

(define event-queue%
  (class object%
    (init-field [current-time 0])
    (init-field [farthest-event-time 0])
    (init-field [queue (make-vector 200 '())])
    (super-new)

    (define/public (add-to-queue-after-delay! delay action)
      (let ([insertion-time (+ (send the-queue get-time) delay)])
        (begin
          (vector-set! queue insertion-time
                       (mappend! (vector-ref queue insertion-time)
                                 (mlist action)))
          (cond [(> insertion-time farthest-event-time) (set! farthest-event-time insertion-time)]))))

    (define/public (queue-empty?)
      (> current-time farthest-event-time))

    (define/public (increment-time!)
      (set! current-time (+ 1 current-time)))

    (define/public (next-item-in-queue)
      (vector-ref queue current-time))

    (define/public (get-time) current-time)

    (define/public (propagate)
      (cond [(queue-empty?) 'done]
            [else (let ([action-list (next-item-in-queue)])
                    (execute! action-list)
                    (increment-time!)
                    (propagate))]))

    (define (execute! action-lst)
      (cond [(null? action-lst) (void)]
            [else (begin
                    ((mcar action-lst))
                    (execute! (mcdr action-lst)))]))))

(define half-adder%
  (class object%
    (init-field a)
    (init-field b)
    (init-field s)
    (init-field c)
    (super-new)
    
    (define d (make-object  wire% "d"))
    (define e (make-object  wire% "e"))
    (make-object probe%  d the-queue)
    (make-object probe%  e the-queue)
    (make-object or-gate% a b d)
    (make-object and-gate% a b c)
    (make-object inverter% c e)
    (make-object and-gate% d e s)))

(define a (make-object  wire% "a"))
(define b (make-object  wire% "b"))
(define s (make-object  wire% "s"))
(define c (make-object  wire% "c"))
;;End of class creation




;;Object creation

(define the-queue (make-object event-queue%))
(make-object half-adder% a b s c)

(get-field  actions c)

(make-object probe%  a the-queue)
(make-object probe%  b the-queue)
(make-object probe%  s the-queue)
(make-object probe%  c the-queue)


(send the-queue add-to-queue-after-delay! 0 (lambda () (send b set-signal! 1)))
(send the-queue add-to-queue-after-delay! 0 (lambda () (send a set-signal! 1)))


(send the-queue  propagate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define full-adder%
  (class object%
   
    (init-field a)
    (init-field b)
    (init-field ci)
    (init-field s)
    (init-field co)
    
    (define d (make-object  wire% "d"))
    (define e (make-object  wire% "e"))
    (define f (make-object  wire% "f"))
    
    (super-new)
    
    (make-object half-adder% b ci d e)
    (make-object half-adder% a d s f)
    (make-object or-gate%  f e co)))


    



    





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  



