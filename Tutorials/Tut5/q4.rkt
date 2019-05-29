#lang racket
(define and-gate%
  (class object%
    (init-field a1)
    (init-field a2)
    (init-field output)
    (init-field [init-tick 'undefined])
    (super-new)

    (define/public (respond tick-number)
      (cond [(or (equal? 'undefined (send a1 get-signal))
                 (equal? 'undefined (send a2 get-signal))) (void)]
            [(equal? 'undefined init-tick) (set! init-tick tick-number)]
            [(equal? tick-number (+ 3 init-tick)) (send output set-value! (and (send a1 get-signal)
                                                                                        (send a2 get-signal)))]))))

(define delay%
  (class object%
    (init-field input)
    (init-field output)
    (init-field [init-tick 'undefined])
    (super-new)

    (define/public (respond tick-number)
      (cond [(equal? 'undefined (send input get-signal)) (void)]
            [(equal? 'undefined init-tick) (set! init-tick tick-number)]
            [(equal? tick-number (+ 1 init-tick)) (send output set-value! (send input get-signal))]))))

(define probe%
  (class object%
    (init-field probe-name)
    (init-field connector)
    (init-field [status #f])
    (super-new)

    (define/public (respond tick-number)
      (cond [(or (equal? 'undefined (send connector get-signal)) (equal? status #t)) (void)]
            [else
             (begin
               (set! status #t)
               (display "Tick Number = ")
               (display tick-number)
               (newline)
               (display "Probe Name = ")
               (display probe-name)
               (newline)
               (display "Signal = ")
               (display (send connector get-signal))
               (newline)
               (newline))]))))

(define connector%
  (class object%
    (init-field [signal 'undefined])
    (super-new)

    (define/public (get-signal) signal)
    (define/public (set-value! val) (set! signal val))))

(define scheduler\%
  (class object%
    (init-field [wire-lst '()])
    (init-field [gate-lst '()])
    (init-field [curr-time 0])

    (super-new)

    (define/public (tick)
      (begin (set! curr-time (+ 1 curr-time))
             (map (lambda (x) (send x respond curr-time)) gate-lst)
             (void)))))

(define a (make-object connector%))
(define b (make-object connector%))
(define c (make-object connector%))
(define d (make-object connector%))
(define e (make-object connector%))
(define f (make-object connector%))
(define del (make-object delay% a d))
(define and1 (make-object and-gate% d b e))
(define and2 (make-object and-gate% e c f))
(define pa (make-object probe% "probe a" a))
(define pb (make-object probe% "probe b" b))
(define pc (make-object probe% "probe c" c))
(define pd (make-object probe% "probe d" d))
(define pe (make-object probe% "probe e" e))
(define pf (make-object probe% "probe f" f))
(define sch (make-object scheduler\% (list a b c d e f)
              (list del and1 and2 pa pb pc pd pe pf)))
(send a set-value! #t)
(send b set-value! #f)
(send c set-value! #t)
(send sch tick)
(send sch tick)
(send sch tick)
(send sch tick)
(send sch tick)
(send sch tick)
(send sch tick)
(send sch tick)






    