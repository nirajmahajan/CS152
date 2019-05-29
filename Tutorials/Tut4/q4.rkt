#lang racket
(define (make-account balance password)
  (define (credit amount)
    (begin
      (set! balance (+ balance amount))
      balance))

  (define (debit amount)
    (cond [(> amount balance) "Insufficient Balance"]
          [else (begin
                  (set! balance (- balance amount))
                  balance)]))

  (define (security key action)
    (cond [(equal? key password) (cond [(equal? action 'withdraw) debit]
                                       [(equal? action 'deposit) credit]
                                       [else "Invalid Request"])]
          [else (lambda (x) "Invalid Password")]))
  security)

(define (make-joint acc1 acc1-pass new-pass)
  (define (security key action)
    (cond [(equal? key new-pass) (cond [(equal? action 'withdraw) (acc1 acc1-pass 'withdraw)]
                                       [(equal? action 'deposit) (acc1 acc1-pass 'deposit)]
                                       [else "Invalid Request"])]
          [else (lambda (x) "Invalid Password")]))
  security)
