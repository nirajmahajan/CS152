#lang racket
(define acc-lst '())
(define card-lst '())

(define account%
  (class object%
    (super-new)
    (init passwd)
    (init balance)
    (init interest-rate)
    (define password passwd)
    (define bal balance)
    (define rate interest-rate)

    (define/public (withdraw amt pass)
      (cond [(equal? pass password) (cond [(>= bal amt) (set! bal (- bal amt))]
                                          [else "Insufficient Balance"])]
            [else "Incorrect Passwd"]))
    (define/public (deposit amt pass) (withdraw (- amt) pass))
    (define/public (show pass)
      (cond [(equal? pass password) bal]
            [else "Incorrect Passwd"]))
    (define/public (pay-interest-tick)
      (pay-interest))
    (define (pay-interest)
      (set! bal (* bal (+ 1 rate))))))

(define credit-card%
  (class object%
    (super-new)
    (init acct)
    (init passwd)
    (define account acct)
    (define password passwd)
    (define outstanding 0)
    
    (define/public (make-purchase amt)
      (set! outstanding (+ amt outstanding)))
    (define/public (clear-outstanding-amount)
      (begin
        (send account withdraw outstanding password)
        (set! outstanding 0)))
    (define/public (apply-penalties-tick)
      (apply-penalties))
    (define (apply-penalties)
      (set! outstanding (* outstanding 1.2)))))

(define joint-account%
  (class object%
    (init account)
    (init passwd)
    (init new-passwd)
    (super-new)
    (field [password passwd]
           [new-password new-passwd]
           [acc account])
    
    (define/public (withdraw amt passwd)
      (cond [(equal? passwd new-password) (send acc withdraw password)]
            [else "Incorrect Passwd"]))
    (define/public (deposit amt pass) (withdraw (- amt) pass))
    (define/public (show pass)
      (cond [(equal? pass new-password) (send acc show password)]
            [else "Incorrect Passwd"]))))

(define timer%
  (class object%
    (super-new)

    (define/public (process-accounts-tick)
      (begin
        (map (lambda (x) (send x pay-interest-tick)) acc-lst)
        (void)))

    (define/public (process-credit-card-tick)
      (foldl (lambda (x y) (send x apply-penalties-tick)) (void) card-lst))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define this-timer (new timer%))
(define my-account (new account% (passwd "amitabha")
                        (balance 1000)
                        (interest-rate 0.05)))
(define my-card (new credit-card% (acct my-account)
                     (passwd "amitabha")))
(define another-account (new account% (passwd "abcd")
                             (balance 5000)
                             (interest-rate 0.1)))
(define another-card (new credit-card% (acct another-account)
                          (passwd "abcd")))
(define jnt-account (new joint-account% (account my-account)
                         (passwd "amitabha") (new-passwd "xyz")))
(set! acc-lst (list my-account another-account))
(set! card-lst (list my-card another-card))

(send my-account withdraw 200 "amitabha")
;(send jnt-account withdraw 200 "amitabha")
(send my-account show "amitabha")
(send this-timer process-accounts-tick)
(send my-account show "amitabha")
(send my-card make-purchase 100)
(send this-timer process-credit-card-tick)
;(get-field pending-payment my-account)
(send my-card clear-outstanding-amount)
(send my-account show "amitabha")
(send my-card make-purchase 50)
(send another-card make-purchase 1000)
(send another-card clear-outstanding-amount)
(send this-timer process-credit-card-tick)
(send my-account show "amitabha")
(send my-card clear-outstanding-amount)
(send another-account show "abcd")
(send my-account show "amitabha")

    




