#lang racket
(define (rem-last lst) (reverse (cdr (reverse lst))))

(define (match-pattern s lst)
  (matcher (string->list s) lst))

;; augments an element into a list if no repetetions
(define (augment elem l)  (let ([elem-sym (car elem)]
        [elem-str (cdr elem)])
    (define (helper lst)
      (cond [(null? lst) (cons elem l)]
            [(and (equal? elem-sym (caar lst)) (equal? elem-str (cdar lst))) l]
            [(equal? elem-sym (caar lst)) #f]
            [else (helper (cdr lst))]))
  (helper l)))

(define (matcher str-l sym-lst)
  (define (helper l1 l2)
    (let ([match-result (matcher l2 (cdr sym-lst))])
      (displayln (list l1 l2 (cdr sym-lst) match-result))
      (cond [(and (null? l2) (equal? #f match-result)) #f]
            [(equal? #f match-result) (helper (append l1 (list (car l2))) (cdr l2))]
            [else (let ([aug-result (augment (cons (car sym-lst) (list->string l1)) match-result)])
                    (cond [(equal? #f aug-result) (helper (append l1 (list (car l2))) (cdr l2))]
                          [else aug-result]))])))

  (cond [(and (null? sym-lst) (null? str-l)) '()]
        [(or (null? sym-lst) (null? str-l)) #f]
        [else (helper (list (car str-l)) (cdr str-l))]))