#lang racket
(define (string-upper-case? str)
    (define lst (string->list str))
    (cond [(null? lst) #t]
          [else (and (char-upper-case? (car lst))
                     (string-upper-case? (list->string (cdr lst))))]))


  (define (string-alphabetic? str)
    (define lst (string->list str))
    (cond [(null? lst) #t]
          [else (and (char-alphabetic? (car lst))
                     (string-alphabetic? (list->string (cdr lst))))]))

  (define (allowed-substitution? subst key)
    (cond [(utils:is-monoalphabetic? subst key) (not (number? (check-duplicates (utils:add-substitution subst key))))]
          [else #f]))

  ;; deciphers a code and matches it with a plain dic-word
  (define (match-code code dicword currkey) 
    (define plain-word (utils:decrypt currkey code))
    (define code-lst (string->list code))
    (define dicword-lst (string->list dicword))

    (cond [(string-upper-case? plain-word) #t]
          [(not (= (length code-lst) (length dicword-lst))) #f]
          [(null? code-lst) #t]
          [else
    
           (define inits-code (car code-lst))
           (define inits-dicword (car dicword-lst))
           (define inits-plain (car (string->list plain-word)))

           (cond [(char-upper-case? inits-plain) (cond [(char=? inits-plain inits-dicword) (match-code (list->string (cdr code-lst))
                                                                                                       (list->string (cdr dicword-lst)) currkey)]
                                                       [else #f])]
                 [else (cond [(match-code (list->string (cdr code-lst))
                                          (list->string (cdr dicword-lst))
                                          currkey)]
                             [else #f])])]))