#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)


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
    (define plain-word code)
    (define code-lst (string->list code))
    (define dicword-lst (string->list dicword))

    (cond [(not (= (length code-lst) (length dicword-lst))) #f]
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

  ;; returns whether a word is fully deciphered
  (define (deciphered? str)
    (string-upper-case? (utils:decrypt key1 str)))
  
  ;; looks up a word (cipher) in dictionary and returns list : (list number of probable matches
  ;;                                                                 (list of answers)
  (define (look-up word)
    (define (helper diclst word ans-lst matches)
      (if (null? diclst) (list matches ans-lst)
          (let ([dicword (car diclst)])
            (cond [(= 2 matches) (list matches ans-lst)]
                  [(match-code word dicword key1) (helper (cdr diclst) word (cons dicword ans-lst) (+ matches 1))]
                  [else (helper (cdr diclst) word ans-lst  matches)]))))
    (helper utils:dictionary word '() 0))
  

  (define key1 key-after-dictionary-closure)
  (define list-of-first-6 (take key1 6))

  (define key-word-lst (map (lambda (x) (integer->char (- (char->integer x) 32)))
                            list-of-first-6))
  (define key-word (list->string key-word-lst))
  (define matches (look-up key-word))
  (define num-matches (car matches))
  (define word-matches (cadr matches))


  (cond [(= num-matches 0) #f]
        [(= num-matches 1) (utils:encryption-key (car word-matches))]
        [else key1]))
