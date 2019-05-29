#lang racket

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (string-upper-case? str)
  (define lst (string->list str))
  (cond [(null? lst) #t]
        [else (and (char-upper-case? (car lst))
                   (string-upper-case? (list->string (cdr lst))))]))
           
(define (allowed-substitution? subst key)
  (cond [(utils:is-monoalphabetic? subst key) (equal? #f (check-duplicates (remove* (list #\_)
                                                                                    (utils:add-substitution subst key))))]
        [else #f]))

(define (dictionary-closure key)
  ;; deciphers a deciphered code and matches it with a plain dic-word
  (define (match-code plain-word dicword currkey)
    (define plain-lst (string->list plain-word))
    (define dicword-lst (string->list dicword))
    (cond [(not (= (length plain-lst) (length dicword-lst))) #f]
          [(null? plain-lst) #t]
          [else
           (define inits-dicword (car dicword-lst))
           (define inits-plain (car plain-lst))

           (cond [(char-upper-case? inits-plain) (cond [(char=? inits-plain inits-dicword) (match-code (list->string (cdr plain-lst))
                                                                                                       (list->string (cdr dicword-lst)) currkey)]
                                                       [else #f])]
                 [else (cond [(allowed-substitution? (list (cons inits-dicword
                                                                 inits-plain))
                                                     currkey) (match-code (list->string (cdr plain-lst))
                                                                          (list->string (cdr dicword-lst)) (utils:add-substitution (list (cons inits-dicword
                                                                                                                                               inits-plain))
                                                                                                                                   currkey))]
                             [else #f])])]))

  ;; returns whether a word is fully deciphered
  (define (deciphered? str)
    (string-upper-case? (utils:decrypt key str)))
  
  ;; looks up a word (cipher) in dictionary and returns list : (list number of probable matches
  ;;                                                                 (list of answers)
  (define (look-up code)
    (define word (utils:decrypt key code))
    (define (helper diclst word ans-lst matches)
      (if (null? diclst) (list matches ans-lst)
          (let ([dicword (car diclst)])
            (cond [(= 2 matches) (list matches ans-lst)]
                  [(match-code word dicword key) (helper (cdr diclst) word (cons dicword ans-lst) (+ matches 1))]
                  [else (helper (cdr diclst) word ans-lst  matches)]))))

    (helper utils:dictionary word '() 0))
  
  (define (subst-generator key cipher plain)
    (define decrypted (utils:decrypt key cipher))
    (remove* '(())(remove-duplicates (map (lambda (x y)
                                            (cond [(char=? x y) '()]
                                                  [else (cons x y)]))
                                          (string->list plain) (string->list decrypted)))))


  ; main part of dic closure!
  
  (define (helper lst)
    (if (null? lst) key
        (cond [(deciphered? (utils:decrypt key (car lst))) (helper (cdr lst))]
              [else (define curr-code (utils:decrypt key (car lst)))
                    (define matches (look-up curr-code))
                    (define num-match (car matches))
                    (define words-matched (cadr matches))
    
                    (cond [(= num-match 0) #f]
                          [(= num-match 1) (let* ([subst (subst-generator key curr-code (car words-matched))]
                                                  [can-proceed (allowed-substitution? subst key)])
                                             (cond [can-proceed (dictionary-closure (utils:add-substitution subst key))]
                                                   [else (helper (cdr lst))]))]
                          [else (helper (cdr lst))])])))

  (helper utils:cipher-word-list))