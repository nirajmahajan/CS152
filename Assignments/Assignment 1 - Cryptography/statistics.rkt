#lang racket

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         "list-comprehension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:
         sort-list-according-to          ;; sorts a given list 'sample' according to the order in which the elements occur in 'lst'
         zip
         
         ;; my-fundoo-analysis
         two-lettered-words-starting-with      ;; returns a list of chars which are the first letter of two-lettered-words (arranged)
         )

; given elements of list are pairs (char freq)

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  ; sorts according to frequencies
  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))

  ;inserts an element in list
  ;list should contain pairs ie '(char . freq)
  (define (insert elem l)
    (if (not (char-alphabetic? elem)) l
        (cond [(null? l) (list (cons elem 1))]
              [(char=? (caar l) elem) (cons (cons elem (+ 1 (cdar l))) (cdr l))]
              [else (cons (car l) (insert elem (cdr l)))])))

  ;adds cipher text to list
  (define (add-cipher cipherlist l)
    (cond [(null? cipherlist) l]
          [else (add-cipher (cdr cipherlist) (insert (car cipherlist) l))]))

  (map car (my-sort (add-cipher (string->list ciphertext) '()))))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
  ; sorts according to frequencies
  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))

  ;; returns a list of two lettered sub strings of the given string->list of a word 
  (define (word-bigram lst)
    (match lst
      [(cons a '()) '()]
      [(cons a (cons b '())) (list (list->string (list a b)))]
      [(cons a (cons b lst)) (cons (list->string (list a b)) (word-bigram (cons b lst)))]))

  (define (string-alphabetic? str)
    (define (helper lst)
      (cond [(null? lst) #t]
            [(not (char-alphabetic? (car lst))) #f]
            [else (helper (cdr lst))]))
    (helper (string->list str)))

  ;; adds a bigram to a list of pairs (cons bigrm freq)
  (define (insert elem l)
    (if (not (string-alphabetic? elem)) l
        (cond [(null? l) (list (cons elem 1))]
              [(string=? (caar l) elem) (cons (cons elem (+ 1 (cdar l))) (cdr l))]
              [else (cons (car l) (insert elem (cdr l)))])))
  
  ;; merges a list of strings into list of strings wth frequency
  (define (merge lst freqlst)
    (cond [(null? lst) freqlst]
          [else (merge (cdr lst) (insert (car lst) freqlst))]))

  (define (helper lst)
    (cond [(null? lst) '()]
          [else (merge (word-bigram (string->list (car lst)))
                       (helper (cdr lst)))]))

  (map car (my-sort (helper cipher-word-list))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (cipher-neighbourhood cipher-bigrams-list mode))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define blank-list (build-list 26 (lambda (x) (cons (integer->char (+ x 97)) 0))))

  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))
  
  ; inserts a cons (char freq) in a list
  (define (insert elem l)
    (if (not (char-alphabetic? elem)) l
        (cond [(null? l) (list (cons elem 1))]
              [(char=? (caar l) elem) (cons (cons elem (+ 1 (cdar l))) (cdr l))]
              [else (cons (car l) (insert elem (cdr l)))])))
  
  ; reads a word and updates its bigrams in the given list
  (define (read-word str mode lst)
    (define wrdlist (string->list str))
    (cond [(equal? mode 'predecessor) (match wrdlist
                                        ['() lst]
                                        [(cons a '()) lst]
                                        [(cons a (cons b rest)) (read-word (list->string (cdr wrdlist)) mode (insert a lst))])]
          [(equal? mode 'successor) (match wrdlist
                                      ['() lst]
                                      [(cons a '()) lst]
                                      [(cons a (cons b rest)) (read-word (list->string (cdr wrdlist)) mode (insert b lst))])]
          [else (match wrdlist
                  ['() lst]
                  [(cons a '()) lst]
                  [(cons a (cons b rest)) (read-word (list->string (cddr wrdlist)) mode (insert a (insert b lst)))])]))

  (define (helper lst ans)
    (cond [(null? lst) ans]
          [else (helper (cdr lst) (read-word (car lst) mode ans))]))

  (my-sort (helper cipher-word-list blank-list)))



;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  ; sorts according to frequencies
  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))

  (define (insert elem l)
    (cond [(null? l) (cons (cons elem 1) l)]
          [(string=? elem (caar l)) (cons (cons elem (+ 1 (cdar l))) (cdr l))]
          [else (cons (car l) (insert elem (cdr l)))]))

  (define (helper lst)
    (cond [(null? lst) '()]
          [(= 1 (length (string->list (car lst)))) (insert (car lst) (helper (cdr lst)))] 
          [else (helper (cdr lst))]))

  (map (lambda (x) (car (string->list (car x)))) (my-sort (helper cipher-word-list))))
  

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  ; sorts according to frequencies
  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))

  (define (insert elem l)
    (cond [(null? l) (cons (cons elem 1) l)]
          [(string=? elem (caar l)) (cons (cons elem (+ 1 (cdar l))) (cdr l))]
          [else (cons (car l) (insert elem (cdr l)))]))

  (define (helper lst)
    (cond [(null? lst) '()]
          [(= 2 (length (string->list (car lst)))) (insert (car lst) (helper (cdr lst)))] 
          [else (helper (cdr lst))]))

  (map car (my-sort (helper cipher-word-list))))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  ; sorts according to frequencies
  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))

  (define (insert elem l)
    (cond [(null? l) (cons (cons elem 1) l)]
          [(string=? elem (caar l)) (cons (cons elem (+ 1 (cdar l))) (cdr l))]
          [else (cons (car l) (insert elem (cdr l)))]))

  (define (helper lst)
    (cond [(null? lst) '()]
          [(= 3 (length (string->list (car lst)))) (insert (car lst) (helper (cdr lst)))] 
          [else (helper (cdr lst))]))

  (map car (my-sort (helper cipher-word-list))))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())

(define (two-lettered-words-starting-with)
  (define 2-lett-words (sort (cipher-common-words-double utils:cipher-word-list) (lambda (x y) (char>? (car (string->list x))
                                                                                                       (car (string->list y))))))

  (define (my-sort l)
    (sort l (lambda (x y) (> (cdr x) (cdr y)))))
  
  (define (op x y)
    (cond [(null? y) (list (cons (car (string->list x)) 1))]
          [(equal? (car (string->list x)) (caar y)) (cons (cons (caar y) (+ (cdar y) 1)) (cdr y))]
          [else (cons (cons (car (string->list x)) 1) y)]))
  
  (map car (my-sort (foldr op '() 2-lett-words))))

;; sorts a given list sample according to the order in which the elements occur in lst 
(define (sort-list-according-to sample lst)
  (define (union a b)
    (cond ((null? b) a)
          ((member (car b) a)
           (union a (cdr b)))
          (else (union (cons (car b) a) (cdr b)))))

  (union sample (remove* (remove* sample lst)
                         lst)))

(define (zip l1 l2)
  (match (list l1 l2)
    [(list '() lst) '()]
    [(list lst '()) '()]
    [(list (cons first1 rest1) (cons first2 rest2)) (cons (cons first1 first2) (zip rest1 rest2))]))



