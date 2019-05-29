#lang racket

(require 2htdp/batch-io)

(require "decision_functions_sig.rkt")
(require "list-comprehension.rkt")

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw (cdr (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw (map cddr (cdr (read-csv-file titanictrain))))

(provide mushroom-raw)
(define mushroom-raw (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (toResult x) (cond [(equal? #t x) 1]
                           [(equal? #f x) 0]
                           [else (string->number x)]))
(define (format data) (map (lambda (x) (cons (map string->number (cdr x)) (toResult (car x)))) data))

;list of (features . result)
(provide toy)
(define toy (format toy-raw))

(provide titanic)
(define titanic (format titanic-raw))

(provide mushroom)
(define mushroom (format mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (define (op x y)
    (cond [(= (cdr x) 1) (cons (+ 1 (car y)) (+ 1 (cdr y)))]
          [else (cons (car y) (+ 1 (cdr y)))]))
  (let ([ans (foldl op (cons 0 0) data)])
    (/ (car ans) (cdr ans))))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (define (log2 x) (if (= x 0) 0 (/ (log x) (log 2))))
  (let* ([prob1 (get-leaf-prob data)]
         [prob0 (- 1 prob1)])
    (- (+ (* prob1 (log2 prob1)) (* prob0 (log2 prob0))))))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
  (define init-entropy (get-entropy data))
  (define data-length (length data))
  
  ;; element of the form (data . value)
  ;;lst of the form ((data-list) . value)
  (define (insert elem lst)
    (cond [(null? lst) (list (cons (list (car elem)) (cdr elem)))]
          [(= (cdr elem) (cdar lst)) (cons (cons (cons (car elem) (caar lst)) (cdr elem)) (cdr lst))]
          [else (cons (car lst) (insert elem (cdr lst)))]))

  (define (data-lists-maker l)
    (cond [(null? l) '()]
          [else (insert (cons (car l) (f (caar l))) (data-lists-maker (cdr l)))]))
  
  (define list-of-datas (map car (data-lists-maker data)))
  (define remaining-entropy (foldl (lambda (x y) (+ y (/ (* (get-entropy x) (length x)) data-length))) 0 list-of-datas))
  
  (- init-entropy remaining-entropy))

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (define ans (map (lambda (x) (cons (entropy-diff (cdr x) data) x)) candidates))
  (cdar (sort ans (lambda (x y) (> (car x) (car y))))))

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (if (null? data) (DTree "0" 0 '())
  (let ([prob1 (get-leaf-prob data)])
  (cond [(or (= prob1 0) (= depth 0) (null? candidates)) (DTree (~a prob1) 0 '())]
        [else 
         (define best-feature (choose-f candidates data))
         (define list-of-datas (to-list-of-datas (cdr best-feature) data))
         (DTree (car best-feature) (cdr best-feature) (lc (build-tree (remove best-feature candidates) x (- depth 1))
                                                          : x <- list-of-datas))]))))

(define (to-list-of-datas f data)
  ;; element of the form (data . value)
  ;;lst of the form ((data-list) . value)
  (define (insert elem lst)
    (cond [(null? lst) (append (list (cons (list (car elem)) (cdr elem))) lst)]
          [(= (cdr elem) (cdar lst)) (cons (cons (cons (car elem) (caar lst)) (cdr elem)) (cdr lst))]
          [else (cons (car lst) (insert elem (cdr lst)))]))
         
  (define (data-lists-maker l)
    (cond [(null? l) '()]
          [else (insert (cons (car l) (f (caar l))) (data-lists-maker (cdr l)))]))
         
  (define my-lst (sort (data-lists-maker data) (lambda (x y) (< (cdr x) (cdr y)))))
  ;;ensures the values start from zero
  (define (helper lst val)
    (cond [(null? lst) '()]
          [(= (cdar lst) val) (cons (car lst) (helper (cdr lst) (+ val 1)))] 
          [else (cons (cons '() val) (helper lst (+ val 1)))]))
  (map car (helper my-lst 0)))


  ;given a test data (features only), make a decision according to a decision tree
  ;returns probability of the test data being classified as 1
  (provide make-decision)
  (define (make-decision tree test)
    (if (equal? 0 (DTree-func tree)) (string->number (DTree-desc tree))
        (let* ([desc (DTree-desc tree)]
               [kids (DTree-kids tree)]
               [func (DTree-func tree)]
               [index (func test)])
          (cond [(>= index (length kids)) 0]
                [else (make-decision (list-ref kids index) test)]))))

  ;============================================================================================================
  ;============================================================================================================
  ;============================================================================================================

  ;annotate list with indices
  (define (pair-idx lst n)
    (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
    )

  ;generate tree edges (parent to child) and recurse to generate sub trees
  (define (dot-child children prefix tabs)
    (apply string-append
           (map (lambda (t)
                  (string-append tabs
                                 "r" prefix
                                 "--"
                                 "r" prefix "t" (~a (cdr t))
                                 "[label=\"" (~a (cdr t)) "\"];" "\n"
                                 (dot-helper (car t)
                                             (string-append prefix "t" (~a (cdr t)))
                                             (string-append tabs "\t")
                                             )
                                 )
                  ) children
                    )
           )
    )

  ;generate tree nodes and call function to generate edges
  (define (dot-helper tree prefix tabs)
    (let* ([node (match tree [(DTree d f c) (cons d c)])]
           [d (car node)]
           [c (cdr node)])
      (string-append tabs
                     "r"
                     prefix
                     "[label=\"" d "\"];" "\n\n"
                     (dot-child (pair-idx c 0) prefix tabs)
                     )
      )
    )

  ;output tree (dot file)
  (provide display-tree)
  (define (display-tree tree outfile)
    (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                       (dot-helper tree "" "\t")
                                       "}"
                                       )
                )
    )
  ;============================================================================================================
  ;============================================================================================================
  ;============================================================================================================
