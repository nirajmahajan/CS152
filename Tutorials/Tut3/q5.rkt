#lang racket
(struct dir (info fdlist) #:transparent)
(struct file (info contents) #:transparent)

(define (name node)
  (cond [(dir? node) (car (dir-info node))]
        [else (car (file-info node))]))

(define thistree
  (dir
   (cons "as" 1024)
   (list (dir
          (cons "cs613" 1024)
          (list (file (cons "quiz" 4246) "junk")
                (file (cons "tut" 7226) "junk")
                (dir (cons "2004" 1024)
                     (list (file (cons "quiz" 576) "junk")
                           (file (cons "tut" 345) "junk")))))
         (dir (cons "cs152" 1024)
              (list (file (cons "quiz1" 4532) "junk")
                    (file (cons "quiz2" 1234) "junk")
                    (file (cons "tut" 1223) "junk")))
         (dir (cons "cs154" 1024)
              (list (file (cons "tut1" 459) "junk")
                    (file (cons "tut2" 782) "junk"))))))

(define (findtree tr path1)
  (define path (cond [(string=? "as" (car path1)) (cdr path1)]
                     [else path1]))
  
  (define (find str l)
    (cond [(equal? str (name (car l))) (car l)]
          [else (find str (cdr l))]))
  
  (cond [(= (length path) 1) (find (car path) (dir-fdlist tr))]
        [else (findtree (find (car path) (dir-fdlist tr)) (cdr path))]))

(define (ls tr path)
  (define folder (findtree tr path))
  (define lst (dir-fdlist folder))
  (map name lst))

(define (size dtr path)
  (define dr (findtree path))
  (define (lst-size l)
    (cond [(null? l) 0]
          [(dir? (car l)) (+ (cdr (dir-info (car l))) (lst-size (cdr l)))]
          [else (+ (cdr (file-info (car l))) (lst-size (cdr l)))]))
  
  (match dr
    [(dir inf1 fdlst) (+ (cdr inf1) (lst-size fdlst))]
    [(file inf1 cont) (cdr inf1)]))

(define (delete dir-tree path)
  (define folder (findtree dir-tree path)) 
  
  (match folder
    [(dir inf1 fdlists) (dir inf1 '())]))


    
  