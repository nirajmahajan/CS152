#lang racket

(struct bnode (ltree rtree) #:transparent)
(struct leaf (val) #:transparent)

(define example-huffman
  (bnode
   (bnode (leaf 'B) (bnode (leaf 'E) (leaf 'D)))
   (bnode (leaf 'A) (bnode (leaf 'C) (bnode
                                      (bnode (leaf 'H) (leaf 'G))
                                      (leaf 'F))))))

(define (encoder tr)
  (define (encode tr curr)
    (match tr
      [(leaf val) (list (cons val (reverse curr)))]
      [(bnode lt rt) (append (encode lt (cons 0 curr))
                            (encode rt (cons 1 curr)))]))

  (encode tr '()))