#lang racket
(struct bnode (lt rt) #:transparent)
(struct leaf (val) #:transparent)
(struct gnode (val ls) #:transparent)

(define (b->g btr)
  (match btr
    [(bnode (leaf vl) (leaf vr)) (gnode vl (list (gnode vr '())))]
    [(bnode (leaf vl) rt) (gnode vl (list (b->g rt)))]
    [(bnode lt (leaf vr)) (gnode (gnode-val (b->g lt)) (append (gnode-ls (b->g lt)) (list (gnode vr '()))))]
    [(bnode lt rt) (gnode (gnode-val (b->g lt)) (append (gnode-ls (b->g lt)) (list (b->g rt))))]))

(define (last-del l) (reverse (cdr (reverse l))))

(define (g->b gtr)
  (match gtr
    [(gnode val '()) (leaf val)]
    [(gnode val lst) (bnode (g->b (gnode val (last-del lst))) (g->b (last lst)))]))


(define gtr (gnode 'f (list (gnode 'g (list (gnode 'x '())))
                            (gnode 'h (list (gnode 'y '())
                                            (gnode 'z '())))
                            (gnode 'i '()))))

(define btr (bnode (bnode (bnode (leaf 'f)
                                 (bnode (leaf 'g)
                                        (leaf 'x)))
                          (bnode (bnode (leaf 'h)
                                        (leaf 'y))
                                 (leaf 'z)))
                   (leaf 'i)))
                                   