(define (problem top1)
(:domain test)
(:objects t1 t2 t3 - type1 b1 b2 - block)
(:init 
(p t1) (q t2) (a t3))

(:goal (forall (?x - type1) (not (exists (?z - type1) (p t1)))))
)

