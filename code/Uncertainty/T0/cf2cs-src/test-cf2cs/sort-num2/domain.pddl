
(define (domain s) 
    (:requirements :typing :equality)
    (:types num)
    (:constants  n1 n2 - num)
    (:predicates (foo) (less ?n1 ?n2 - num))

        
    (:action cmpswap-1-2
     :effect (and (less n1 n2) (not (less n2 n1)) 
    ))

)

