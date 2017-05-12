
(define (domain bmtuc)
  (:requirements :typing)
  (:types p toilet)
    
  (:predicates
    (pos ?x - p)
    (defused)
    (nclogged ?t - toilet)
  )

  (:action dunk
   :parameters  (?x - p ?t - toilet)
   :precondition (nclogged ?t)
   :effect
    (and
       (oneof (not (nclogged ?t)) (nclogged ?t))
       (when (pos ?x) (defused))
    )
  )

  (:action flush
   :parameters  (?t - toilet)
   :effect 
   (nclogged ?t))
  )

