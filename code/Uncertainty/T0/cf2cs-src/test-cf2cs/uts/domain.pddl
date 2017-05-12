(define (domain uts)
   (:requirements :typing :equality)
   (:types node)
   (:predicates (visited ?x - node) (edge ?x ?y - node) (at ?x - node) (started))
   (:action start
     :parameters (?x - node)
     :effect (when (at ?x) (and (started) (visited ?x)))
   )
   (:action travel
     :parameters (?x ?y - node)
     :precondition (started)
     :effect (when (and (at ?x) (edge ?x ?y)) (and (visited ?y) (at ?y) (not (at ?x))))
   )
)

