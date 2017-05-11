(define (domain tile)
  (:requirements :strips :equality)
  (:constants blank)
  (:predicates (at ?t ?x ?y)
	       (dec ?p ?pp)
               (tile ?t)
               (position ?p))

  (:action move-up
    :parameters (?t ?x ?py ?ny)
    :precondition (and (tile ?t) (position ?x) (position ?py) (position ?ny) 
		       (dec ?ny ?py) (at blank ?x ?ny) (at ?t ?x ?py))
    :effect (and (not (at blank ?x ?ny)) (not (at ?t ?x ?py))
		 (at blank ?x ?py) (at ?t ?x ?ny)))

  (:action move-down
    :parameters (?t ?x ?py ?ny)
    :precondition (and (tile ?t) (position ?x) (position ?py) (position ?ny) 
		       (dec ?py ?ny) (at blank ?x ?ny) (at ?t ?x ?py))
    :effect (and (not (at blank ?x ?ny)) (not (at ?t ?x ?py))
		 (at blank ?x ?py) (at ?t ?x ?ny)))

  (:action move-left
    :parameters (?t ?y ?px ?nx)
    :precondition (and (tile ?t) (position ?y) (position ?px) (position ?nx) 
		       (dec ?px ?nx) (at blank ?nx ?y) (at ?t ?px ?y))
    :effect (and (not (at blank ?nx ?y)) (not (at ?t ?px ?y))
		 (at blank ?px ?y) (at ?t ?nx ?y)))

  (:action move-right
    :parameters (?t ?y ?px ?nx)
    :precondition (and (tile ?t) (position ?y) (position ?px) (position ?nx) 
		       (dec ?nx ?px) (at blank ?nx ?y) (at ?t ?px ?y))
    :effect (and (not (at blank ?nx ?y)) (not (at ?t ?px ?y))
		 (at blank ?px ?y) (at ?t ?nx ?y)))
  )
