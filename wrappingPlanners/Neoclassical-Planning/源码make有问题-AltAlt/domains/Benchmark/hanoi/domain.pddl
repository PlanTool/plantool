(define (domain hanoi)
 ; (:requirements :strips :equality)

  (:predicates
     (top ?x)
     (smaller ?x ?y)
     (on ?x ?y))

  (:action move
	:parameters (?x ?y ?z)
	:precondition (and (on ?x ?y)
			    (top ?x)
			    (top ?z)
			    (smaller ?x ?z)
			    (smaller ?x ?y)
			    (not (= ?z ?y)))
;			    (neq ?z ?y))
	:effect (and (on ?x ?z)
		     (top ?y)
		     (not (on ?x ?y))
		     (not (top ?z)))))