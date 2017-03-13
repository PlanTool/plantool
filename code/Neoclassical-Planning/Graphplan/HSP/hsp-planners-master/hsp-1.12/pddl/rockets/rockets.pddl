;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rocket Domain definition
;; 
;; USB
;;

(define (domain rockets)
  (:requirements :strips )
  (:predicates  (rocket ?r)	;; types
		(place ?p)
		(object ?o)
		(at ?x ?y)	;; predicates
		(in ?o ?r)
		(has-fuel ?r)
		(no-fuel ?r))
               
  (:action Load
	     :parameters (?o ?r ?p)
	     :precondition (and (object ?o)
				(rocket ?r)
				(place ?p)
				(at ?r ?p)
				(at ?o ?p))
	     :effect (and (in ?o ?r)
                          (not (at ?o ?p))))

 
 (:action Unload
	     :parameters (?o ?r ?p)
	     :precondition (and (object ?o)
				(rocket ?r)
				(place ?p)
				(at ?r ?p)
				(in ?o ?r))
	     :effect (and (at ?o ?p)
                          (not (in ?o ?r))))


 (:action Move
	     :parameters (?r ?p1 ?p2)
	     :precondition (and (rocket ?r)
				(place ?p1)
				(place ?p2)
				(has-fuel ?r)
				(at ?r ?p1))
	     :effect (and (at ?r ?p2) 
			  (no-fuel ?r)
                          (not (has-fuel ?r)) 
			  (not (at ?r ?p1))))

  (:action Refuel
	     :parameters (?r)
	     :precondition (and (rocket ?r)
				(no-fuel ?r))
	     :effect (and (has-fuel ?r)
                          (not (no-fuel ?r)))))

