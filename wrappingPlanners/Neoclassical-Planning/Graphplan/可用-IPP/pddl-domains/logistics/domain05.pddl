(define (DOMAIN LOGISTICS-ADL)
  (:requirements :adl :domain-axioms)
  (:types physobj - object
	  obj vehicle - physobj
	  truck airplane - vehicle
	  location city - object
	  airport - location)
  (:predicates (at ?x - physobj ?l - location)
	       (in ?x - obj ?t - vehicle)
	       (loc-at ?l - location ?c - city))

;;; Unnecessary, because of the type inheritance (trucks and airplanes 
;;; are vehicles).
;;;  (:axiom is-vehicle
;;;	  :context (:or (truck ?vehicle)
;;;			(airplane ?vehicle))
;;;	  :implies (vehicle ?vehicle))
  
(:action load 
	     :parameters (?obj - obj ?airplane - vehicle ?loc - location) 
	     :precondition (and (at ?obj ?loc) (at ?airplane ?loc)) 
	     :effect (and (in ?obj ?airplane)))
  
(:action unload 
	     :parameters (?obj - obj ?airplane - vehicle ?loc - location) 
	     :precondition (and (in ?obj ?airplane) (at ?airplane ?loc)) 
	     :effect (and (not (in ?obj ?airplane))))
  
(:action go 
	   :parameters (?vehicle - vehicle ?loc-from ?loc-to - location
			?city - city)
	     :precondition (and (at ?vehicle ?loc-from) 
				 (loc-at ?loc-from ?city) 
				 (loc-at ?loc-to ?city))
	     :effect  (and (at ?vehicle ?loc-to) 
			    (not (at ?vehicle ?loc-from))		  
			    (forall (?x - obj)
				     (when (and (in ?x ?vehicle))
					    (and (not (at ?x ?loc-from)) 
						  (at ?x ?loc-to))))))
(:action fly
 :parameters (?vehicle - airplane ?loc-from ?loc-to - location)
 :precondition (and (at ?vehicle ?loc-from) )
 :effect  (and (at ?vehicle ?loc-to) 
                    (not (at ?vehicle ?loc-from))		  
			    (forall (?x - obj)
				     (when (and (in ?x ?vehicle))
					    (and (not (at ?x ?loc-from)) 
						  (at ?x ?loc-to)))))))


