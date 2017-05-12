;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logistics  Domain definition
;; 
;; USB
;;

(define (domain logistics)
  (:requirements :strips)
  (:predicates 
	(truck ?t)           ;; types
	(location ?l)
	(airport ?a)
	(airplane ?p)
	(city ?c)
	(object ?o)

	(at ?x ?y)       ;; predicates
	(in ?x ?y)
	(loc ?x ?y))
                
 (:action LoadTruck1
	     :parameters (?o ?t ?l)
	     :precondition (and (object ?o)
				(truck ?t)
				(location ?l)						
				(at ?t ?l)
                                (at ?o ?l))
	     :effect (and (in ?o ?t) 
                          (not (at ?o ?l))))

  (:action LoadTruck2
	     :parameters (?o ?t ?a)
	     :precondition (and (object ?o)
				(truck ?t)
				(airport ?a)
				(at ?t ?a)
				(at ?o ?a))
	     :effect (and (in ?o ?t) 
                          (not (at ?o ?a))))

  (:action LoadAirplane
	     :parameters (?o ?p ?a)
	     :precondition (and (object ?o)
				(airplane ?p)
				(airport ?a)
				(at ?o ?a)
				(at ?p ?a))
	     :effect (and (in ?o ?p) 
                          (not (at ?o ?a))))

 (:action UnloadTruck1
	     :parameters (?o ?t ?l)
	     :precondition (and (object ?o)
				(truck ?t)
				(location ?l)
				(at ?t ?l)
				(in ?o ?t))
	     :effect (and (at ?o ?l) 
                          (not (in ?o ?t))))


 (:action UnloadTruck2
	     :parameters (?o ?t ?a)
	     :precondition (and (object ?o)
				(truck ?t)
				(airport ?a)
				(at ?t ?a)
				(in ?o ?t))
	     :effect (and (at ?o ?a) 
                          (not (in ?o ?t))))


 (:action UnloadAirplane
	     :parameters (?o ?p ?a)
	     :precondition (and (object ?o)
				(airplane ?p)
				(airport ?a)
				(in ?o ?p)
				(at ?p ?a))
	     :effect (and (at ?o ?a) 
                          (not (in ?o ?p))))


 (:action DriveTruck1
	     :parameters (?t ?l1 ?l2 ?c)
	     :precondition (and (truck ?t)
				(location ?l1)
				(location ?l2)
				(city ?c)
				(at ?t ?l1)
				(loc ?l1 ?c)
				(loc ?l2 ?c))
	     :effect (and (at ?t ?l2)
                          (not (at ?t ?l1))))


 (:action DriveTruck2
	     :parameters (?t ?l ?a ?c)
	     :precondition (and (truck ?t)
				(location ?l)
				(airport ?a)
				(city ?c)
				(at ?t ?l)
				(loc ?l ?c)
				(loc ?a ?c))
	     :effect (and (at ?t ?a)
                          (not (at ?t ?l))))

(:action DriveTruck3
	     :parameters (?t ?a ?l ?c)
	     :precondition (and (truck ?t)
				(airport ?a)
				(location ?l)
				(city ?c)
				(at ?t ?a)
				(loc ?a ?c)
				(loc ?l ?c))
	     :effect (and (at ?t ?l)
                          (not (at ?t ?a))))


(:action FlyAirplane
	     :parameters (?p ?a1 ?a2)
	     :precondition (and (airplane ?p)
				(airport ?a1)
				(airport ?a2)
				(at ?p ?a1))
	     :effect (and (at ?p ?a2)
                          (not (at ?p ?a1)))))
