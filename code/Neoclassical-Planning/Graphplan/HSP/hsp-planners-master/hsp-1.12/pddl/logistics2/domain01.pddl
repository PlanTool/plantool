;;; these domains engineered from the att satplan encodings 
;;; which were derived from the graphplan domains
;;; which I think were derived from Veloso's prodigy domains
;;; - DSW 1/97

(define (domain logistics-strips)
  (:requirements :strips :equality)
  (:predicates (obj ?o)
	       (truck ?t)
	       (at ?o ?l)
	       (in ?o ?t)
	       (airplane ?p)
	       (airport ?s)
	       (in-city ?s ?city)
	       (city ?c)
	       (location ?l))
  ;; Need separate loading operations for trucks and planes, but
  ;; single unloading operation will do.
  (:action load-truck
	     :parameters (?o ?truck ?loc)
	     :precondition (and (obj ?o)
				 (truck ?truck)
				 (at ?o ?loc)
				 (at ?truck ?loc))
	     :effect (and (not (at ?o ?loc))
			   (in ?o ?truck)))
  (:action load-plane
	     :parameters (?o ?p ?loc)
	     :precondition (and (obj ?o)
				 (airplane ?p)
				 (at ?o ?loc)
				 (at ?p ?loc))
	     :effect (and (not (at ?o ?loc))
			   (in ?o ?p)))
  (:action unload
	     :parameters (?o ?v ?loc)
	     :precondition (and (in ?o ?v)
				 (at ?v ?loc))
	     :effect (and (at ?o ?loc)
			   (not (in ?o ?v))))
  (:action fly
	     :parameters (?p ?s ?d)
	     :precondition (and (airplane ?p)
				 (airport ?s)
				 (airport ?d)
				 (at ?p ?s)
				 (not (= ?s ?d)))
	     :effect (and (at ?p ?d)
			   (not (at ?p ?s))))
  (:action drive
	     :parameters (?truck ?s ?d ?city)
	     :precondition (and (truck ?truck)
				 (at ?truck ?s)
				 (in-city ?s ?city)
				 (in-city ?d ?city)
				 (not (= ?s ?d)))
	     :effect (and (at ?truck ?d)
			   (not (at ?truck ?s)))))








