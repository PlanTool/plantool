(define (domain rover)
(:requirements :strips :equality)
(:predicates
	 (at ?x ?y) (at_lander ?x ?y) (can_traverse ?r ?x ?y) (equipped_for_soil_analysis ?r) (equipped_for_rock_analysis ?r) (equipped_for_imaging ?r) (empty ?s) (have_rock_analysis ?r ?w) (have_soil_analysis ?r ?w) (full ?s) (calibrated ?c ?r) (supports ?c ?m) (available ?r) (visible ?w ?p) (have_image ?r ?o ?m) (communicated_soil_data ?w) (communicated_rock_data ?w) (communicated_image_data ?o ?m) (at_soil_sample ?w) (at_rock_sample ?w) (visible_from ?o ?w) (store_of ?s ?r) (calibration_target ?i ?o) (on_board ?i ?r) (channel_free ?l)(rover ?x) (waypoint ?x) (store ?x) (camera ?x) (mode ?x) (lander ?x) (objective ?x) )
(:action navigate
 :parameters ( ?x ?y ?z)
:duration 5
 :precondition
	(and  (rover ?x)  (waypoint ?y)  (waypoint ?z)   (can_traverse ?x ?y ?z)  (available ?x)  (at ?x ?y)  (visible ?y ?z))
 :effect
	(and   (at ?x ?z)   (not (at ?x ?y))))

(:action sample_soil
 :parameters ( ?x ?s ?p)
 :duration 10
 :precondition
	(and  (rover ?x)  (store ?s)  (waypoint ?p)   (at ?x ?p)  (at ?x ?p)  (at_soil_sample ?p)  (equipped_for_soil_analysis ?x)  (store_of ?s ?x)  (empty ?s))
 :effect
	(and   (not (at_soil_sample ?p))   (have_soil_analysis ?x ?p)   (full ?s)   (not (empty ?s))))

(:action sample_rock
 :parameters ( ?x ?s ?p)
:duration 8
 :precondition
	(and  (rover ?x)  (store ?s)  (waypoint ?p)   (at ?x ?p)  (at ?x ?p)  (at_rock_sample ?p)  (equipped_for_rock_analysis ?x)  (store_of ?s ?x)  (empty ?s))
 :effect
	(and   (not (at_rock_sample ?p))   (have_rock_analysis ?x ?p)   (full ?s)   (not (empty ?s))))

(:action drop
 :parameters ( ?x ?y)
:duration 1
 :precondition
	(and  (rover ?x)  (store ?y)   (store_of ?y ?x)  (full ?y))
 :effect
	(and   (empty ?y)   (not (full ?y))))

(:action calibrate
 :parameters ( ?r ?i ?t ?w)
 :duration 5
 :precondition
	(and  (rover ?r)  (camera ?i)  (objective ?t)  (waypoint ?w)   (equipped_for_imaging ?r)  (calibration_target ?i ?t)  (at ?r ?w)  (visible_from ?t ?w)  (on_board ?i ?r))
 :effect
	   (and (calibrated ?i ?r)))

(:action take_image
 :parameters ( ?r ?p ?o ?i ?m)
 :duration 7
 :precondition
	(and  (rover ?r)  (waypoint ?p)  (objective ?o)  (camera ?i)  (mode ?m)   (calibrated ?i ?r)  (on_board ?i ?r)  (equipped_for_imaging ?r)  (supports ?i ?m)  (visible_from ?o ?p)  (at ?r ?p))
 :effect
	(and   (not (calibrated ?i ?r))   (have_image ?r ?o ?m)))

(:action communicate_soil_data
 :parameters ( ?r ?l ?p ?x ?y)
:duration 10
 :precondition
	(and  (rover ?r)  (lander ?l)  (waypoint ?p)  (waypoint ?x)  (waypoint ?y)   (at ?r ?x)  (at_lander ?l ?y)  (have_soil_analysis ?r ?p)  (visible ?x ?y)  (available ?r)  (channel_free ?l))
 :effect
	(and   (available ?r)   (communicated_soil_data ?p)   (channel_free ?l)   (not (channel_free ?l))   (not (available ?r))))

(:action communicate_rock_data
 :parameters ( ?r ?l ?p ?x ?y)
 :duration 10
 :precondition
	(and  (rover ?r)  (lander ?l)  (waypoint ?p)  (waypoint ?x)  (waypoint ?y)   (at ?r ?x)  (at_lander ?l ?y)  (have_rock_analysis ?r ?p)  (visible ?x ?y)  (available ?r)  (channel_free ?l))
 :effect
	(and   (available ?r)   (communicated_rock_data ?p)   (channel_free ?l)   (not (channel_free ?l))   (not (available ?r))))

(:action communicate_image_data
 :parameters ( ?r ?l ?o ?m ?x ?y)
 :duration 15
 :precondition
	(and  (rover ?r)  (lander ?l)  (objective ?o)  (mode ?m)  (waypoint ?x)  (waypoint ?y)   (at ?r ?x)  (at_lander ?l ?y)  (have_image ?r ?o ?m)  (visible ?x ?y)  (available ?r)  (channel_free ?l))
 :effect
	(and   (available ?r)   (communicated_image_data ?o ?m)   (channel_free ?l)   (not (channel_free ?l))   (not (available ?r))))

)
