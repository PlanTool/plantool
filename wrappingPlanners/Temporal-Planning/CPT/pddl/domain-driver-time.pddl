(define (domain driverlog)
  (:requirements :strips :equality) 
  (:predicates 	(OBJ ?obj)
	       	(TRUCK ?truck)
               	(LOCATION ?loc)
		(driver ?d)
		(at ?obj ?loc)
		(in ?obj1 ?obj)
		(driving ?d ?v)
		(link ?x ?y) (path ?x ?y)
		(empty ?v)
)


(:action LOAD-TRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :duration 2 
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
   (at ?truck ?loc) (at ?obj ?loc))
  :effect
   (and (not (at ?obj ?loc)) (in ?obj ?truck)))

(:action UNLOAD-TRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :duration 2
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
        (at ?truck ?loc) (in ?obj ?truck))
  :effect
   (and (not (in ?obj ?truck)) (at ?obj ?loc)))

(:action BOARD-TRUCK
  :parameters
   (?driver
    ?truck
    ?loc)
  :duration 1
  :precondition
   (and (DRIVER ?driver) (TRUCK ?truck) (LOCATION ?loc)
   (at ?truck ?loc) (at ?driver ?loc) (empty ?truck))
  :effect
   (and (not (at ?driver ?loc)) (driving ?driver ?truck) (not (empty ?truck))))

(:action DISEMBARK-TRUCK
  :parameters
   (?driver
    ?truck
    ?loc)
  :duration 1
  :precondition
   (and (DRIVER ?driver) (TRUCK ?truck) (LOCATION ?loc)
        (at ?truck ?loc) (driving ?driver ?truck))
  :effect
   (and (not (driving ?driver ?truck)) (at ?driver ?loc) (empty ?truck)))

(:action DRIVE-TRUCK
  :parameters
   (?truck
    ?loc-from
    ?loc-to
    ?driver)
  :duration 10
  :precondition
   (and (TRUCK ?truck) (LOCATION ?loc-from) (LOCATION ?loc-to) (DRIVER ?driver) 
   (at ?truck ?loc-from)
   (driving ?driver ?truck) (link ?loc-from ?loc-to))
  :effect
   (and (not (at ?truck ?loc-from)) (at ?truck ?loc-to)))

(:action WALK
  :parameters
   (?driver
    ?loc-from
    ?loc-to)
  :duration 20
  :precondition
   (and (DRIVER ?driver) (LOCATION ?loc-from) (LOCATION ?loc-to)
	(at ?driver ?loc-from) (path ?loc-from ?loc-to))
  :effect
   (and (not (at ?driver ?loc-from)) (at ?driver ?loc-to)))

 
)
