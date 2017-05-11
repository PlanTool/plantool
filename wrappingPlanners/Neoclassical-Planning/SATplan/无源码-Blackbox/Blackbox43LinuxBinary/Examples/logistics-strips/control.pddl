; control rules taken from TLPLAN
; written by Yi-Cheng Huang

(define (control logcontrol)
	(:domain logistics)

(:defpredicate in_wrong_city 
 :parameters (?obj ?loc)
 :body (exists (?goal_loc) (goal (at ?obj ?goal_loc))
         (exists (?city) (in-city ?loc ?city)
           (not (in-city ?goal_loc ?city)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graph Prunning control rules
;

; don't load an object into to a truck if it is at the goal location
(:action LOAD-TRUCK
  :exclude (goal (at ?obj ?loc)))

; don't load an object into a truck if it is not in the goal city 
; and it is at an airport
(:action LOAD-TRUCK
  :exclude (and (in_wrong_city ?obj ?loc) (AIRPORT ?loc)))

; don't unload an object from a truck if it is in the wrong city and 
; it is not at an airport
(:action UNLOAD-TRUCK
 :exclude (and (in_wrong_city ?obj ?loc) (not (AIRPORT ?loc))))

; don't unload an object from a truck if it is in the goal city 
; but it is not at the goal location
(:action UNLOAD-TRUCK
 :exclude (and (not (in_wrong_city ?obj ?loc)) (not (goal (at ?obj ?loc)))))

; don't load an object into an airplane if it is in the goal city
(:action LOAD-AIRPLANE
 :exclude (not (in_wrong_city ?obj ?loc)))

; don't unload an object from an airplane if it is not in the goal city
(:action UNLOAD-AIRPLANE
 :exclude (in_wrong_city ?obj ?loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Wff control rules
;

; don't move a truck if there is an object that needs to be moved 
(:wffctrl w1
 :scope (forall (?trk) (truck ?trk)
	  (forall (?obj) (obj ?obj)
            (forall (?loc) (location ?loc)
              (and (in_wrong_city ?obj ?loc) (not (AIRPORT ?loc))))))
 :precondition (and (at ?trk ?loc) (at ?obj ?loc))
 :effect (next (at ?trk ?loc)))

; don't move a truck if there is an object that needs to be unloaded
(:wffctrl w2
 :scope (forall (?trk) (truck ?trk)
	  (forall (?obj) (obj ?obj)
            (forall (?loc) (AIRPORT ?loc)
              (in_wrong_city ?obj ?loc))))
 :precondition (and (at ?trk ?loc) (in ?obj ?trk))
 :effect (next (at ?trk ?loc)))

; don't move a truck if there is an object in the truck and at the goal
(:wffctrl w3
 :scope (forall (?trk) (truck ?trk)
	  (forall (?obj) (obj ?obj)
            (forall (?loc) (location ?loc)
              (goal (?obj ?loc)))))
 :precondition (and (at ?trk ?loc) (in ?obj ?trk))
 :effect (next (at ?trk ?loc)))

; don't move an airplane if there is an object that needs to be moved 
;(:wffctrl w4
; :scope (forall (?pln) (AIRPLANE ?pln)
;	  (forall (?obj) (obj ?obj)
;            (forall (?loc) (AIRPORT ?loc)
;              (in_wrong_city ?obj ?loc))))
; :precondition (and (at ?obj ?loc) (at ?pln ?loc))
; :effect (next (at ?pln ?loc)))

; don't move an airplane if there is an object that needs to be unloaded 
(:wffctrl w5
 :scope (forall (?pln) (AIRPLANE ?pln)
	  (forall (?obj) (obj ?obj)
            (forall (?loc) (AIRPORT ?loc)
              (not (in_wrong_city ?obj ?loc)))))
 :precondition (and (at ?pln ?loc) (in ?obj ?pln))
 :effect (next (at ?pln ?loc)))


)


