; rules taken from TLPLAN
; written by: Yi-Cheng Huang

(define (control ctrl1)
        (:domain flat-tire-strips)

; keep correctly placed wheels on their current hubs
(:action remove-wheel
	:exclude (goal (on ?w ?h)))
	
; don't place a wheel on an incorrect hub
(:action put-on-wheel
	:exclude (not (goal (on ?w ?h))))

(:action put-away
	:exclude (not (goal (in ?x ?c))))

; don't inflate a whell unless it needs to be inflated.
(:action inflate
	:exclude (not (goal (inflated ?w))))


; don't jack up a hub unless it needs to be jacked up.
(:wffctrl c1
 :scope (forall (?h) (hub ?h) (forall (?w) (wheel ?w) (goal (on ?w ?h))))
 :precondition (and (on-ground ?h) (on ?w ?h))
 :effect (next (on-ground ?h))) 	

; keep a hub jacked up until its wheel is on
(:wffctrl c1
 :scope (forall (?h) (hub ?h) (forall (?w) (wheel ?w) (goal (on ?w ?h))))
 :precondition (and (not (on-ground ?h)) (not (on ?w ?h)))
 :effect (next (not (on-ground ?h))))	


; remove the wheel asap --- this helps the SAT solver alot!!!
(:wffctrl c2
 :scope (forall (?h) (hub ?h) 
		(forall (?w) (wheel ?w) (not (goal (on ?w ?h)))))
 :precondition (and (on ?w ?h) (unfastened ?h))
 :effect (next (not (on ?w ?h))))

; put on the wheel asap
(:wffctrl c3
 :scope (forall (?h) (hub ?h) 
		(forall (?w) (wheel ?w) (goal (on ?w ?h))))
 :precondition (and (free ?h) (have ?w))
 :effect (next (on ?w ?h)))

; don't close container when jack is still used
;(:wffctrl c4
; :scope (forall (?c) (container ?c) (goal (in jack ?c)))
; :precondition (have jack)
; :effect (next (open ?c)))
)
