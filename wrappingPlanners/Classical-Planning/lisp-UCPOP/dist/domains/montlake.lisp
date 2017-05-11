;; The Montlake bridge is broken.  They keep taking the pin out manually.
;; I have written a domain in its honor.

(in-package "UCPOP")

(define (domain montlake)
    (:operator switch-semaphore
     :parameters nil
     :precondition nil
     :effect (and (when (and (semaphore red)
			     (all-gates-up))
		    (and (not (semaphore red))
			 (semaphore green)))
		  (when (semaphore green)
		    (and (not (semaphore green))
			 (semaphore yellow)))
		  (when (semaphore yellow)
		    (and (not (semaphore yellow))
			 (semaphore red))))
     )
    (:operator shut-first-gates
     :parameters nil
     :precondition (semaphore red)
     :effect (not (first-gates-up))
     )
    (:operator shut-second-gates
     :parameters nil
     :precondition (not (first-gates-up))
     :effect (not (second-gates-up))
     )
    (:operator open-first-gates
     :parameters nil
     :precondition (second-gates-up)
     :effect (first-gates-up)
     )
    (:operator open-second-gates
     :parameters nil
     :precondition (and (pin-in)
			(joe-at tower))
     :effect (second-gates-up)
     )
    (:operator pull-pin
     :parameters nil
     :precondition (and (all-gates-down)
			(joe-at pin))
     :effect (not (pin-in))
     )
    (:operator push-pin
     :parameters nil
     :precondition (and (all-gates-down)
			(joe-at pin))
     :effect (pin-in)
     )
    (:operator joe-moves
     :parameters (?here ?there)
     :precondition (joe-at ?here)
     :effect (and (not (joe-at ?here))
		  (joe-at ?there))
     )
    (:operator open-bridge
     :parameters nil
     :precondition (and (all-gates-down)
			(not (pin-in))
			(or (joe-at tower)
			    (joe-at landing)))
     :effect (and (bridge-up)
		  (not (boats-waiting)))
     )
    (:operator close-bridge
     :parameters nil
     :precondition nil
     :effect (not (bridge-up))
     )
    (:axiom gates-up
	    :context (and (first-gates-up)
			  (second-gates-up))
	    :implies (all-gates-up))
    (:axiom gates-down
	    :context (and (not (first-gates-up))
			  (not (second-gates-up)))
	    :implies (all-gates-down))

    )

(define (problem bridge)
    :domain 'montlake
    :inits ((joe-at tower)
	    (first-gates-up)
	    (second-gates-up)
	    (pin-in)
	    (boats-waiting)
	    (semaphore green))
    :goal (and (not (boats-waiting))
	       (semaphore green)))