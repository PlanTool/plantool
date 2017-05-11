(define (control ctrl1)
	(:domain prodigy-bw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Wff control
;

; don't pick-up and put-down a block on table again
(:wffctrl w1
 :scope (forall (?block) (true) (true))
 :precondition (and (on-table ?block) (next (holding ?block)))
 :effect (not (next (next (on-table ?block)))))

; don't unstack and stack a block to its original place again
(:wffctrl w2
 :scope (forall (?block1) (true)
	   (forall (?block2) (true) (true)))
 :precondition (and (on ?block1 ?block2) (next (holding ?block1)))
 :effect (not (next (next (on ?block1 ?block2)))))

)


