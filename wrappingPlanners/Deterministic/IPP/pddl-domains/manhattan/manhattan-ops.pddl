(define (domain manhattan)
(:requirements :strips)
(:constants robot)
(:predicates (conn ?x ?y)
             (key_shape ?k ?s)
             (lock_shape ?x ?s)
             (at ?r ?x )
             (place ?p)
             (key ?k)
             (shape ?s)
             (locked ?x)
             (holding ?k)
             (open ?x)
             (arm_empty ))



(:action unlock
:parameters (?curpos ?lockpos ?key ?shape)
:precondition (and (place ?curpos) (place ?lockpos) (key ?key) (shape ?shape)
          (conn ?curpos ?lockpos) (key_shape ?key ?shape)
                   (lock_shape ?lockpos ?shape) (at robot ?curpos) 
                   (locked ?lockpos) (holding ?key))
:effect (and  (open ?lockpos) (not (locked ?lockpos))))


(:action move
:parameters (?curpos ?nextpos)
:precondition (and (place ?curpos) (place ?nextpos)
               (at robot ?curpos) (conn ?curpos ?nextpos) (open ?nextpos))
:effect (and (at robot ?nextpos) (not (at robot ?curpos))))

(:action pickup
:parameters (?curpos ?key)
:precondition (and (place ?curpos) (key ?key) 
                  (at robot ?curpos) (at ?key ?curpos) (arm_empty ))
:effect (and (holding ?key)
   (not (at ?key ?curpos)) (not (arm_empty ))))


(:action pickup-and-loose
:parameters (?curpos ?newkey ?oldkey)
:precondition (and (place ?curpos) (key ?newkey) (key ?oldkey)
                  (at robot ?curpos) (holding ?oldkey) (at ?newkey ?curpos))
:effect (and (holding ?newkey) (at ?oldkey ?curpos)
        (not (holding ?oldkey)) (not (at ?newkey ?curpos))))

(:action putdown
:parameters (?curpos ?key)
:precondition (and (place ?curpos) (key ?key) 
                  (at robot ?curpos) (holding ?key))
:effect (and (arm_empty ) (at ?key ?curpos) (not (holding ?key)))))


	
