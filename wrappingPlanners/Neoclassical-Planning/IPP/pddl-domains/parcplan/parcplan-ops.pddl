;; blocks world with limited space on table and arbitrary number of robot arms
 


(define (domain parcplan)
(:requirements :strips )
(:predicates (clear ?ob)
             (on ?x ?y)
             (on_table ?ob ?pos)
             (arm_empty ?r)
             (holding ?r ?ob)
             (robot ?r) 
             (position ?pos) 
             (block ?ob))


(:action pickup
  :parameters (?r ?ob ?pos)
  :precondition (and (robot ?r) (position ?pos) (block ?ob)
                     (clear ?ob) (on_table ?ob ?pos) (arm_empty ?r))
  :effect (and (holding ?r ?ob) (clear ?pos)
          (not (clear ?ob)) (not (on_table ?ob ?pos)) (not (arm_empty ?r))))


(:action putdown
  :parameters (?r ?ob ?pos)
  :precondition (and (robot ?r) (position ?pos) (block ?ob)
                     (holding ?r ?ob) (clear ?pos))
  :effect (and (clear ?ob) (arm_empty ?r) (on_table ?ob ?pos) 
          (not (holding ?r ?ob)) (not (clear ?pos))))


(:action stack
  :parameters (?r ?ob ?underob)
  :precondition  (and (robot ?r) (block ?ob) (block ?underob)
                      (clear ?underob) (holding ?r ?ob))
  :effect (and (arm_empty ?r) (clear ?ob) (on ?ob ?underob)
          (not (clear ?underob)) (not (holding ?r ?ob))))

(:action unstack
  :parameters (?r ?ob ?underob)
  :precondition (and (robot ?r) (block ?ob) (block ?underob)
                     (on ?ob ?underob) (clear ?ob) (arm_empty ?r))
  :effect (and (holding ?r ?ob) (clear ?underob)
          (not (on ?ob ?underob)) (not (clear ?ob)) (not (arm_empty ?r)))))
