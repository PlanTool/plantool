;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block World  Domain definition
;; 
;; USB
;;

(define (domain blocks_world)
  (:requirements :strips)
  (:predicates (on-table ?x)
               (on ?x ?y)		       
               (clear ?x))
  
  (:action MoveToTable
      :parameters (?x ?y)
      :precondition (and (clear ?x)
			 (on ?x ?y))
      :effect (and (clear ?y)
		   (on-table ?x)
		   (not (on ?x ?y))))
  
  (:action MoveToBlock1
      :parameters (?x ?y ?z)
      :precondition (and (clear ?x)
			 (clear ?z)
			 (on ?x ?y))
      :effect (and (clear ?y)
		   (on ?x ?z)
		   (not (clear ?z))
		   (not (on ?x ?y))))
  
  (:action MoveToBlock2
      :parameters (?x ?y)
      :precondition (and (clear ?x)
			 (clear ?y)
			 (on-table ?x))
      :effect (and (on ?x ?y)
		   (not (clear ?y))
		   (not (on-table ?x)))))

  

