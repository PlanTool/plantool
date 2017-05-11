;;; This file illustrates some of the search control techniques added in this
;;; version 

;;; This ranking function considers primary effects in links
;;; All links are given a score of 1 unless they're primary,  i.e. the
;;; heuristic is that more primary links means a more correct plan

(defun rank4 (plan &aux (n 0))
  (+ (length (plan-steps plan))
     (length (plan-flaws plan))
     (dolist (l (plan-links plan) n)
       (unless (eq (effect-ranking (link-effect l)) :primary)
	 (incf n)))))


(define (domain init-flat-tire)
  
  (:operator cuss
      :effect (:not (annoyed)))
  
  (:operator open
      :parameters ((container ?x))
      :precondition (:and (:not (locked ?x)) (:not (open ?x)))
      :effect (open ?x))
  
  (:operator close
      :parameters ((container ?x))
      :precondition (open ?x)
      :effect (:not (open ?x)))
  
  (:operator fetch
      :parameters (?x (container ?y))
      :precondition (:and (:neq ?x ?y) (in ?x ?y) (open ?y))
      :effect (:and (have ?x)
		    (:not (in ?x ?y))))
  
  (:operator put-away
      :parameters (?x (container ?y))
      :precondition (:and (:neq ?x ?y) (have ?x) (open ?y))
      :effect (:and (in ?x ?y)
		    (:not (have ?x))))
  
  (:operator loosen
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (have wrench) (tight ?x ?y) 
			  (on-ground ?y))
      :effect (:and (loose ?x ?y)
		    (:not (tight ?x ?y))))
  
  (:operator tighten
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (have wrench) (loose ?x ?y) 
			  (on-ground ?y))
      :effect (:and (tight ?x ?y)
		    (:not (loose ?x ?y))))

  (:operator jack-up
      :parameters ((hub ?y))
      :precondition (:and (on-ground ?y) (have jack))
      :effect (:and (:not (on-ground ?y))
		    (:side (:not (have jack)))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:operator jack-down
      :parameters ((hub ?x))
      :precondition (:not (on-ground ?x))
      :effect (:and (on-ground ?x)
		    (:side (have jack))))
  
  (:operator undo
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) 
			  (:not (on-ground ?y)) (:not (unfastened ?y))
			  (have wrench) (loose ?x ?y))
      :effect (:and (have ?x) (unfastened ?y)
		    (:not (on ?x ?y)) (:not (loose ?x ?y))))
  
  (:operator do-up
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y)
			  (have wrench) (unfastened ?y)
			  (:not (on-ground ?y)) (have ?x))
      :effect
      (:and (loose ?x ?y) (:not (unfastened ?y)) (:not (have ?x))))

  (:operator remove-wheel
      :parameters ((wheel ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (:not (on-ground ?y))
			  (on ?x ?y) (unfastened ?y))
      :effect (:and (have ?x) (free ?y) (:not (on ?x ?y))))
  
  (:operator put-on-wheel
      :parameters ((wheel ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (have ?x) (free ?y) (unfastened ?y)
			  (:not (on-ground ?y)))
      :effect
      (:and (on ?x ?y) (:not (have ?x)) (:not (free ?y))))
  
  (:operator inflate
      :parameters ((wheel ?x))
      :precondition (:and (have pump) (:not (inflated ?x)) (intact ?x))
      :effect (inflated ?x)))

(define (problem fixit)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (in jack boot) (in pump boot)
	    (in wheel2 boot) (in wrench boot) 
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (:and 
	   (:not (open boot)) (in jack boot) (in pump boot)
	   (in wheel1 boot) (in wrench boot)
	   (tight nuts hub) (inflated wheel2)(on wheel2 hub)))

(define (problem fix1)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (in jack boot) (in pump boot)
	    (in wheel2 boot) (in wrench boot) 
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (:and (have jack) (have pump) (have wheel2)
		(have wrench)))

(define (problem fix2)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (open boot)
	    (have jack) (have pump) (have wheel2) (have wrench)
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (:and
	   (inflated wheel2) (:not (on-ground hub))
	   (loose nuts hub)))

(define (problem fix3)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts)
	    (container boot) (intact wheel2)
	    (have pump) (have wheel2)
	    (have wrench) (on wheel1 hub) (inflated wheel2)
	    (loose nuts hub))
    :goal (:and (tight nuts hub) (on-ground hub)
		(on wheel2 hub) 
		))

(define (problem fix4)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts)
	    (container boot) (intact wheel2)
	    (have jack) (have pump) (have wheel1)
	    (have wrench) (open boot)
	    (inflated wheel2) 
	    (on wheel2 hub) 
	    (tight nuts hub) (on-ground hub)
	    )
    :goal (:and
	   (in jack boot) (in pump boot) (in wheel1 boot)
	   (in wrench boot) (inflated wheel2) (on wheel2 hub) 
	   (tight nuts hub)))

(define (problem fix5)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot)
	    (open boot) (in jack boot) (in pump boot)
	    (in wheel1 boot)
	    (in wrench boot) (inflated wheel2) (on wheel2 hub) 
	    (tight nuts hub))
    :goal (:and
	   (:not (open boot)) (in jack boot) (in pump boot)
	   (in wheel1 boot)
	   (in wrench boot) (inflated wheel2) (on wheel2 hub) 
	   (tight nuts hub)))

