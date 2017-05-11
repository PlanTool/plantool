
(in-package "UCPOP")
(use-package "VARIABLE")


;;; Introduce safety constraints defined in CNF.
;;;
;;; For all constraints that are satisfied in the initial state,
;;; they would be checked at each step of the planning process and
;;; any violations must be resolved.
;;;
;;; For examples of usage, see "domains.lisp."
;;;
;;; For details, please refer to
;;;     Dan Weld and Oren Etzioni, "The First Law of Robotics 
;;;     (a call to arms)," Proceedings of AAAI-94, pp. 1042-1047.


(defun DETECT-VIOLATION (plan link step)
  (when *safety-constraints*
    (let ((bind2 (plan-bindings plan))
	  (effect (link-effect link))
	  new-violations)
      (dolist (other-effect (remove effect (p-step-add step)) new-violations)
	(dolist (add-cond (effect-add other-effect))
	  (dolist (constraint (strip-conj *safety-constraints* :and))
	    (when (some #'(lambda (f)
			    (affects f add-cond bind2))
			(strip-conj constraint :or))
	      (push (make-unsafe :link link
				 :clobber-effect other-effect
				 :clobber-condition add-cond
				 :violation constraint)
		    new-violations))))))))


(defun HANDLE-VIOLATION (unsafe-ln plan)
  (or (DISAVOW unsafe-ln plan)
      (let* ((bind2 (plan-bindings plan))
	     (add-cond (unsafe-clobber-condition unsafe-ln))
	     (constraint (unsafe-violation unsafe-ln))
	     binds
	     violation)
	(dolist (f (strip-conj constraint :or))
	  (when (setf binds (affects f add-cond bind2))
	    (setf violation
	      (replace-binding (canonical `(:not ,(remove-literal f constraint)))
			       (car binds)))
	    (return)))
	(nconc (CONFRONT unsafe-ln (car binds) plan)
	       (EVADE violation unsafe-ln plan)))))


;;; If an effect is true in the initial states, then there is no problem 
;;; and the corresponding action may be added to the plan.
;;;
(defun DISAVOW (unsafe-ln plan)
  (let* ((bind2 (plan-bindings plan))
	 (inits (mapcar #'(lambda (x) (bind-variable x bind2))
			(initial-conditions plan)))
	 (add-cond (bind-variable (unsafe-clobber-condition unsafe-ln) bind2)))
    (when (my-member add-cond inits :test #'equal)
      (list (tweak-plan                
	     plan		  
	     :reason `(:bogus)
	     :flaws (remove-1 unsafe-ln (plan-flaws plan)))))))


;;; If the effect of an action is conditional of the form "when S then E"
;;; then the action may be added to the plan as long as the planner 
;;; commits to ensuring that execution will not result in E.  This is 
;;; achieved by adding ~S as a new subgoal to be made true at the time 
;;; when the action is executed.
;;;
(defun CONFRONT (unsafe-ln binds plan)
  (let* ((effect (unsafe-clobber-effect unsafe-ln))
	 (goal (peel-goal binds effect)))
    (when goal
      (let* ((ord (if *ord-constrain-on-confront* 
		      (nconc (when (< 0 (link-id1 (unsafe-link unsafe-ln)))
			       `((,(link-id1 (unsafe-link unsafe-ln)) 
				  ,(effect-id effect))))
			     (when (numberp (link-id2 (unsafe-link unsafe-ln)))
			       `((,(effect-id effect) 
				  ,(link-id2 (unsafe-link unsafe-ln)))))
			     (plan-ordering plan))
		    (plan-ordering plan)))
	     (p (tweak-plan plan 
			    :reason `(:goal (:not ,goal) ,(effect-id effect))
			    :ordering ord
			    :flaws (remove-1 unsafe-ln (plan-flaws plan))
			    :add-goal 
			    (make-openc :condition (canonical `(:not ,goal))
					:id (effect-id effect)))))
	(when *d-sep* (error "D-SEP does not appear to be working"))
	(when p (list p))))))


;;; Alternatively, by definition of violation it is legal to execute 
;;; an action Ap as long as the violation R will not be true *after*
;;; exectution.  The planner achieve this via goal regression, i.e. 
;;; by computing the *causation preconditions* for ~R and Ap, to be 
;;; made true at the time when Ap is executed.
;;;
(defun EVADE (violation unsafe-ln plan)  
  (let* ((effect (unsafe-clobber-effect unsafe-ln))
	 (ord (if *ord-constrain-on-confront* 
		  (nconc (when (< 0 (link-id1 (unsafe-link unsafe-ln)))
			   `((,(link-id1 (unsafe-link unsafe-ln)) 
			      ,(effect-id effect))))
			 (when (numberp (link-id2 (unsafe-link unsafe-ln)))
			   `((,(effect-id effect) 
			      ,(link-id2 (unsafe-link unsafe-ln)))))
			 (plan-ordering plan))
		(plan-ordering plan)))
	 (p (tweak-plan plan 
			:reason `(:goal (:not ,violation) ,(effect-id effect))
			:ordering ord
			:flaws (remove-1 unsafe-ln (plan-flaws plan))
			:add-goal 
			(make-openc :condition (canonical `(:not ,violation))
				    :id (effect-id effect)))))
    (when *d-sep* (error "D-SEP does not appear to be working"))
    (when p (list p))))


;;; Several utility functions

(defun strip-conj (l conj)
  (cond ((eq (car l) conj)
	 (cdr l))
	((atom (car l))
	 (list l))
	(t l)))

(defun remove-literal (e l)
  (let ((new-l (remove-1 e l :test #'equal)))
    (if (and (= (length new-l) 2)
	     (my-member (car new-l) '(:and :or)))
	(second new-l)
      new-l)))

(defun replace-binding (l binding)
  (cond ((null l) nil)
	((listp l)
	 (mapcar #'(lambda (e) (replace-binding e binding)) l))
	(t
	 (if (variable? l)
	     (cdr (assoc l binding))
	   l))))


