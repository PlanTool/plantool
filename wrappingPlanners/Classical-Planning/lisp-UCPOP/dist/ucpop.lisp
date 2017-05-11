" (c) 1990-1995 Copyright (c) University of Washington
  Written by Tony Barrett, J Scott Penberthy, Stephen Soderland, and 
  Daniel Weld.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Program   : "UCPOP"
;;;
;;;  Description:
;;;  
;;;    These files contain the code for the planner UCPOP.  UCPOP
;;;    is a domain independent, partial order planner that handles
;;;    an expressive action representation (conditional effects,
;;;    universally quantified effects, and universally quantified
;;;    preconditions and goals).  UCPOP is both complete and sound.
;;;    The algorithm is described in:
;;;
;;;       Penberthy, J. S. and Weld, D., ``UCPOP: A Sound, Complete,
;;;       Partial-Order Planner for ADL,'' Third International
;;;       Conference on Knowledge Representation and Reasoning
;;;       (KR-92), Cambridge, MA, October 1992.
;;;
;;;    The representation of actions is a syntactic variant of a subset 
;;;    of Pednault's ADL.  See the BNF in the README file and the examples 
;;;    in domains.lisp
;;;
;;;    A plan includes a list of steps and a list of links between steps. 
;;;    Links are in the form  (id1 condition1 id2) where id1 is a step that
;;;    establishes condition1, which is in turn a precondition of step id2.
;;;    The plan also has a list of open conditions (not yet acheived) and a 
;;;    list of unsafe links (possibly clobbered by another step).  The other 
;;;    components of a plan are ordering constraints and bindings of variables.
;;;
;;;    Each iteration of the main control level creates possible next plans 
;;;    which are added to a priority queue.  New plans are created by adding
;;;    constraints to resolve unsafe links.  If there are no unsafe links,
;;;    new plans are created by adding a new step or new link to achieve
;;;    an open condition.  The plan is completed when there are no more unsafe
;;;    links or open conditions.  
;;;

(in-package "UCPOP")
(use-package "VARIABLE")

#+:clim-2 (import '(vcr:vcr-frame))

(defvar *nodes-visited*)
(defvar *d-sep* t)			; Use Peot and Smith's delay technique
(defvar *d-forall* t)	

(defvar *delay-link* nil) ; Are filter conditions used?

;; Make DISABLE only introduce ordering constraints when set to T
(defvar *ord-constrain-on-confront* nil)

;; Cause ucpop to recognize a threat only when the threatening step's
;; effect contradicts the link's label
(defvar *positive-threats* nil)

;;; Cause ucpop to check for safety constraints
(defvar *safety-p* nil)

;;; Cause ucpop to be aware of side effects.
;;; A step that possesses  side effects should not be instantiated
;;; as a new step by linking the side effects to an open condition.  
(defvar *side-effects* t)

;;; If flaw-fun is not specified in the problem, use this as default.
(defvar *default-flaw-fun* 'get-flaw)

;;; If rank-fun is not specified in the problem, use this as default.
;;; rank3 counts steps, unsafe conditions, and open conditions.
;;; rank4 does not count unsafes, or eq & neq OCs.
(defvar *default-rank-fun* 'rank3)

;;  The function generating the successor plans for a node X flaw
(defvar *new-plan-fun* 'new-plans)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outline of this file
;;;
;;;  1.   Handy interface for using the program
;;;  2.   Main control level of UCPOP
;;;  3.   Ranking partial plans
;;;  4.   Handling open conditions
;;;  4.1.   Adding new steps
;;;  4.2.   Adding links to old steps
;;;  4.3.   Adding links under closed world assumption
;;;  5.   Protecting causal links
;;;  5.1.   Resolving an unsafe link
;;;  5.2.   Detecting unsafety conditions
;;;  6.   handle partial orderings.
;;;  7.   Creating plan entries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Handy interface for using the program

;;; The flaw function selects a flaw from the list of flaws.
;;; It is used internally only!  Do not set it manually!
(defvar *flaw-fun* nil)

;;; This returns two values: a plan and a stat record
;;; If this is the top-level call, then both are printed.
(defun PLAN (initial
	     goals
	     &key
	     flaw-fun
	     rank-fun
	     (search-fun #'bestf-search))
  (setq *flaw-fun* (if flaw-fun flaw-fun *default-flaw-fun*))
  (setq rank-fun (if rank-fun rank-fun *default-rank-fun*))
  (multiple-value-bind (plan done? time q-len av-branch)
      (ucpop initial goals rank-fun search-fun)
    (values plan 
	    (make-stat :algo        "UCPOP"             
		       :date        (today)
		       :prob-num    1
		       :num-init    (length initial)       
		       :num-goal    (length goals)
		       :plan-len    (if plan (plan-high-step plan) 0)
		       :reached-max? (>= *plans-created* *search-limit*)
		       :complete?    done?
		       :time         time
		       :visited      *nodes-visited*     
		       :created      *plans-created*
		       :q-len        q-len
		       :ave-branch   (float av-branch)
		       :unify-count  *unify-count*
		       :rank-unifies *compute-rank-unifies*
		       :add-bindings *add-bind-count*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Main control level of UCPOP

;;; Returns 5 values: final plan, t or nil denoting success, run time
;;; length of q at termination, average branching factor
(defun UCPOP (init goal rank-fun search-fun)
  (init-ucpop)				; clear globals for metering
  (let* ((init-time (get-internal-run-time)))
    (multiple-value-bind (plan bfactor qlength)
	(funcall search-fun		; Perform the search!
		 (init-plan init goal)
		 #'plan-refinements 
		 #'plan-test 
		 rank-fun 
		 *search-limit*)
      (values plan			; the plan itself
	      (and plan (plan-test plan)) ; plan successfull?
	      (- (get-internal-run-time) init-time) ; time
	      qlength			; final length of the queue
	      bfactor))))		; average branching factor

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Make the first plan
(defun INIT-PLAN (init goal)
  (setf goal (convert-eqn goal))
  (test-wd goal nil)                    ; Test for valid goal equation
  (setf goal (compile-goal goal nil))
  (test-domain goal)
  (unless (listp (car init)) (error "illegal initial state specification"))
  (let ((g (instantiate-term goal :goal)))
    (tweak-plan 
     nil
     :reason '(:init)
     :new-steps
     (list (make-p-step :ID :Goal :precond g)
	   (make-p-step :ID 0 :add (list (make-effect :id 0 :add init))))
     :flaws nil
     :ordering nil
     :bindings (new-bindings)
     :high-step 0
     :add-goal (make-openc :condition (canonical g) :id :goal))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  goal test function for a search.
(defun plan-test (plan)  (null (plan-flaws plan)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  creates a list of one step refinements to the current plan.
(defun plan-refinements (plan)
  (incf *nodes-visited*)
  (funcall *new-plan-fun* plan (funcall *flaw-fun* plan)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  get a flaw in a plan.  it returns unsafe links before open conditions
(defun get-flaw (plan)
  (or (get-unsafe plan) (get-open plan) (get-forall plan)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  get a flaw in a plan.  ZLIFO style:  prefer 0 or 1 cost open conds
(defun get-hacked-zlifo-flaw (plan)
  (or (get-unsafe plan) (get-hacked-zlifo-open plan) (get-forall plan)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Given the flaw generate the one step refinements
(defun new-plans (plan f)
  (let ((kids (cond ((fact-p f) (handle-fact f plan))
		    ((openc-p f) (handle-open f plan))
		    ((unsafe-p f) (handle-unsafe-or-violation f plan))
		    ((univ-threat-p f) (handle-univ-threat f plan))
		    (t nil))))
#+:clim-2(vcr-frame plan f kids)
kids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. ranking partial plans

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  perform an a* search on the number of causal links in a plan.
(defun rank (plan)
  (let ((ret (length (plan-links plan))))
    (dolist (f (plan-flaws plan) ret)
      (when (openc-p f) (incf ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;;  to find a plan with the least searching.  ranking is based on the
;;;  number of unsafe plus the number of goals to be resolved plus the
;;;  number of steps.  including the number of steps prevents creating
;;;  long plans that add cycles of moves getting no closer to the goal.
(defun rank3 (plan)
  (+ (length (plan-steps plan))
     (length (plan-flaws plan))))

;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;;  This ranking function actual code from Schubert and Gerevini. In their 
;;;  EWSP95 paper, they argue it works much better. Counts only steps
;;;  and open conditions - not threats.

(defun rank4 (plan)
  (+ (length (plan-steps plan))
    (num-openc plan)))

(defun num-openc (plan)
  (let ((n 0))
    (dolist (c (plan-flaws plan))
	    (when (openc-p c) 
	      (when (and (not (eql (car (openc-condition c))
				   :eq))
			 (not (eql (car (openc-condition c))
				   :neq)))
		(setq n (+ n 1)))))
    n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. handling open conditions, disjunctions, and facts (foralls too)

(defun get-forall (plan)
  (if (null *d-forall*)
      (find-if #'univ-threat-p (plan-flaws plan))
    (dolist (f (plan-flaws plan) nil)
      (when (and (univ-threat-p f)
		 (null (find-if #'variable? 
				(bind-variable (univ-threat-term f)
					       (plan-bindings plan)))))
	(return f)))))

(defun handle-univ-threat (f plan)
  (let* ((forall (univ-threat-forall f))
	 (a (univ-threat-term f))
	 (univ nil)
	 (exis nil)
	 (binds (unify (forall-type forall) a (plan-bindings plan))))
    (cond (binds
	   (dolist (b (car binds))
	     (if (my-member (car b) (forall-vars forall))
		 (push b univ) (push b exis)))
	   (let ((eqn (v-sublis univ (forall-condition forall))))
	     (when (member (car a) *dynamic-pred*)
	       (setf eqn `(:or (:not ,a) (:and ,a ,eqn))))
	     (when exis
	       (setf eqn 
		 `(:or (:and ,@(mapcar 
				#'(lambda (b) `(:eq ,(car b) ,(cdr b)))
				exis)
			     ,eqn)
		       ,@(mapcar 
			  #'(lambda (b) `(:neq ,(car b) ,(cdr b)))
			  exis))))
	     (list (tweak-plan
		    plan
		    :reason `(:forall ,f)
		    :flaws (remove-1 f (plan-flaws plan))
		    :add-goal (make-openc :condition eqn
					  :id (forall-id forall))))))
	  (t (list (tweak-plan
		    plan
		    :reason `(:bogus)
		    :flaws (remove-1 f (plan-flaws plan))))))))

;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;; Return an open condition. 
;;; (Avoid facts that return :no-match-attempted and filter conditions that
;;;  cannot match any existing step yet)
(defun get-open (plan)
  (dolist (open-cond (plan-flaws plan) nil)
    (when (and (openc-p open-cond) 
	       (> (plan-high-step plan) (openc-src-limit open-cond)))
      (return open-cond))
    (when (fact-p open-cond)
      (setf (fact-bindings open-cond)
	(apply (fact-function open-cond) 
	       (mapcar 
		#'(lambda (x) (bind-variable x (plan-bindings plan)))
		(cdr (fact-condition open-cond)))))
      (unless (eq :no-match-attempted (fact-bindings open-cond))
	(return open-cond)))))

;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;; handle an open condition.
(defun handle-open (open-cond plan &aux (ret nil))
  (cond ((and (openc-p open-cond) (eq (car (openc-condition open-cond)) :or))
	 (handle-or open-cond plan))
	((setf ret (use-axiom open-cond plan)) (list ret))
	(*delay-link*
	 (if (> -1 (openc-src-limit open-cond))
	     (nconc (reuse-step open-cond plan) ; We have seen this cond before
		    (delay-openc open-cond plan))
	   (nconc (reuse-step open-cond plan)
		  (add-step open-cond plan)
		  (delay-openc open-cond plan))))
	(t (nconc (add-step open-cond plan)
		  (reuse-step open-cond plan)))))

;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;; by this point, all open conditions are facts that provide extra bindings.
(defun handle-fact (f plan)
  (mapcan 
   #'(lambda (tmp)
       (let ((p (tweak-plan
		 plan 
		 :reason `(:fact ,(fact-condition f))
		 :flaws (remove-1 f (plan-flaws plan))
		 :add-goal 
		 (make-openc :condition 
			     `(:and ,@(mapcar #'(lambda (x)
						  `(:eq ,(car x) ,(cdr x)))
					      tmp))
			     :id :goal))))
	 (when p (list p))))
   (fact-bindings f)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  handling disjunction goal
(defun handle-or (goal plan)
  (mapcan 
   #'(lambda (g)
       (let ((p (tweak-plan
		 plan 
		 :reason `(:goal ,g ,(openc-id goal))
		 :flaws (remove-1 goal (plan-flaws plan))
		 :add-goal (make-openc :condition g :id (openc-id goal)))))
	 (when p (list p))))
   (cdr (openc-condition goal))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.1. adding new steps and reusing steps

(defun delay-openc (open-cond plan)
  (when (member (if (eq :not (car (openc-condition open-cond)))
		    (caadr (openc-condition open-cond))
		  (car (openc-condition open-cond)))
		*dynamic-pred*)
    (let ((new-openc (copy-openc open-cond)))
      (setf (openc-src-limit new-openc) (plan-high-step plan))
      (list (tweak-plan plan
			:reason `(:delay-link ,(openc-condition open-cond) 
					      ,(openc-id open-cond))
			:flaws (cons new-openc
				     (remove-1 open-cond (plan-flaws plan))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;; returns a list of pairs of new plans and step#.  the new plan
;;; establishes the condition by adding a new step, instantiated 
;;; from a step in *templates* with post-conditions that unify with
;;; the condition. 
(defun add-step (open-cond plan)
  (let ((new-step-num (1+ (plan-high-step plan))))
    (mapcan
     #'(lambda (templ)
	 (new-link open-cond 
		   (instantiate-step templ new-step-num) 
		   plan))
     (get-opers (openc-condition open-cond)))))

;;;;;;;;;;;;;;;;;;;;;;;;                                                 
;;; quickly get those operators in the templates that match a condition
(defun get-opers (condition &aux (ret nil))
  (labels ((test-templ (templ)
	     (dolist (e (p-step-add templ) nil)
	       (dolist (a (effect-add e))
		 (when (and (eql (car a) (car condition))
			    (or (not (eq (car a) :not))
				(eql (caadr a) (caadr condition))))
		   (return-from test-templ t))))))
    (dolist (templ *templates* ret)
      (when (test-templ templ)
	(push templ ret)))))
	
;;;;;;;;;;;;;;;;;;;
;;;  reuse existing steps to satisfy an open condition
(defun reuse-step (open-cond plan)
  (let ((id (openc-id open-cond)))
    (mapcan
     #'(lambda (step)
	 (new-link open-cond 
		   (find step (plan-steps plan) :key #'p-step-id)
		   plan))
     (delete-if #'(lambda (x) (< x (openc-src-limit open-cond)))
		(possibly-prior id plan)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  using axioms to expand an open condition
(defun use-axiom (goal plan &aux (ret nil))
  (let* ((condition (openc-condition goal))
	 (c (if (eq :not (car condition)) (cadr condition) condition))
	 (id (openc-id goal))
	 (new-goal nil))
    (dolist (a *axioms*)
      (let ((binds (unify (effect-add a) c (plan-bindings plan))))
	(when binds 
	  (setf ret t)
	  (let ((g (peel-goal (car binds) a))
		(b (mapcar #'(lambda (e) `(:eq ,(car e) ,(cdr e)))
			   (peel-binds (car binds) a))))
	    (push (if b `(:and ,@b g) g) new-goal)))))
    (if (cdr new-goal) (push :or new-goal) (setf new-goal (car new-goal)))
    (when (eq :not (car condition)) 
      (setf new-goal (canonical `(:not ,new-goal))))
    (when ret
      (tweak-plan plan
		  :reason `(:goal ,new-goal ,id)
		  :flaws (remove-1 goal (plan-flaws plan))
		  :add-goal (make-openc :condition new-goal :id id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.2. adding links to steps

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  creates new plans to achieve an open condition by creating a causal link.
;;;  since there are zero or more ways that step can achieve the open
;;;  condition, this routine returns a list of zero or more plans.
(defun new-link (open-cond step plan)
  (let* ((condition (openc-condition open-cond))
	 (id (openc-id open-cond)))
    (nconc
     (when (and (eql (p-step-id step) 0) (eq (car condition) :not))
       (new-cw-link open-cond id plan (car (p-step-add step))))
     (mapcan
      #'(lambda (effect)
	  (unless (and *side-effects* (eq (effect-ranking effect) :side))
	    (mapcan
	     #'(lambda (add-cond)
		 (new-link* open-cond step plan effect add-cond condition id))
	     (effect-add effect))))
      (p-step-add step)))))

(defun new-link* (open-cond step plan effect add-cond condition id &aux (b nil))
  (when (setf b (unify add-cond condition (plan-bindings plan)))
    (let ((goal (peel-goal (car b) effect)))
      (when (> (effect-id effect) (plan-high-step plan))
	(setf goal (if goal `(:and ,goal ,(p-step-precond step))
		     (p-step-precond step))))
      (setf b (peel-binds (car b) effect))
      (let* ((new-l (make-link :id1 (effect-id effect) :id2 id
			       :condition condition :effect effect))
	     (p
	      (tweak-plan 
	       plan
	       :reason (if (> (effect-id effect) (plan-high-step plan)) 
			   `(:step ,(effect-id effect) ,add-cond)
			 `(:link ,(effect-id effect) ,add-cond))
	       :flaws (remove-1 open-cond (plan-flaws plan))
	       :new-steps (when (> (effect-id effect) (plan-high-step plan)) 
			    (list step))
	       :new-link new-l
	       :ordering (if (or (eql id :goal) (eql (effect-id effect) '0))
			     (plan-ordering plan)
			   (cons (list (effect-id effect) id)
				 (plan-ordering plan)))
	       :bindings (add-bind b (plan-bindings plan))
	       :add-goal (when goal 
			   (make-openc :condition goal
				       :id (effect-id effect)))
	       :high-step (max (effect-id effect) (plan-high-step plan)))))
	(when p
	  (let ((x (assoc :new-bindings (plan-other p))))
	    (if x (setf (cdr x) (nconc b (cdr x)))
	      (push (cons :new-bindings b) (plan-other p))))
	  (when *safety-p*
	    (dolist (v (detect-violation p new-l step))
	      (push v (plan-flaws p))))
	  (list p))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.3. Adding link to initial conditions for closed world model

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Creates a link from a negated goal to the initial conditions
;;;  If a link can be added, a new plan is created with that link
;;;  A link will be added (from :not P to initial conditions) if
;;;
;;;   - for every initial condition Q, P does not unify with Q or
;;;   - for every initial condition Q with which P may unify, there
;;;       exists a binding constraint such that P and Q do not unify
;;;
;;;  If one of the above conditions holds, then a new plan is returned
;;;  with the link added, and any appropriate binding constraints added.

(defun NEW-CW-LINK (open-cond id plan effect)
  (let* ((condition (openc-condition open-cond))
         (bind-goals nil))
    
    (dolist (e (effect-add effect))
      (let ((b (unify (cadr condition) e (plan-bindings plan))))
	(when b
	  (setf b (car b))
	  (unless b (return-from NEW-CW-LINK nil))
	  (push (if (= 1 (length b))
		    `(:neq ,(caar b) ,(cdar b))
		  `(:or ,@(mapcar #'(lambda (x) 
				      `(:neq ,(car x) ,(cdr x)))
				  ;; changed from (car b) to b
				  b)))
		bind-goals))))
    
    (let ((p
	   (tweak-plan 
	    plan
	    :reason `(:cw-assumption)
	    :flaws (remove open-cond (plan-flaws plan) :test #'eq)
	    :new-link (make-link :id1 0 :condition condition :id2 id)
	    :add-goal (when bind-goals
			(make-openc :condition `(:and ,@bind-goals)
				    :id (effect-id effect))))))
      (when p (list p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. Handling unsafe links

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tests whether one condition affects another.  This happens when one can
;;;  unify with the other or its negation when *positive-threats* is T, or
;;;  unify with the other's negation when *positive-threats* is nil.

;;; Efficiency issue:  probably reduce flexibility and introduce a function
;;; that can toggle definition of AFFECTS according to *positive-threats*,
;;; instead of checking it everytime.
;;; Speed of the code fragment run when *positive-threats* is nil is now 
;;; 2 conditionals.  (same as when *positive-threats* is T)

(defmacro AFFECTS (cond1 cond2 bindings)
  `(if *positive-threats* 
       (unify (if (eq :not (car ,cond1)) (cadr ,cond1) ,cond1)
	      (if (eq :not (car ,cond2)) (cadr ,cond2) ,cond2)
	      ,bindings)
     (cond ((eq :not (car ,cond1)) (if (eq :not (car ,cond2)) nil
				     (unify (cadr ,cond1) ,cond2 ,bindings)))
	   ((eq :not (car ,cond2)) (unify ,cond1 (cadr ,cond2) ,bindings))
	   (t nil))))

(defun GET-UNSAFE (plan)
  (if (null *d-sep*)
      (find-if #'unsafe-p (plan-flaws plan))
    (dolist (unsafe-ln (plan-flaws plan) nil)
      ;; Return bogus unsafety conditions (ordered that way)
      (when (unsafe-p unsafe-ln)
	(when (not (my-member (effect-id (unsafe-clobber-effect unsafe-ln))
			      (possibly-between 
			       (link-id1 (unsafe-link unsafe-ln))
			       (link-id2 (unsafe-link unsafe-ln))
			       plan)))
	  (return-from get-unsafe unsafe-ln))
	;; Return bogus (via variable constraints) or forced unsafety conds
	(let ((binds (affects (unsafe-clobber-condition unsafe-ln)
			      (link-condition (unsafe-link unsafe-ln))
			      (plan-bindings plan))))
	  (when (or (null binds)
		    (null (peel-binds (car binds) 
				      (unsafe-clobber-effect unsafe-ln))))
	    (return-from get-unsafe unsafe-ln)))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(defun HANDLE-UNSAFE-OR-VIOLATION (unsafe-ln plan)
  (if (null (unsafe-violation unsafe-ln))
      (handle-unsafe unsafe-ln plan)
    (handle-violation unsafe-ln plan)))


;;;  Code used to handle a detected unsafety condition.  Returns a list
;;;  of plan refinements that resolve the threat.
(defun HANDLE-UNSAFE (unsafe-ln plan &aux binds)
  (if (and (setf binds (affects 
			(unsafe-clobber-condition unsafe-ln) ;if still exists..
			(link-condition (unsafe-link unsafe-ln))
			(plan-bindings plan)))
	   (my-member (effect-id (unsafe-clobber-effect unsafe-ln))
		      (possibly-between (link-id1 (unsafe-link unsafe-ln))
					(link-id2 (unsafe-link unsafe-ln))
					plan)))
      (nconc (disable unsafe-ln (car binds) plan)
	     (demote unsafe-ln plan)
	     (promote unsafe-ln plan))
    (list (tweak-plan plan
		      :reason `(:bogus)
		      :flaws (remove-1 unsafe-ln (plan-flaws plan))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.1. Resolving an unsafe link

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  To resolve an unsafe link, add goals to disable the errant effect.
;;;  Systematicity bug -- should add goals and anti goals O[2^preconds]
;;;                       (but only adds anti-goals O[preconds])

(defun DISABLE (unsafe-ln binds plan)
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
	 (b (mapcar #'(lambda (x) `(:eq ,(car x) ,(cdr x)))
		    (peel-binds binds effect)))
	 (goal (if b `(:and ,@b ,(peel-goal binds effect))
		 (peel-goal binds effect)))
	 (p (tweak-plan plan 
			:reason `(:goal (:not ,goal) ,(effect-id effect))
			:ordering ord
			:flaws (remove-1 unsafe-ln (plan-flaws plan))
			:add-goal 
			(make-openc :condition (canonical `(:not ,goal))
				    :id (effect-id effect)))))
    (when (and *d-sep* b) (cerror "Disable D-SEP and continue" "D-SEP does not appear to be working.~%Try evaluating (setq *d-sep* nil) before evaluating this query.")
          (setq *d-sep* nil))
    (when p (list p))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  To resolve an unsafe link, add an ordering constraint so that the clobber
;;;  step comes before the establishing step.  The output is a list of new
;;;  plans.
(defun DEMOTE (unsafe-ln plan)
  (let* ((clobber-id (effect-id (unsafe-clobber-effect unsafe-ln)))
	 (id (link-id1 (unsafe-link unsafe-ln)))
	 (demotable (my-member clobber-id (possibly-prior id plan))))
    (if demotable
	(list (tweak-plan plan 
			  :reason `(:order ,clobber-id ,id)
			  :flaws (remove-1 unsafe-ln (plan-flaws plan))
			  :ordering (cons (list clobber-id id)
					  (plan-ordering plan)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  To resolve an unsafe link, add an ordering constraint so that the clobber
;;;  step comes after the second step of the link.  The output is a list of new
;;;  plans.
(defun PROMOTE (unsafe-ln plan)
  (let* ((clobber-id (effect-id (unsafe-clobber-effect unsafe-ln)))
	 (link (unsafe-link unsafe-ln))
	 (id (link-id2 link))
	 (promotable (my-member id (possibly-prior clobber-id plan))))
    (if promotable
	(list (tweak-plan plan :reason `(:order ,id ,clobber-id)
			  :flaws (remove-1 unsafe-ln (plan-flaws plan))
			  :ordering (cons (list id clobber-id)
					  (plan-ordering plan)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2. Detecting unsafety conditions

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tests whether a link is possibly clobbered by the delete conditions of any
;;;  steps possibly between the ID1 and ID2 of the link.  Returns nil if the 
;;;  link is safe, otherwise returns a list of (link clobber-id clobber-bind)
(defun TEST-LINK (plan link)
  (let ((new-unsafe nil)
	(bind2 (plan-bindings plan))
	(between-ids (intersection 
		      (possibly-prior (link-id2 link) plan)
		      (possibly-after (link-id1 link) plan))))
    (dolist (step (plan-steps plan) new-unsafe)
      (when (my-member (p-step-id step) between-ids)
	(dolist (effect (p-step-add step))
	  (dolist (add-cond (effect-add effect))
	    (when (affects add-cond (link-condition link) bind2)
	      (push (make-unsafe :link link 
				 :clobber-effect effect 
				 :clobber-condition add-cond) 
		    new-unsafe))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tests whether a step possibly clobbers any link.  Returns nil if all
;;;  links are safe, otherwise returns a list of unsafe
;;;  Assumes that step is only constrained wrt start and a single other point.
;;;  This lets us only have to check if a link's source is prior to the step.
;;;
;;;  Warniing this assumes that all of the effects have the same id.
(defun TEST-EFFECTS (plan effects &aux (ret nil))
  (when effects
    (let ((prior (possibly-prior (effect-id (car effects)) plan))
	  (after (possibly-after (effect-id (car effects)) plan)))
      (dolist (l (plan-links plan) ret)
	(when (and (my-member (link-id1 l) prior) 
		   (my-member (link-id2 l) after))
	  (dolist (effect effects)
	    (dolist (c (effect-add effect))
	      (when (affects c (link-condition l) (plan-bindings plan))
		(push (make-unsafe :link l 
				   :clobber-effect effect 
				   :clobber-condition c) 
		      ret)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. handle partial orderings.

(defun POSSIBLY-BETWEEN (s1 s2 plan)
  (intersection (possibly-after s1 plan) (possibly-prior s2 plan)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns list of step-id's of steps possibly prior to a given
;;; step.  Possibly prior always includes the initial conditions.
;;; First build a list of steps constrained to be not prior by
;;; the ordering constraints.  Then add to possibly prior all
;;; steps that aren't in the not-prior list.
(defun POSSIBLY-PRIOR (step-id plan)
  (when (not (eql step-id '0))
    (let ((table (make-list (1+ (plan-high-step plan))
			    :initial-element 'bad))
	  (poss-prior nil))
      (when (numberp step-id)              ; bug fix dbc 6/95  
        (setf (nth step-id table) nil))    ; bug fix dbc 6/95
      (dolist (s (plan-steps plan))
	(when (numberp (p-step-id s))
	  (setf (nth (p-step-id s) table) nil)))
      (when (numberp step-id)
	(dolist (l (plan-ordering plan))
	  (push (cadr l) (nth (car l) table)))
	(do ((c (list step-id) (cdr c)))
	    ((null c) nil)
	  (when (listp (nth (car c) table))
	    (nconc c (nth (car c) table))
	    (setf (nth (car c) table) 'after))))
      (dotimes (n (1+ (plan-high-step plan)) poss-prior)
	(when (listp (car table))
	  (push n poss-prior))
	(pop table)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns list of step-id's of steps possibly after a given
;;; step.  Possibly after always includes the goal conditions.
;;; First build a list of steps constrained to be not prior by
;;; the ordering constraints.  Then add to possibly after all
;;; steps that aren't in the not-after list.
(defun POSSIBLY-AFTER (step-id plan)
  (when (not (eq step-id :goal))
    (let ((table (make-list (1+ (plan-high-step plan))
			    :initial-element 'bad))
	  (poss-after '(:goal)))
      (dolist (s (plan-steps plan))
	(when (numberp (p-step-id s))
	  (setf (nth (p-step-id s) table) nil)))
      (when (< 0 step-id)
	(dolist (l (plan-ordering plan))
	  (push (car l) (nth (cadr l) table)))
	(do ((c (list step-id) (cdr c)))
	    ((null c) nil)
	  (when (listp (nth (car c) table))
	    (nconc c (nth (car c) table))
	    (setf (nth (car c) table) 'before))))
      (dotimes (n (plan-high-step plan) poss-after)
	(when (listp (cadr table))
	  (push (1+ n) poss-after))
	(pop table)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7. Creating plan entries

;;;;;;;;;;;;;;;;
;;;  Compute the peelings of an effect caused by binding a universal variable.
;;;  The last effect returned is the actual unpeeled effect.
;;;
;;;  Assumption: the universal variables are always the car of a binding
;;;(defun PEEL-GOAL (binds effect &aux (ret nil))
;;;  (cond ((effect-forall effect)
;;;	 (setf ret (copy-tree (effect-precond effect)))
;;;	 (dolist (b binds (if ret ret (effect-precond effect)))
;;;	   (when (my-member (car b) (effect-forall effect))
;;;	     (unless ret (setf ret (copy-tree (effect-precond effect))))
;;;	     (setf ret (nsubst (cdr b) (car b) ret)))))
;;;	(t (effect-precond effect))))

;;;;;;;;;;;;;;;;
;;;  Return a new goal where all of the universal variables are replaced
(defun PEEL-GOAL (binds effect)
  (cond ((effect-forall effect)
	 (let ((ret (copy-tree (effect-precond effect))))
	   (dolist (b binds ret)
	     (when (my-member (car b) (effect-forall effect))
	       (setf ret (nsubst (cdr b) (car b) ret))))))
	(t (effect-precond effect))))

;;;;;;;;;;;;;;;;
;;;  Return only those bindings that are *not* universal
(defun PEEL-BINDS (binds effect)
  (mapcan #'(lambda (b)
	      (unless (my-member (car b) (effect-forall effect)) 
		(list b)))
	  binds))
  
;;;;;;;;;;;;;;;;
;;;  Create a modifixed version of PLAN
(defun TWEAK-PLAN (plan &key		; initially written by jsp
			reason
                        (new-steps nil)
                        (new-link nil)
                        (flaws :same)
                        (ordering :same)
                        (bindings :same)
                        (high-step :same)
			(add-goal nil))
  "Return a new plan that is a copy of PLAN with the supplied
   instance variable tweaked."
  (flet ((tweak-it (keyword plan-accessor)
           (if (eq keyword :same) (funcall plan-accessor plan) keyword)))
    (when (tweak-it bindings #'plan-bindings)
      (let ((plan1 (make-plan
		    :steps (when plan (plan-steps plan))
		    :links (when plan (plan-links plan))
		    :flaws (tweak-it flaws #'plan-flaws)
		    :ordering (tweak-it ordering  #'plan-ordering)
		    :foralls (when plan (plan-foralls plan))
		    :bindings (tweak-it bindings #'plan-bindings)
		    :high-step (tweak-it high-step #'plan-high-step)
		    :other `((:new-goal . ,add-goal)
			     (:reason . ,reason)))))
	(when (and add-goal plan1)
	  (setf plan1 (handle-goal (openc-condition add-goal)
				   (openc-id add-goal)
				   plan1)))
	(when plan1
	  (when new-link (push new-link (plan-links plan1)))
	  (dolist (new-step new-steps)
	    (push new-step (plan-steps plan1)))
	  (new-unsafes plan1 new-link new-steps))))))

(defun NEW-UNSAFES (plan new-link new-steps)
  (when new-link			; new link
    (dolist (u (test-link plan new-link))
      (push u (plan-flaws plan))))
  (dolist (new-step new-steps plan)		; new steps
    (unless (test-step new-step plan)
      (return nil))))

(defun TEST-STEP (new-step plan1)
  (dolist (u (test-effects plan1 (p-step-add new-step)))
    (push u (plan-flaws plan1)))
  (dolist (f (plan-foralls plan1) plan1)
    (dolist (e (p-step-add new-step))
      (dolist (a (effect-add e))
	(let ((binds (unify (forall-type f) a (plan-bindings plan1))))
	  (when binds
	    (push (make-univ-threat :forall f :term a 
				    :step (p-step-id new-step))
		  (plan-flaws plan1))))))))

;;;;;;;;;;;;;;;;
;;; 
(defun HANDLE-GOAL (eqn id plan &aux (bs nil) (fs nil))
  (labels
      ((NEW-GOAL (n)
	 (when (equal n '(:not nil)) (return-from handle-goal nil))
	 (when (null n) (return-from new-goal nil))
	 (dolist (o (plan-flaws plan)) 
	   (when (and (openc-p o) (eql (openc-id o) id))
	     (when (equal (openc-condition o) n) 
	       (return-from new-goal nil))
	     (when (negates (openc-condition o) n) 
	       (return-from handle-goal nil))))
	 (dolist (l (plan-links plan)) 
	   (when (eql id (link-id2 l))
	     (when (equal (link-condition l) n)
	       (return-from new-goal nil))
	     (when (negates (link-condition l) n) 
	       (return-from handle-goal nil))))
	 t)
       (HANDLE-GOAL* (e)
	 (case (car e)
	   (:eq (push (cons (cadr e) (caddr e)) bs) nil)
	   (:neq (push `(:not ,(cadr e) . ,(caddr e)) bs) nil)
	   (:and (mapcan #'handle-goal* (cdr e)))
	   (:forall (handle-forall e id plan))
	   (:exists (handle-goal* (handle-exists e)))
	   (otherwise
	    (let ((temp (find (car e) *facts* :key #'car)))
	      (cond (temp 
		     (list (make-fact :condition e :function (cdr temp))))
		    ((new-goal e)
		     (list (make-openc :condition e :id id)))
		    (t nil)))))))
    (setf fs (handle-goal* eqn))
    (push (cons :new-bindings bs) (plan-other plan))
    (setf bs (add-bind bs (plan-bindings plan)))
    (when bs
      (setf (plan-bindings plan) bs)
      (setf (plan-flaws plan) (nconc fs (plan-flaws plan)))
      plan)))

;;;;;;;;;;;;;;;;
;;;  See if one goal is a strict negation of the other.
(defun NEGATES (n1 n2)
  (let ((p1 (if (eq (car n1) :not) n1 n2))
	(p2 (if (eq (car n1) :not) n2 n1)))
    (and (eq (car p1) :not) (equal p2 (cadr p1)))))

;;;;;;;;;;;;;;;;
;;; 
(defun HANDLE-FORALL (goal id plan)
  (let* ((priors (possibly-prior id plan))
	 (ft (nth 2 goal))
	 (forall (make-forall :id id :vars (nth 1 goal) 
			      :type ft :condition (nth 3 goal))))
    (push forall (plan-foralls plan))
    (mapcan 
     #'(lambda (s)
	 (when (my-member (p-step-id s) priors)
	   (mapcan 
	    #'(lambda (e)
		(mapcan 
		 #'(lambda (a)
		     (let ((binds (unify ft a (plan-bindings plan))))
		       (when binds
			 (list (make-univ-threat :forall forall :term a 
						 :step (p-step-id s))))))
		 (effect-add e)))
	    (p-step-add s))))
     (plan-steps plan))))

;;;;;;;;;;;;;;;;
;;; 
(defun HANDLE-EXISTS (goal)
  (let ((alst (mapcar #'(lambda (x) (cons x (uniquify-var x)))
		      (cadr goal))))
    (v-sublis alst `(:and ,(caddr goal) ,(cadddr goal)))))

;;;;;;;;;;;;;;;;
;;;
(defun V-SUBLIS (alst e)
  (cond ((consp e) (cons (v-sublis alst (car e)) (v-sublis alst (cdr e))))
	((variable? e)
	 (let ((a (assoc e alst)))
	   (if a (cdr a) e)))
	(t e)))
	 
;;;;;;;;;;;;;;;;
;;;  Make a plan and keep track of the total number of plans created.
(defun MAKE-PLAN (&rest args)
  (setq *plans-created* (+ 1 *plans-created*))
  (apply #'make-plan* args))


