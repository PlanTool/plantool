" (c) 1990-1995 Copyright (c) University of Washington
  Written by Stephen Soderland, Tony Barrett and Daniel Weld.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "UCPOP")

(use-package "VARIABLE")

(export '(define reset-domain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Variables

(defvar *Templates* nil)		; list of dummy steps
(defvar *axioms* nil)			; list of axioms
(defvar *facts* nil)			; list of facts
(defvar *search-limit* 2000)		; max number of plans created


;;; Statistics related variables

(defvar *nodes-visited* 0)		; # plans visited in search
(defvar *plans-created* 0)		; # plans created in search
(defvar *branch* 0)			; compute avg branch factor

;;; Variables for Stuart Russell's IE search routine

(defvar *ie-limit*)
(defvar *ie-branches*)
(defvar *dynamic-pred* nil)	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Testing the validity of domains.

(defun TEST-DOMAIN (goal &aux univ-eff univ-pre primitive-pred
			      derived-pred fact-pred)
  (labels 
      ((precond* (eqn)
	 (case (car eqn)
	   ((:forall :exists)
	    (unless (member (caaddr eqn) univ-pre)
	      (push (caaddr eqn) univ-pre))
	    (precond* (cadddr eqn)))
	   ((:and :or :not)
	    (dolist (a (cdr eqn)) (precond* a)))
	   (otherwise nil))))
    (setf univ-eff nil 
	  univ-pre nil 
	  primitive-pred nil 
	  derived-pred nil 
	  fact-pred nil)
    (precond* goal)
    (dolist (f *facts*)
      (unless (member (car f) fact-pred)
	(push (car f) fact-pred)))
    (dolist (a *axioms*)
      (unless (member (car (effect-add a)) derived-pred)
	(push (car (effect-add a)) derived-pred)))
    (dolist (s *templates*)
      (precond* (p-step-precond s))
      (dolist (e (p-step-add s))
	(precond* (effect-precond e))
	(dolist (a (effect-add e))
	  (when (effect-forall e)
	    (unless (or (eq :not (car a)) (member (car a) univ-eff))
	      (push (car a) univ-eff)))
	  (when (eq :not (car a)) (setf a (cadr a)))
	  (unless (member (car a) primitive-pred)
	    (push (car a) primitive-pred)))))
    (let ((a (intersection univ-eff univ-pre)))
      (when a (error "Generator predicates ~a appear in universal effects" a)))
    (let ((a (intersection primitive-pred derived-pred)))
      (when a (error "Axiom predicates ~a appear in effects" a)))
    (let ((a (intersection primitive-pred fact-pred)))
      (when a (error "Fact predicates ~a appear in effects" a)))
    (let ((a (intersection derived-pred fact-pred)))
      (when a (error "Fact predicates ~a appear in axioms" a)))
    (let ((a (intersection fact-pred univ-pre)))
      (when a (error "Fact predicates ~a appear in generators" a)))
    (let ((a (intersection derived-pred univ-pre)))
      (when a (error "Axiom predicates ~a appear in generators" a))))
  (setf *dynamic-pred* primitive-pred))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Defining operators

(defun DEFOPER (name &key parameters at-time context precondition
			  effects effect description)
  (declare (ignore description))
  (when at-time 
    (error "Explicit time points not implemented."))
  (when context
    (error "Contexts are not implemented."))
  (setf precondition (convert-eqn precondition)
	effect (convert-eqn effect)
	effects (convert-eqn effects))
  (when (member name *templates* :key #'(lambda (a) (car (p-step-action a))))
    (error "Two actions with name ~a" name))
  (let* ((vars (parm-vars parameters))
	 (typ (parm-types parameters))
	 (pre (remove nil (if (equalp (car precondition) :and)
			      (append typ (cdr precondition))
			    (append typ (list precondition)))))
	 (eff (nconc (mapcar #'(lambda (x) 
				 (apply #'defeffect (cons vars x)))
			     effects)
		     (make-eff effect vars))))
    (setf pre (if (> (length pre) 1) (cons :and pre) (car pre)))
    (when pre (test-wd pre vars))
    (push (make-p-step :action (cons name vars)
		       :parms (parm-vars parameters)
		       :precond (canonical (compile-goal pre vars))
		       :add eff)
	  *templates*)))

(defun MAKE-EFF (e vlst &aux (ret nil))
  (labels 
      ((eff* (e when forall &key ranking)
	 (case (car e)
	   ((:primary :side)
	    (eff* (cadr e) when forall :ranking (car e)))
	   (:and
	    (dolist (x (cdr e)) (eff* x when forall)))
	   (:forall
	    (eff* (caddr e) when (append forall (cadr e))))
	   (:when
	       (eff* (caddr e) (cons (cadr e) when) forall))
	   ((:or :implies :exists)
	    (error "illegal effect equation"))
	   (otherwise
	    (push (defeffect vlst :effect e :forall forall :ranking ranking 
			     :when (cond ((null when) nil)
					 ((null (cdr when))
					  (car when))
					 (t (cons :and when))))
		  ret)))))
    (when e (eff* e nil nil))
    ret))

(defun DEFEFFECT (vlst &key effect forall when likely ranking)
  (when likely (error "Probability not implemented."))
  (dolist (f (parm-vars forall))
    (unless (variable? f) 
      (error "attempt to univerally quantify constant [~a]" f))
    (when (member f vlst)
      (error "multiple definition of variable [~a]" f))
    (push f vlst))
  (when when (test-wd when vlst))
  (setf effect (if (eq :and (car effect)) (cdr effect) (list effect)))
  (dolist (e effect)
    (dolist (v (parm-vars forall))
      (unless (member v (if (eq :not (car e)) (cadr e) e))
	(error "Universal effect [~a] should mention :forall variable [~a]"
	       e v)))
    (when (eq :not (car e)) (setf e (cadr e)))
    (unless (member (car e) *dynamic-pred*)
      (push (car e) *dynamic-pred*)))
  (when (parm-types forall)
    (if when (setf when `(:and ,@(parm-types forall) ,when))
      (setf when `(:and ,@(parm-types forall)))))
  (make-effect :forall (parm-vars forall)
	       :precond (canonical (compile-goal when vlst))
	       :ranking ranking
	       :add effect))

(defun CONVERT-EQN (eqn)
  (sublis '((when . :when)(and . :and)(or . :or)(not . :not)(imply . :imply)
	    (exists . :exists)(forall . :forall)(eq . :eq)(neq . :neq)) eqn))

(defun COMPILE-GOAL (eqn vars)
  (case (car eqn)
    (:imply
     (if (cdddr eqn) (error ":imply only takes 2 arguments")
       (compile-goal `(:or (:not ,(cadr eqn)) ,(caddr eqn)) vars)))
    (:and 
     (cons :and (mapcar #'(lambda (e) (compile-goal e vars))
			(cdr eqn))))
    (:or
     (cons :or (mapcar #'(lambda (e) (compile-goal e vars))
		       (cdr eqn))))
    (:not
     (if (cddr eqn) (error ":NOT only takes 2 arguments")
       (list :not (compile-goal (cadr eqn) vars))))
    ((:exists :forall)
     (let ((vs nil))
       (dolist (v (cdadr eqn))
	 (when (and (variable? v) (not (member v vars)))
	   (push v vars) (push v vs)))
       (when (null vs) (error "No new variables defined in ~a" eqn))
       (list (car eqn)
	     vs (cadr eqn) 
	     (when (caddr eqn)
	       (compile-goal (caddr eqn) vars)))))
    (otherwise eqn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Defining axioms

(defun DEFAXIOM (name &key context implies)
  (let ((vars nil))
    (dolist (v (cdr implies))
      (when (and (variable? v) (not (member v vars)))
	(push v vars)))
    (setf context (convert-eqn context))
    (test-wd context vars)
    (push (make-effect :id name
		       :forall vars
		       :precond (canonical (compile-goal context vars))
		       :add implies)
	  *axioms*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Defining facts

;;; Facts are precompiled into function objects.
(defun DEFFACT (name body)
  (push `(,(car name) . ,(compile nil `(lambda ,(cdr name) ,body)))
	*facts*))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Test to see if logical equation is syntacticly correct
(defun TEST-WD (wd vlst)
  (case (car wd)
    ((:exists :forall)
     (unless (null (cdddr wd)) (cerror "" "illegal expression: ~a" wd))
     (test-term (cadr wd) t)
     (dolist (v (cdadr wd))
       (when (and (variable? v) (not (member v vlst))) (push v vlst)))
     (test-wd (caddr wd) vlst))
    ((:eq :neq)
     (unless (null (cdddr wd)) (cerror "" "illegal expression: ~a" wd))
     (test-arguments (cdr wd) vlst))
    ((:and :or)
     (dolist (e (cdr wd))
       (test-wd e vlst)))
    (:not
     (unless (null (cddr wd)) (cerror "" "illegal expression: ~a" wd))
     (test-wd (cadr wd) vlst))
    (:imply 
     (unless (null (cdddr wd)) (cerror "" "illegal expression: ~a" wd))
     (test-wd (cadr wd) vlst)
     (test-wd (caddr wd) vlst))
    (otherwise
     (test-term wd vlst))))

;;; *** beware of bug!!! *** I only consider cases were :exists and :forall
;;; only has a single primitive test term.  (not an eqn as in PRODIGY)

(defun TEST-TERM (wd vlst)
  (when wd
    (unless (and (consp wd) (symbolp (car wd)))
      (cerror "" "illegal term: ~a" wd))
    (cond ((eq (car wd) :not)
	   (unless (null (cddr wd)) (cerror "" "illegal term: ~a" wd))
	   (test-term (cadr wd) vlst))
	  (t (test-arguments (cdr wd) vlst)))))

(defun TEST-ARGUMENTS (as vlst)
  (dolist (p as)
    (when (and (listp vlst) (variable? p) (not (my-member p vlst)))
      (cerror "" "unbound variable ~a" p))
    (when (consp p) (cerror "" "illegal constant ~a" p))))

(defun TEST-TYPED-VAR (v vlst)
  (unless (and (consp v)
	       (symbolp (car v)) (not (variable? (car v)))
	       (variable? (cadr v)) (null (cddr v))
	       (not (member (cadr v) vlst)))
    (cerror "" "illegal typed variable ~a" v)))

(defun PARM-VARS (parms)
  (mapcar #'(lambda (p) (if (listp p) (cadr p) p)) parms))

(defun PARM-TYPES (parms)
  (remove-if #'symbolp parms))

;;;;;;;;;;;;;;;;
;;; Convert a <WD> into canonical form
(defun CANONICAL (eqn &optional (not nil))
  (labels ((canonical* (e) (canonical e not)))
    (cond
     ((null eqn) (if not (list :not eqn) eqn))
     ((eq (car eqn) :not)
      (canonical (cadr eqn) (not not)))
     ((eq (car eqn) :or)
      (cons (if not :and :or) (mapcar #'canonical* (cdr eqn))))
     ((eq (car eqn) :and)
      (cons (if not :or :and) (mapcar #'canonical* (cdr eqn))))
     ((eq (car eqn) :forall)
      (list (if not :exists :forall)
	    (cadr eqn) (caddr eqn) (canonical* (cadddr eqn))))
     ((eq (car eqn) :exists)
      (list (if not :forall :exists)
	    (cadr eqn) (caddr eqn) (canonical* (cadddr eqn))))
     ((eq (car eqn) :eq)
      (cons (if not :neq :eq) (cdr eqn)))
     ((eq (car eqn) :neq)
      (cons (if not :eq :neq) (cdr eqn)))
     (t (if not (list :not eqn) eqn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. Miscelaneous other routines

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add new high step number to each variable id for every term in 
;;; step from template.
(defun INSTANTIATE-STEP (step num)
  (labels ((instantiate-effect (e)
	     (make-effect 
	      :id num
	      :forall (instantiate-term (effect-forall e) num)
	      :precond (instantiate-term (effect-precond e) num)
	      :add (instantiate-term (effect-add e) num))))
    (let ((s (find num (p-step-cache step) :key #'p-step-id)))
      (if s s
	(car (push 
	      (make-p-step
	       :id num
	       :action (instantiate-term (p-step-action step) num)
	       :precond (instantiate-term (p-step-precond step) num)
	       :add (mapcar #'instantiate-effect (p-step-add step)))
	      (p-step-cache step)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Get today's date
(defun TODAY ()
  (let ((d (multiple-value-list (get-decoded-time))))
    (format nil "~a/~a ~a at ~a:~a:~a"
            (nth 4 d) (nth 3 d) (nth 5 d) (nth 2 d) (nth 1 d) (nth 0 d))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Reset the global variables used for collecting statistics on the
;;;  planner.
(defun INIT-UCPOP ()
  (setf *nodes-visited* 0)
;;;  (setf *flaw-fun* nil)   SET BY THE PLAN FUNCTION - MTF
  (setf *unify-count* 0)
  (setf *compute-rank-unifies* 0)
  (setf *add-bind-count* 0)
  (setf *branch* 0)
  (setf *plans-created* 0))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A divide routine that does not blow up on zero.
(defun div* (x y)
  (if (= y 0) (* 99999 99999 99999) (/ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. Replace slow lisp utilities

(defun MY-MEMBER (elt lst &key (test #'eq))
  (dolist (l lst nil)
    (when (funcall test elt l) (return t))))

(defun REMOVE-1 (elt lst &key (test #'eq))
  (cond ((null lst) nil)
	((funcall test elt (car lst)) (cdr lst))
	(t (cons (car lst) (remove-1 elt (cdr lst) :test test)))))

(defun DELETE-1 (elt lst &key (test #'eq))
  (cond ((null lst) nil)
	((funcall test elt (car lst)) (cdr lst))
	(t (setf (cdr lst) (delete-1 elt (cdr lst) :test test))
	   lst)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; A merge routine that takes no space.  (modifies both A and B)
(defun MY-MERGE (b a test &aux ret temp)
  (cond ((null a) b)
	((null b) a)
	(t (cond ((funcall test (car a) (car b))
		  (setf ret a
			temp a)
		  (setf a (cdr a)))
		 (t
		  (setf ret b
			temp b)
		  (setf b (cdr b))))
	   (do () ((or (null a) (null b)))
	     (cond ((funcall test (car a) (car b))
		    (setf (cdr temp) a)
		    (setf  temp a)
		    (setf a (cdr a)))
		   (t
		    (setf (cdr temp) b)
		    (setf  temp b)
		    (setf b (cdr b)))))
	   (setf (cdr temp) (if a a b))
	   ret)))

(defun SETB (&rest args &aux (ret nil))
  "This routine functions as a setf to bind variables"
  (do* ((a args (cddr a)))
      ((null a) ret)
    (if (and (cdr a) (variable? (car a)))
	(push (cons (car a) (cadr a)) ret)
      (error "illegal bindings"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7. Print functions

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Print function for Templates
(defun PRINT-TEMPLATES (&optional (templates *templates*))
  (format t "~&~%Templates:")
  (dolist (templ-n templates)
    (let ((action (p-step-action templ-n))
	  (pre-cond (p-step-precond templ-n))
	  (add (p-step-add templ-n)))
      (format t "~&~a~%  Pre  : ~a~%  Add  :~%"
	      action pre-cond)
      (dolist (a add)
	(format t "  <when: ~a  Add: ~a>~%" 
		(effect-precond a) (effect-add a))))))
		
;;; Idea:
;;; Side effects can have 2 defs:
;;; 1. add link but not add step, or
;;; 2. no link or step,  just a threat.
;;;  Both are incomplete,  but 1 depends on goal ordering.
