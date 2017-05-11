" (c) 1990-1995 Copyright (c) University of Washington
  Written by Lenhart Schubert and Alphonso Gerevini.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

;;;;;;;;;;;;;;;;;; READ ZLIFO.DOC FOR MORE DETAILS ;;;;;;;;;;;;;;;;;;;;
;;  
;; Sept. '95 Integrated with UCPOP 4.0 -- Marc Friedman
;; Dec. '94 (original version); added facts, disjunctive open conds,
;; and some cleanups Sep.'95
;;
;; Written by Alfonso Gerevini while visiting the CS Dept. of the U. 
;; of Rochester. gerevini@{cs.rochester.edu,irst.itc.it} 
;;
;; This Lisp code is being distributed for non-commercial research
;; purposes to the people who inquired about it in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY.
;;
;; HOW TO INTEGRATE ZLIFO INTO UCPOP:
;; Add the following functions to the file ucpop.lisp. Note that
;; the UCPOP functions GET-OPEN and NEW-PLANS should be replaced
;; with the new ones. 
;;
;; WARNING
;; The current implementation of ZLIFO has NOT been tested with
;; domains containing axioms and operators with universally quantified
;; variables. The code is not optimized.

(in-package "UCPOP")

(defvar *zlifo-is-on* nil)
(defvar *zlifo-d-sep* nil)
(defvar *zlifo-rank* nil)
(defvar *zlifo-flaw* nil)
(defvar *zlifo-plan* nil)

(defun turn-zlifo-on ()
  (when (not *zlifo-is-on*)
    (setq *zlifo-d-sep* *d-sep* )
    (setq *zlifo-rank* *default-rank-fun* )
    (setq *zlifo-flaw* *default-flaw-fun* )
    (setq *zlifo-plan* *new-plan-fun* )

    (setq *zlifo-is-on* t)

    (setq *d-sep* t)
    (setq *default-rank-fun* 'rank4)
    (setq *default-flaw-fun* 'get-zlifo-flaw)
    (setq *new-plan-fun* 'new-plans-zlifo))
  )

(defun turn-zlifo-off ()
  (when *zlifo-is-on*
    (setq *zlifo-is-on* nil)

    (setq *default-rank-fun* *zlifo-rank*)
    (setq *default-flaw-fun* *zlifo-flaw*)
    (setq *new-plan-fun* *zlifo-plan*)
    (setq *d-sep* *zlifo-d-sep*))
  )

(defun GET-ZLIFO-FLAW (plan)
  (or (get-unsafe plan) (get-zlifo-open plan) (get-forall plan)))

(defun GET-ZLIFO-OPEN (plan)
  "Returns a list (OC M), where OC is the open condition of the
   input plan [plan], and M is the number of ways in which OC can 
   be resolved (i.e., the numeber of refined plans that will be 
   generated)"
  
  (do* ((open-cond-list (plan-flaws plan) (cdr open-cond-list))
	(open-cond (car open-cond-list) (car open-cond-list))
	(cond-found nil)
	(num-matches nil)
	(num-matches-or nil)
	(ret1 nil)
	(ret2 nil)
	(ret2-1 nil)       
	(ret3 nil))
      ((or (null open-cond-list) cond-found) 
       (cond (cond-found
	      (list ret1 0))                
	     (t
	      (if (and (null ret1) (null ret2) (null ret2-1) (null ret3)) nil
		(cond ((not (null ret1)) 
		       (list ret1 1))         
		      ((not (null ret2))
		       (list ret2 1))          
		      ((not (null ret2-1))
		       (list ret2-1 1))        
		      (t (list ret3 num-matches)))))))
      (cond ((openc-p open-cond)
	     (cond ((eq (car (openc-condition open-cond)) :or)
		   ;; open cond is a disjunctive goal
		    (setq num-matches-or 
			  (OR-MATCHES (openc-condition open-cond) plan))
		    (cond ((zerop num-matches-or)
			   (setq cond-found t)
			   (setq num-matches 0)
			   (setq ret1 open-cond))
			  ((eql num-matches-or 1)
			   (when (null ret2) 
				 (setq num-matches 1) 
				 (setq ret2 open-cond)))
			  (t
			   (when (null ret3) 
				 (setq num-matches num-matches-or)
				 (setq ret3 open-cond)))))
		   (t  
		    ;; open cond is not a disjunctive goal
		    (let ((can-start (I-MATCHES open-cond plan))
			  (can-steps (S-MATCHES open-cond plan))
			  (can-opers (O-MATCHES 
				      (openc-condition open-cond) plan)))
		      (cond ((and (zerop can-opers)
				  (zerop can-steps)
				  (zerop can-start))
			     (setq num-matches 0)
			     (setq ret1 open-cond)
			     (setq cond-found t))
			    ((and (eql can-opers 1)
				  (zerop can-steps)
				 (zerop can-start))
			     (when (null ret1) 
				   (setq ret1 open-cond)))
			    ((and (eql can-start 1) 
				  (zerop can-opers)
				  (zerop can-steps))
			     (when (null ret2) 
				   (setq ret2 open-cond)))
			    (t 
			     (when (null ret3) 
				   (setq num-matches 
					 (+ can-opers can-steps can-start))
				   (setq ret3 open-cond))))))))
	    ((fact-p open-cond)
	     (setf (fact-bindings open-cond)
		   (apply (fact-function open-cond) 
			  (mapcar 
			   #'(lambda (x) 
			       (bind-variable x (plan-bindings plan)))
			   (cdr (fact-condition open-cond)))))
	     (unless (eq :no-match-attempted (fact-bindings open-cond))
		     (cond ((zerop (length (fact-bindings open-cond)))
			    (setq cond-found t) 
			    (setq ret1 open-cond))
			   ((eql (length (fact-bindings open-cond)) 1)
			    (when (null ret2-1) 
				  (setq ret2-1 open-cond)))
			   (t 
			    (when (null ret3) 
			      (setq num-matches 
				    (length (fact-bindings open-cond)))
			      (setq ret3 open-cond)))))))))


#|
;; ZLIFO without the sub-preference among OCs which can be achieved 
;; in a unique way
(defun GET-ZLIFO-OPEN2 (plan)
  (do* ((open-cond-list (plan-flaws plan) (cdr open-cond-list))
	(open-cond (car open-cond-list) (car open-cond-list))
	(cond-found nil)
	(num-matches nil)
	(num-matches-or nil)
	(ret1 nil)
	(ret2 nil)
	(ret3 nil))
       ((or (null open-cond-list) cond-found) 
	(cond (cond-found
	       (list ret1 num-matches))
	      (t
	       (if (and (null ret1) (null ret2) (null ret3)) nil
		 (cond ((not (null ret1)) 
			(list ret1 num-matches))
		       ((not (null ret2))
			(list ret2 num-matches))
		       (t (list ret3 num-matches)))))))
       (cond ((openc-p open-cond)
	      (cond ((eq (car (openc-condition open-cond)) :or)
		     ;; open cond is a disjunctive goal
		     (setq num-matches-or 
			   (OR-MATCHES (openc-condition open-cond) plan))
		     (cond ((zerop num-matches-or)
			    (setq cond-found t)
			    (setq num-matches 0)
			    (setq ret1 open-cond))
			   ((eql num-matches-or 1)
			    (when (null ret1) 
				  (setq num-matches 1) 
				  (setq ret1 open-cond)))
			   (t
			    (when (and (null ret2) (null ret1)) 
				  (setq num-matches num-matches-or)
				  (setq ret2 open-cond)))))
		    (t  
		     ;; open cond is not a disjunctive goal
		     (let ((can-start (I-MATCHES open-cond plan))
			   (can-steps (S-MATCHES open-cond plan))
			   (can-opers (O-MATCHES
				       (openc-condition open-cond) plan)))
		       (cond ((and (zerop can-opers)
				   (zerop can-steps)
				   (zerop can-start))
			      (setq ret1 open-cond)
			      (setq num-matches 0)
			      (setq cond-found t))
			     ((eql (+ can-opers can-steps can-start) 1)
			      (cond ((null ret1) 
				     (setq num-matches 1)
				     (setq ret1 open-cond))
				    (t nil)))
			     (t 
			      (when (and (null ret2) (null ret1)) 
				    (setq ret2 open-cond)
				    (setq num-matches 
					  (+ can-opers can-steps can-start)))))))))
	     ((fact-p open-cond)
	      (setf (fact-bindings open-cond)
		    (apply (fact-function open-cond) 
			   (mapcar 
			    #'(lambda (x) 
				(bind-variable x (plan-bindings plan)))
			    (cdr (fact-condition open-cond)))))
	      (unless (eq :no-match-attempted (fact-bindings open-cond))
		      (cond ((zerop (length (fact-bindings open-cond)))
			     (setq cond-found t) 
			     (setq num-matches 0)
			     (setq ret1 open-cond))
			    ((eql (length (fact-bindings open-cond)) 1)
			     (when (null ret1) 
				   (setq num-matches 1)
				   (setq ret1 open-cond)))
			    (t (when (null ret2) 
				     (setq num-matches 
					   (length (fact-bindings open-cond)))
				     (setq ret2 open-cond)))))))))
|#

(defun I-MATCHES (ocond plan)
  "Returns the number of (positive) conditions in initial state
   matching the open condition [ocond], given the set of bindings
   of the current plan [plan]"

  (let* ((condition (openc-condition ocond))
	 (step (find 0 (plan-steps plan) :key #'p-step-id))
	 (binds nil)
	 (num-matches 0))
    (if (eql (car condition) :not)
      	(let ((effect (car (p-step-add step))))
	  (setq binds (ACHIEVE-COND-CWA ocond 
					(plan-bindings plan) 
					effect))
	  (when (listp binds)
		(setq num-matches (+ 1 num-matches))))
      (dolist (effect (p-step-add step))	
	      (dolist (add-cond (effect-add effect))
		      (setq binds (unify add-cond 
					 condition 
					 (plan-bindings plan)))
		      (when binds
			    (setq num-matches (+ 1 num-matches))))))
    num-matches))


(defun S-MATCHES (open-cond plan)
  "Returns the number of effects of steps in the current plan [plan]
   matching the open condition [open-cond], given the set of bindings 
   of the current plan"

  (do* ((id (openc-id open-cond))
	(steps (possibly-prior id plan) (cdr steps))
	(reuse (ACHIEVE open-cond (car steps) plan)
	       (if (null steps) nil
		 (ACHIEVE open-cond (car steps) plan)))
	(num-reuse (if (not reuse) 0 reuse)
		   (if (not reuse) num-reuse (+ reuse num-reuse))))
      ((null steps) num-reuse)))


(defun O-MATCHES (condition plan)
  "Returns the number of effects of operators matching the open 
   condition [condition], given the set of bindings of the current 
   plan [plan]"
  
  (let ((num-opers 0))
    (dolist (templ *templates*)
      (dolist (e (p-step-add templ) nil)
	 (dolist (add-cond (effect-add e))
		 (when (UNIFY-EFFECT add-cond condition 
			      (plan-bindings plan) templ plan)
		       (setq num-opers (+ 1 num-opers))))))
    num-opers))


(defun UNIFY-EFFECT (cond-st cond bs oper plan)
  (if (not (unify cond-st cond bs)) nil
    (let ((eq-neqs nil)
	  (precs nil)
	  (bs-copy bs)
	  (new-bs  nil))
      (setq precs (if (listp (car (p-step-precond oper)))
		      (LIST-PRECS (p-step-precond oper))
		    (list (p-step-precond oper))))
      (dolist (p precs)
	      (case (car p)
		    (:eq (push (cons (cadr p) (caddr p)) eq-neqs))
		    (:neq (push `(:not ,(cadr p) . ,(caddr p)) eq-neqs))))
      (setf new-bs (add-bind eq-neqs bs-copy))
      (setf (plan-bindings plan) bs)
      new-bs)))


(defun LIST-PRECS (p)
  (if (eq (car p) :and)
      (list-precs (cdr p))
    p))


(defun OR-MATCHES (disjunctive-oc plan)
  "Compute the  number of ways of achieving a disjunctive OCs in 
   terms of the sum of its disjuncts which are not eq/neq constrains, 
   plus the eq/neq disjuncts which are individually consistent with 
   the current set of binding constraints."

  (let ((copy-b (plan-bindings plan))
	(eq-neqs nil)
	(b nil)
	(n 0)
	(list-ocs (cdr disjunctive-oc)))
    (dolist (p list-ocs)
	    (case (car p)
		  (:eq (setq b (list (cons (cadr p) (caddr p))))
		       (when (null (add-bind eq-neqs copy-b))
			     (setq n (+ 1 n))
			     (setq copy-b (plan-bindings plan))))  
		  (:neq (setq b (list `(:not ,(cadr p) . ,(caddr p))))
			(when (null (add-bind eq-neqs copy-b))
			      (setq n (+ 1 n))
			      (setq copy-b (plan-bindings plan))))
		  (otherwise
		   (setq n (+ 1 n)))))
    n))		


(defun ACHIEVE (open-cond st plan)
  "Check whether an open condition [open-cond] can be achieved
   by a step [st] in the current plan [plan]"

  (if (eql st 0) nil
    (let* ((condition (openc-condition open-cond))
	   (step (find st (plan-steps plan) :key #'p-step-id))
	   (can-achieve 0)
	   (binds nil))
      (dolist (effect (p-step-add step))
	      (dolist (add-cond (effect-add effect))
		      (setq binds (unify add-cond 
					 condition 
					 (plan-bindings plan)))
		      (when binds (setq can-achieve (+ 1 can-achieve)))))
      (if (zerop can-achieve) nil can-achieve))))


(defun ACHIEVE-COND-CWA (open-cond binds init-state)
  "Test whether a negative open condition [open-cond] matches the 
   initial state [init-state]"
 
  (let* ((condition (openc-condition open-cond))
         (bind-goals nil))
    (dolist (e (effect-add init-state))
      (let ((b (unify (cadr condition) e binds)))
	(when b
	  (setf b (car b))
	  (unless b (return-from ACHIEVE-COND-CWA nil))
	  (push (if (= 1 (length b))
		    `(:neq ,(caar b) ,(cdar b))
		  `(:or ,@(mapcar #'(lambda (x) 
				      `(:neq ,(car x) ,(cdr x)))
				  (car b))))
		bind-goals))))
    (list bind-goals)))


(defun NEW-PLANS-ZLIFO (plan f)
  "Given the flaw generate the one-step refinements"

  (if (null f) nil	
    (let* ((isfact? (if (listp f) 
			(if (fact-p (car f)) t nil) nil))
	   (kids (cond (isfact?       
			;; (car f) is a fact
			(handle-fact (car f) plan)) 
		       ((unsafe-p f) (handle-unsafe f plan))
		       ((univ-threat-p f) (handle-univ-threat f plan))
		       ((and (listp f) (not isfact?))
			;; (car f) is an open cond
			(if (not (null (car f)))
			    (handle-open (car f) plan)))
		       (t (error "incomplete plan has no flaws")))))
#+:clim-2(vcr-frame plan f kids)
      kids)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plan ranking functions for implementing S+OC+F, revised version of S+OC 
;; (S: steps, OC: open conds, F: facts; See TAI'95 paper)
;
;(defun RANK3 (plan)
;  (+ (length (plan-steps plan))
;    (num-openc plan)))
;
;(defun NUM-OPENC (plan)
;  (let ((n 0))
;    (dolist (c (plan-flaws plan))
;	    (when (or (openc-p c) (fact-p c)) 
;		  (if (openc-p c)
;		      (setq n (+ n 1)))))
;    n))








