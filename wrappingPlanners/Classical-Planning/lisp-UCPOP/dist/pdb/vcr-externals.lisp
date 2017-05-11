" (c) 1992-1995 Copyright (c) University of Washington
  Originally written by Tony Barrett. 
  Version (3.0) by David Christianson
  Version (4.0) enhancements and CLIM-2.0 conversion by Chung Kwok

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

;;; This file defines all the external interface function for linking up UCPOP and PDB.
;;; All the functions are exported in shell.lisp
;;; So to define an external function,  
;;; 1. add it here
;;; 2. export it in shell.lisp.
;;; 3. Call it in pdb code.

(in-package "VCR-EXTERNAL")
(use-package "VCR")
(use-package "UCPOP")

(import '(ucpop::ASSERT-DOMAIN ucpop::NEW-PLANS ucpop::SC-CONTROL
          ucpop::PROBLEM-NAME ucpop::PROBLEM-DOMAIN ucpop::PROBLEM-INITS
          ucpop::PROBLEM-GOAL ucpop::PROBLEM-RANK-FUN ucpop::DOMAIN-NAME
          ucpop::DOMAIN-OPERATORS ucpop::DOMAIN-AXIOMS ucpop::DOMAIN-FACTS
          ucpop::P-STEP-P ucpop::P-STEP-ACTION ucpop::OPENC-P
          ucpop::OPENC-CONDITION ucpop::UNSAFE-P
          ucpop::UNSAFE-CLOBBER-CONDITION ucpop::UNIV-THREAT-P
          ucpop::BIND-VARIABLE ucpop::PLAN-BINDINGS ucpop::PLAN-ORDERING
          ucpop::PLAN-HIGH-STEP ucpop::TOP-SORT ucpop::PLAN-LINKS
          ucpop::PLAN-FLAWS ucpop::PLAN-STEPS ucpop::PLAN-OTHER
          ucpop::UNSAFE-LINK ucpop::UNSAFE-CLOBBER-EFFECT ucpop::P-STEP-ADD
          ucpop::P-STEP-PRECOND ucpop::P-STEP-ID ucpop::LINK-ID2
          ucpop::OPENC-ID ucpop::EFFECT-ID ucpop::EFFECT-FORALL
          ucpop::EFFECT-PRECOND ucpop::EFFECT-ADD ucpop::LINK-ID1
          ucpop::LINK-EFFECT ucpop:: ucpop::PLAN-REFINEMENTS ucpop::PLAN-TEST
          ucpop::LINK-CONDITION ucpop::*TESTS* ucpop::*DOMAINS*
          ucpop::*SEARCH-LIMIT* ucpop::univ-threat-forall ucpop::forall-condition 
	  ucpop::*default-rank-fun*))

;;; Returns t if succeed,  otherwise nil
;(defun GET-PLANNING-RESULT (plan)
;  (plan-test plan))

(defun UCPOP-BUG (p f limit sc search-fun rank-fun domain ps record-firings)
  (declare (ignore ps record-firings))
  (let ((old-record *vcr-recording*))
      (setq *vcr-recording* T)
      (assert-domain domain)
      (cond ((= limit 1) (new-plans p f))
	    (sc (funcall 'sc:srch p sc limit))
	    (T
	     (funcall (intern search-fun 'ucpop) p #'ucpop::plan-refinements
		      #'ucpop::plan-test rank-fun limit)))
      (setq *vcr-recording* old-record)))


(defun UCPOP-PLAN (problem limit sc-fun search-fun debug)
  (let (
	(old-record *vcr-recording*)
	(old-limit (get-search-limit))
	)
    (setq *vcr-recording* debug)
    (set-search-limit limit)
    (shell-display-status "Planning...")
    (shell-show-busy t)
    (multiple-value-bind (plan stat)
	(cond (sc-fun (sc-control problem sc-fun))
	      ((symbolp search-fun) (funcall (intern search-fun 'ucpop) problem))
	      (t (window-msg "Error in planning" :window vcr::*shell*
			     :style :error)))
      (shell-show-busy nil)
      (shell-display-status "Planning...done")
      (vcr-display-stat plan stat)
      (set-search-limit old-limit)
      (setq *vcr-recording* old-record)
      (pdb-display)
      )))

;;; --- Problems

(defun GET-PROBLEMS ()
  *tests*)

(defun GET-PROBLEM-NAME   (p) (format nil "~a" (problem-name p)))
(defun GET-PROBLEM-DOMAIN (p) (problem-domain p))
(defun GET-PROBLEM-INITS  (p) (problem-inits p))
(defun GET-PROBLEM-GOAL   (p) (problem-goal p))
(defun GET-PROBLEM-RANK-F (p) (or (problem-rank-fun p) *default-rank-fun*))

;;; --- Domains

(defun GET-DOMAINS ()
  *domains*)

(defun GET-DOMAIN-NAME      (d) (domain-name d))
(defun GET-DOMAIN-OPERATORS (d) (domain-operators d))
(defun GET-DOMAIN-AXIOMS    (d) (domain-axioms d))
(defun GET-DOMAIN-FACTS     (d) (domain-facts d))

;;; --- Functions available

(let (
      (search-functions '(BF-CONTROL))
      (controller-functions '(BF-MIMIC))
      )
  (defun GET-SEARCH-FUNCTIONS ()
    search-functions)
  (defun GET-CONTROLLER-FUNCTIONS ()
    controller-functions))

;;; --- Search Limit & Stats

(defun SET-SEARCH-LIMIT (val)
  (setf *search-limit* val))

(defun GET-SEARCH-LIMIT ()
  *search-limit*)

;;; (defun GET-PLANS-SEARCHED () *nodes-visited*)

;;; This aids in output for various structures by producing a list corresponding to bound
;;; output.  Car of list can be used to shorten name.
(defun GET-BOUND-CLAUSE (object &optional plan)
  (cond ((p-step-p object)
	 (get-bound-clause (p-step-action object) plan))
	((openc-p object)
	 (get-bound-clause (openc-condition object) plan))
	((unsafe-p object)
	 (get-bound-clause (unsafe-clobber-condition object) plan))
	((univ-threat-p object) 
	 (get-bound-clause (forall-condition (univ-threat-forall object)) plan))
	(t
	 (cond ((null plan)
		(bind-variable object '(nil)))
	       (t
		(bind-variable object
			       (plan-bindings plan)))))))


;;; --- plans

(defun GET-PLAN-ORDERING  (plan)
  (top-sort (plan-ordering plan) (plan-high-step plan)))
(defun GET-PLAN-NUM-STEPS (plan) (plan-high-step plan))
(defun GET-PLAN-LINKS     (plan) (plan-links plan))
(defun GET-PLAN-UNSAFE    (plan) (remove-if-not #'unsafe-p (plan-flaws plan)))
(defun GET-PLAN-OPEN      (plan) (remove-if-not #'openc-p (plan-flaws plan)))
(defun GET-PLAN-FLAWS     (plan) (plan-flaws plan))
(defun GET-PLAN-STEPS     (plan) (plan-steps plan))
(defun GET-PLAN-STEP (plan id)
  (find (case id (:init 0) (t id)) (plan-steps plan) :key #'p-step-id))

;;; Returns plan difference.  Not quite finished yet,
;;; returns :fail for those things that it doesn't know.
(defun GET-PLAN-DIFF (plan)
  (let ((reason (cdr (assoc :reason (plan-other plan)))))
    (case (car reason)
      ;; unsafe
      (:protect
       (list (cadr reason)))
      ;; openc
      (:suspend-read-only
       (list (cadr reason)))
      ;; fact
      (:suspend-fact
       (list (cadr reason)))
      (:init :fail)
      ;; goal
      (:or (cadr reason))
      
      ;; effects
      (:separate
       (let ((unsafe (cadr reason)))
	 (list (unsafe-link unsafe)
	       (unsafe-clobber-effect unsafe))))
      (:peel
       (let ((unsafe (cadr reason)))
	 (list (unsafe-link unsafe)
	       (unsafe-clobber-effect unsafe))))
      (:disable
       (let ((unsafe (cadr reason)))
	 (list (unsafe-link unsafe)
	       (unsafe-clobber-effect unsafe))))
      (:promote
       (let ((unsafe (cadr reason)))
	 (list (unsafe-link unsafe)
	       (unsafe-clobber-effect unsafe))))
      (:demote
       (let ((unsafe (cadr reason)))
	 (list (unsafe-link unsafe)
	       (unsafe-clobber-effect unsafe))))
      
      (:expand-exec :fail)
      ;; open-cond
      (:meta-link
       (list (cadr reason)))
      ;; open
      (:expand
       (list (cadr reason)))
      ;; open
      (:collapse
       (list (cadr reason)))
      ;; step
      (:execute
       (list (second reason)))
      ;; step
      (:step
       (list (third reason)))
      (:retract-links :fail)
      ;; open
      (:get-ci
       (list (cadr reason)))
      ;; open
      (:partition
       (list (cadr reason)))
      ;; openc, add-cond
      (:link
       (list (cadr reason) (cadddr reason)))
      ;; unsafe
      (:bogus
       (list (cadr reason))))))

(defun GET-FULL-REASON-LABEL (plan)
  (let ((reason (cdr (assoc :reason (plan-other plan)))))
    (case (car reason)
      (:fact (format nil "Handle fact ~a"
		     (bind-variable (cadr reason)
				     (plan-bindings plan))))
      (:init "Initial plan")
      (:bogus "Remove bogus unsafe condition")
      (:goal 
       (format nil "Add goal [~a] for step [~d]"
	       (bind-variable (cadr reason)
			       (plan-bindings plan))
	       (bind-variable (caddr reason)
			       (plan-bindings plan))))
      (:step
       (format nil "Add step ~a to provide ~a"
	       (bind-variable (p-step-action 
				(find (cadr reason) (plan-steps plan)
				      :key #'p-step-id))
			       (plan-bindings plan))
	       (bind-variable (caddr reason)
			       (plan-bindings plan))))
      (:link
       (format nil "Link to step ~a for effect ~a" 
	       (bind-variable (cadr reason)
			       (plan-bindings plan))
	       (bind-variable (caddr reason)
			       (plan-bindings plan))))
      (:cw-assumption "Make a closed world assumption")
      (:forall (format nil "Handling universal threat:~%~a" (cadr reason)))
      (:order
       (format nil "Order step ~a before ~a"
	       (bind-variable (cadr reason)
			       (plan-bindings plan))
	       (bind-variable (caddr reason)
			       (plan-bindings plan)))))))

(defun GET-ICONIC-REASON-LABEL (plan)
  (let ((reason (cdr (assoc :reason (plan-other plan)))))
    (case (car reason)
      (:bogus "BOGUS")
      (:init "START")
      (:goal (format nil "G:~a" (caadr reason)))
      (:fact "FACT")
      (:step
       (format nil "S:~a" 
	       (car (p-step-action 
		     (find (cadr reason) (plan-steps plan)
			   :key #'p-step-id)))))
      (:link
       (format nil "L:~a" (cadr reason)))
      (:cw-assumption "CWA")
      (:order
       (format nil "~a<~a" (cadr reason) (caddr reason)))
      (:forall "UNIV-THREAT")			; a label is already displayed above.
      )))


(defun GET-PLAN-STEP? (plan)
  (eq (car (cdr (assoc :reason (plan-other plan)))) :step))


;;; --- steps

(defun GET-STEP-EFFECTS (step) (p-step-add step))
(defun GET-STEP-PRECOND (step) (p-step-precond step))
(defun GET-STEP-ID      (step) (let ((id (p-step-id step)))
				 (case id (0 :init) (t id))))
(defun GET-STEP-ACTION  (step) (p-step-action step))
(defun GET-STEP-COND (step plan)
  (mapcar #'link-condition
	  (remove-if-not
	   #'(lambda (l)
	       (eq (link-id2 l)  (p-step-id step)))
	   (plan-links plan))))

(defun GET-STEP-OPENC (step plan)
  (remove-if-not
   #'(lambda (f)
       (and (openc-p f)
	    (eq (openc-id f) (p-step-id step))))
   (plan-flaws plan)))


;;; --- effects

(defun GET-EFFECT-ID    (effect) (effect-id effect))
(defun GET-EFFECT-FORALL (effect) (effect-forall effect))
(defun GET-EFFECT-PRECOND (effect) (effect-precond effect))
(defun GET-EFFECT-ADD    (effect) (effect-add effect))

;;; --- links

(defun GET-LINK-TO       (link) (let ( (id (link-id2 link)) )
				  (case id (0 :init) (t id))))
(defun GET-LINK-FROM     (link) (let ( (id (link-id1 link)) )
				  (case id (0 :init) (t id))))
(defun GET-LINK-EFFECT   (link) (link-effect link))
(defun GET-LINK-OPENC    (link) (link-condition link))
(defun GET-LINK-ADD-COND (link) (link-condition link))
(defun GET-LINK-THREATS  (link plan)
  (remove-if-not #'(lambda (f)
		     (and (unsafe-p f) (eq (unsafe-link f) link)))
		 (plan-flaws plan)))

;;; --- flaws

;;; (defun GET-OPENC-ID  (openc)  (openc-id openc))
(defun GET-THREAT-ID (unsafe) (effect-id (unsafe-clobber-effect unsafe)))
(defun GET-THREAT-COND (unsafe) (unsafe-clobber-condition unsafe))

;;; --- facts

(defun GET-FACT-CONDITION (f) (car f))
(defun GET-FACT-FUNCTION  (f) (cdr f))

(defun get-step-execute? (step node) (declare (ignore step node)))
(defun get-step-executed? (step node) (declare (ignore step node)))

;;; --- search control

(defun GET-SC-RULES (sc)
  (rule-net::rule-net-rules (sc::sc-control-net sc)))

(defun GET-SC-RULE-NAME (rule)
  (rule-net::rule-name rule))

(defun display-text-plan (plan stream)
  (when plan (ucpop::display-plan plan stream)))

(defun display-plan-stat (stat stream)
  (when stat (ucpop::display-stat stat stream)))

;;;
;;; Note: this is a display function which depends on the planner
;;; used. It does not belong in this file.
(defun condition-label (cond)
  (let ((condition (if is-xii (cdr cond) cond)))
    (if (eq :not (car condition))
	(format nil "~~~a" (caadr condition))
      (format nil "~a" (car condition)))))

;;; No-op for ucpop.
(defun display-planner-specific-objects (frame stream node plan detail graphics))
