" (c) 1993,1994 copyright (c) university of washington
  written by tony barrett.

  all rights reserved. use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  all copies must
  include this copyright message.  this software is made available as is, and
  neither the authors nor the university of washington make any warranty about
  the software or its performance.

  when you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "UCPOP")

(use-package "VARIABLE")
(export '(*verbose*))

(defvar *verbose* nil)			; Print whole plan?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Data Structures

(defstruct (PLAN (:constructor make-plan*)
	    (:print-function print-plan))
  steps					; list of steps
  links					; list of causal links
  flaws					; list of OPENCs and UNSAFEs
  ordering				; list of (ID1 ID2)
  foralls				; list of all forall preconditions
  bindings				; binding constraints
  high-step				; integer # of highest step in plan
  (other nil)				; an alist of scr & debug stuff
  )

;;;;;;;;;;;;;;;;
;;; Topological Sort   
;;; Returns correct order: first step at head
;;; Input: max is an integer
;;;    Ordering is a list of pairs (f l) where step number f must be before l
;;;    f, l <= max
;;; See Aho, Hopcoft, Ullman p70 for faster way
(defun TOP-SORT (ordering max)
  (let ((a (top-sort1 (copy-tree ordering) max))
	(b nil))
    (dotimes (i max (nconc a b))
      (when (not (my-member (1+ i) a :test #'eql))
	(push (1+ i) b)))))

;;; Topological Sort util  -   This code is DESTRUCTIVE!  Pass it a copy!
(defun TOP-SORT1 (ordering max)
  (when ordering
    (let ((as (mapcar #'cadr ordering)))
      (do ((p ordering (cdr p)))
	  ((not (my-member (caar p) as))
	   (cons (caar p)
		 (top-sort1 (delete-if #'(lambda (x) 
					   (eql (car x) (caar p))) ordering)
			    (- max 1))))))))

;;;;;;;;;;;;;;;;
;;; This version does a toplogical sort and then prints out the steps
;;; in the order in which they should be executed
(defun DISPLAY-PLAN (plan &optional (stream t) ignore)
  (declare (ignore ignore))
  (let ((steps (make-array (+ 1 (plan-high-step plan))))
        (order (top-sort (plan-ordering plan) 
			 (plan-high-step plan)))
        (goal nil))
    (format stream "~%Initial  : ~a~%" 
	    (mapcar #'(lambda (x) (bind-variable x (plan-bindings plan)))
		    (effect-add 
		     (car (p-step-add 
			   (find 0 (plan-steps plan) :key #'p-step-id))))))
    (dolist (step-n (plan-steps plan))
	    (cond 
	     ((eql (p-step-id step-n) :Goal)
	      (setf goal (p-step-precond step-n)))
	     (t
	      (setf (aref steps  (p-step-id step-n)) step-n))))
    (dotimes (i (plan-high-step plan))
	     (let* ((sn (nth i order))
		    (step (aref steps sn)))
	       (format stream "~%Step ~2a : ~20a   Created ~2a" 
		       (+ 1 i)
		       (when step
			 (bind-variable (p-step-action step)
					(plan-bindings plan)))
		       sn)
	       (dolist (l (plan-links plan))
		 (when (eql (link-id2 l) sn)
		   (format stream "~%           ~2a -> ~20a"
			   (link-id1 l) 
			   (bind-variable (link-condition l)
					  (plan-bindings plan)))
		   (dolist (u (plan-flaws plan))
		     (when (and (unsafe-p u) (eq l (unsafe-link u)))
		       (format stream "<~a>" 
			       (effect-id (unsafe-clobber-effect u)))))))
	       (dolist (l (plan-flaws plan))
		 (when (and (openc-p l) (eql (openc-id l) sn))
		   (format stream "~%           ?? -> ~20a"
			   (bind-variable (openc-condition l)
					  (plan-bindings plan)))))))
    
    (format stream "~%~%Goal    : ~a" goal)
    (dolist (l (plan-links plan))
      (when (eql (link-id2 l) :Goal)
	(format stream "~%           ~2a -> ~20a"
		(link-id1 l) 
		(bind-variable (link-condition l) (plan-bindings plan)))
	(dolist (u (plan-flaws plan))
	  (when (and (unsafe-p u) (eq l (unsafe-link u)))
	    (format stream "<~a>" (effect-id (unsafe-clobber-effect u)))))))
    (dolist (l (plan-flaws plan))
      (when (and (openc-p l) (eql (openc-id l) :Goal))
	(format stream "~%           ?? -> ~20a"
		(bind-variable (openc-condition l) (plan-bindings plan)))))
    (format stream "~%Facts:")
    (dolist (l (plan-flaws plan))
      (when (fact-p l)
        (format stream " ~a"
                (bind-variable (fact-condition l) (plan-bindings plan)))))
    (unless (plan-flaws plan)
      (format stream "~%Complete!"))
    (let ((trace (cdr (assoc :trace (plan-other plan)))))
      (when trace (format stream "~a" trace)))))

;;;;;;;;;;;;;;;;
;;;  The terse print function
(defun PRINT-PLAN (plan &optional (stream t) depth)
  (declare (ignore depth))
  (if *verbose* (display-plan plan stream)
    (let ((o 0) (u 0) (f 0))
      (dolist (x (plan-flaws plan))
	(cond ((unsafe-p x) (incf u))
	      ((openc-p x) (incf o))
	      ((univ-threat-p x) (incf f))))
      (format stream "#plan<S=~a; O=~a; U=~a; F=~a>" 
	      (- (length (plan-steps plan)) 1) o u f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct FLAW 
  (rank nil))

(defstruct (OPENC (:include flaw)
	    (:print-function print-open))
  condition            ;; open precondition {condx}
  id                   ;; id of step which requires it
  (src-limit -1))      ;; Do not link to any step less than this

(defun print-open (self stream depth)
  (declare (ignore depth))
  (format stream "#<OPEN ~a step~a>" 
	  (openc-condition self) (openc-id self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (LINK (:print-function print-link))
  id1
  condition
  id2
  effect              ;; effect linked to
  )


(defun print-link (self stream depth)
  (declare (ignore depth))
  (format stream "#<LINK ~a ~a ~a>"
          (link-id1 self)
          (link-condition self)
          (link-id2 self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (UNSAFE (:include flaw)
	    (:print-function print-unsafe))
  link                 ;; id of threatened link
  clobber-effect       ;; effect which threatens it
  clobber-condition    ;; added condition which causes the threat
  violation)           ;; for safety violation

(defun print-unsafe (self stream depth)
  (declare (ignore depth))
  (format stream "#<UNSAFE link~a step~a>"
	  (unsafe-link self)
	  (effect-id (unsafe-clobber-effect self))))

(defun display-unsafe (self &optional (stream *debug-io*) (indent 0))
  (indent-stream stream indent)
  (format stream "Unsafe: ~a threatens ~a under ~a"
	  (unsafe-clobber-effect self)
	  (unsafe-link self)
	  (unsafe-clobber-condition self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (FACT (:include flaw))
  condition
  function
  (bindings nil))

;;; There's gotta be a better way!
(defun indent-stream (stream num)
  (format stream (make-string num :initial-element #\Space)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (STAT (:print-function print-stat))
  algo                                  ; tweak or strips
  date                                  ; when performed
  prob-num                              ; identifier
  num-init                              ; how many initial conditions
  num-goal
  plan-len                              ; how many steps
  reached-max?                          ; terminated because of nodes?
  complete?                             ; planner successful
  time                                  ; internal cpu time
  visited                               ; nodes-visted
  created                               ; calls to make-plan
  q-len                                 ; queue len at termination
  ave-branch                            ; average branching factor
  unify-count
  rank-unifies
  add-bindings
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Print out statistics from single run
(defun DISPLAY-STAT (s &optional (st t) ignore)
  (declare (ignore ignore))
  (format st "~%~%~a Stats: Initial terms = ~2a;   Goals = ~2a;  ~a (~a steps)" 
          (stat-algo s) (stat-num-init s) (stat-num-goal s)
          (if (stat-complete? s) "Success" "Failure")
          (stat-plan-len s))
  (format st "~%      Created ~a plans, but explored only ~a"
          (stat-created s) (stat-visited s))
  (format st "~%      CPU time: ~9,4F sec" (/ (stat-time s) 
                                        internal-time-units-per-second))
  (format st "~%      Branching factor: ~6,3F" (stat-ave-branch s))
  (format st "~%      Working Unifies: ~4a" 
          (- (stat-unify-count s) (stat-rank-unifies s)))
  (format st "~%      Bindings Added: ~4a"  (stat-add-bindings s)))


(defun PRINT-STAT (s &optional (stream t) depth)
  (declare (ignore depth))
  (if *verbose* (display-stat s stream)
    (format stream "#Stats:<cpu time = ~,4F>" 
	    (float (/ (stat-time s) internal-time-units-per-second)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (P-STEP (:print-function print-p-step))
  ID					; integer step number
  action				; formula such as (puton ?X ?Y)
  parms					; parameters
  precond				; conditions like (clear ?X)
  add					; effects asserted by step
  (cache nil)				; A cache of existing steps
  )

(defun PRINT-P-STEP (s &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<~a>" (car (p-step-action s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (EFFECT (:print-function print-effect))
  (ID nil)				; Effects associated step
  (forall nil)
  (precond nil)				; Preconditions of effect
  (ranking nil)				; one of :primary, :side or nil (= N/A)
  add)					; Added entry

(defun PRINT-EFFECT (s &optional (stream t) depth)
  (declare (ignore depth))
  (if (and (effect-id s) (eql 0 (effect-id s)))  (format stream "initial-state")
    (format stream "[~a:~a->~a]"
	    (effect-id s) (effect-precond s) (effect-add s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct FORALL
  ID					; integer step number
  vars					; (forall *vars*
  type					;         *type*
  condition)				;         *condition*)


(defstruct (UNIV-THREAT (:include flaw))
  forall
  term
  step)

