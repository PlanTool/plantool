" (c) 1992,1993,1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "RULE-NET")

(export '(defclause defrule defdemon defnet trace-rule untrace-rule 
	  assertion variable$ clear-firings profile show-profile
	  collect-firings dump-firings ignore-firings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following code is used to implement rule networks
;;; 
;;; Calling conventions:
;;;
;;; 1) Define clauses and rules with def
;;; 2) Create a rule-net
;;; 3) Tell the rule-net about assertions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Structures and global variables

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  An instance of this structure exists for every predicate that
;;;  a controller knows about.  It can be define by a lisp generator
;;;  function, or other rules.  (but not both!)
(defstruct (term (:print-function print-pred))
  name
  (vectors nil)				; Term defined by rules
  (generator nil)			; Uses lisp generator functions
  (consequents nil)			; rules that use this term
  (demon nil))				; Called whenever a new vector is added

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A single search control rule
(defstruct (rule (:print-function print-rule))
  consequents
  antecedents 
  trigger
  name)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A controller
(defstruct (rule-net (:print-function print-net))
  name
  (predicates nil)
  (rules nil))				; predicates of rule-net

(defvar *trace* nil)			; List of rules and clauses to trace
(defvar *profile* :off)
(defvar *trace-stream* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Handy interface for creating and using a rule net.

(defmacro DEFCLAUSE (net clause &body body)
  "Create a clause"
  `(setf (term-generator (get-predicate ',(car clause) ,net))
     #'(lambda ,(cdr clause) ,@body)))
    
(defmacro DEFDEMON (net clause &body body)
  "Create a clause"
  `(setf (term-demon (get-predicate ',(car clause) ,net))
     #'(lambda ,(cdr clause) ,@body)))

(defun DEFRULE (type name &key when effect)
  "Create a rule in rule net TYPE"
  (let ((instance
	 (make-rule 
	  :consequents (mapcar #'(lambda (e) 
				   (cons (get-predicate (car e) type) (cdr e)))
			       (if (eq :and (car effect)) (cdr effect) (list effect)))
	  :antecedents 
	  (mapcar #'(lambda (x) (cons (get-predicate (car x) type) (cdr x)))
		  when)
	  :trigger nil
	  :name name)))
    (push instance (rule-net-rules type))))

;    (dolist (a (remove-duplicates (rule-antecedents instance) 
;				  :key #'car :test #'eq))
;      (push instance (term-consequents (car a))))))

(defun DEFNET (name)
  "Create a rule network"
  (make-rule-net :name name))

(defun TRACE-RULE (&optional name)
  "Make rule NAME print a debugging message whenever it fires"
  (setf *trace* (cond ((and name (listp *trace*)) (cons name *trace*))
		      (name (list name))
		      (t t))))

(defun UNTRACE-RULE ()
  "Make none of the RULES print debugging messages"
  (setf *trace* nil))

(defun ASSERTION (net clause)
  "Add a clause to the rule net.  This inserts a vector."
  (let ((p (get-predicate (car clause) net)))
    (insert-vec p (cdr clause))))

(defun CLEAR-FIRINGS (net)
  "Clear out a rule net."
  (dolist (p (rule-net-predicates net))
    (setf (term-vectors p) nil
	  (term-consequents p) nil))
  (dolist (r (rule-net-rules net))
    (unless (rule-trigger r)
      (dolist (a (rule-antecedents r) 
		(error "rule [~a] has no trigger antecedents" (rule-name r)))
	(unless (term-generator (car a))
	  (setf (rule-trigger r) (car a))
	  (return nil))))
    (push r (term-consequents (rule-trigger r)))))

(defun PROFILE ()
  "toggle profiling on"
  (setf *profile* nil))

(defun SHOW-PROFILE ()
  "Display a profile and turn profiling off"
  (dolist (p (sort *profile* #'string-lessp 
		   :key #'(lambda (x) (symbol-name (car x)))))
    (format t "~%Rule: ~20a trigger=[~6a] fire=[~6a]" (car p) (cadr p) (caddr p)))
  (setf *profile* :off))

(defun COLLECT-FIRINGS ()
  "set up a string stream to cache *all* rule firings"
  (setf *trace-stream* (make-string-output-stream))
  t)

(defun IGNORE-FIRINGS ()
  "destroy the firings cache"
  (setf *trace-stream* t))

(defun DUMP-FIRINGS ()
  "dump the firings cache into a string and return the string"
  (when (not (eq t *trace-stream*))
    (get-output-stream-string *trace-stream*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Creating a rule net

(defun GET-PREDICATE (x type)
  "Get a predicate entry.  Converts name to a net-node."
  (dolist (p (rule-net-predicates type))
    (when (eq x (term-name p)) (return-from get-predicate p)))
  (car (push (make-term :name x) 
	     (rule-net-predicates type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Computing with the rule net

(defun UPDATE (rule assert &aux (trace nil) (profile nil))
  "This routine is called when rule RULE is triggered.  RULE may fire."
  (labels 
      ((update* (elts alist)
	 (cond ((null elts) 
		(when trace
		  (format *trace-stream* "~%Fire [~a]:~%  ~a ~%  -> ~a" 
			  (rule-name rule)
			  (mapcar #'(lambda (x) 
				      (cons (car x) 
					    (my-sublis alist (cdr x))))
				  (rule-antecedents rule))
			  (mapcar #'(lambda (x) 
				      (cons (car x) 
					    (my-sublis alist (cdr x))))
				  (rule-consequents rule))))
		(if profile (incf (caddr profile)))
		(dolist (rc (rule-consequents rule))
		  (let ((v (my-sublis alist (cdr rc))))
		    (insert-vec (car rc) v))))
	       (t (dolist (a (get-vecs (caar elts) (cdar elts) alist))
		    (update* (cdr elts) a))))))
    (unless (eq *profile* :off)
      (setf profile (assoc (rule-name rule) *profile*))
      (unless profile
	(setf profile (car (push (list (rule-name rule) 0 0) *profile*)))))
    (setf trace 
      (if (consp *trace*) 
	  (dolist (r *trace* nil) 
	    (when (or (eql r (rule-name rule))
		      (find r (rule-consequents rule) 
			    :key #'(lambda (x) (term-name (car x)))))
	      (return t)))
	*trace*))
    (dolist (ant (rule-antecedents rule))
      (unless (or (term-generator (car ant))
		  (term-vectors (car ant)))
	(setf (rule-trigger rule) (car ant))
	(push rule (term-consequents (rule-trigger rule)))
	(return-from update nil)))
    (when (= 1 (length (term-vectors (car assert))))
      (dolist (ant (rule-antecedents rule))
	(unless (or (term-generator (car ant))
		    (find rule (term-consequents (car ant)) :test #'eq))
	  (push rule (term-consequents (car ant))))))
    (dolist (ant (rule-antecedents rule))
      (when (eq (car ant) (car assert))
	(let ((m (match (cdr ant) (cdr assert))))
	  (unless (eq :fail m)
	    (if profile (incf (cadr profile)))
	    (update* (remove-1 ant (rule-antecedents rule)) m)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. Handling variables
;;;
;;; A statement like "(on Book Table)" would place the vector "(Book Table)" 
;;; with the TERM structure associated with the predicate "on".  

(defun INSERT-VEC (pred new-vec)
  "Insert a vector NEW-VEC for predicate PRED.  Alert consequent rules."
  (when (term-demon pred) (apply (term-demon pred) new-vec))
  (when (null (term-generator pred))
    (push new-vec (term-vectors pred)))
  (dolist (l (term-consequents pred)) (update l (cons pred new-vec))))

(defun GET-VECS (p pattern alst &aux (ret nil))
  "Get all vectors in P that match a pattern (returns alists)"
  (let* ((v (my-sublis alst pattern)))
    (if (term-generator p)
	(setf ret (mapcar #'(lambda (x) (append x alst))
			  (apply (term-generator p) v)))
      (dolist (a (term-vectors p) ret)
	(let ((b (match v a alst)))
	  (when (listp b) (push b ret)))))
    (when (and (term-generator p)
	       (if (consp *trace*) 
		   (dolist (r *trace* nil) 
		     (when (eq (term-name p) r) (return t)))
		 *trace*))
      (format *trace-stream* "~%Inspected ~a~%  Found ~a"
	      (cons (term-name p) v) 
	      (mapcar #'(lambda (r) (cons (term-name p) (my-sublis r v)))
		      ret)))
    ret))

(defun MATCH (pattern lst &optional alst)
  "Compute the most general unifier between PATTERN and LST.
   Only PATTERN contains variables."
  (labels 
      ((match* (p l)
	 (when (not (eql p l))
	   (cond
	    ((variable$ p)
	     (dolist (a alst (push (cons p l) alst))
	       (when (eq (car a) p)
		 (if (eql (cdr a) l) (return alst)
		   (throw 'done :fail)))))
	    ((or (atom p) (atom l)) (throw 'done :fail))
	    (t (match* (car p) (car l))
	       (match* (cdr p) (cdr l)))))))
   (catch 'done
     (match* pattern lst)
     alst)))

(defun MY-SUBLIS (alst tree)
  "A faster sublis function.  It only tries to substitute for variables."
  (labels
      ((MY-SUBLIS* (tr)
	 (cond ((consp tr) (mapcar #'my-sublis* tr))
	       ((variable$ tr)
		(dolist (a alst tr)
		  (when (eq tr (car a)) 
		    (return (cdr a)))))
	       (t tr))))
    (my-sublis* tree)))

(defun REMOVE-1 (elt lst &key (test #'eq))
  (cond ((null lst) nil)
        ((funcall test elt (car lst)) (cdr lst))
        (t (cons (car lst) (remove-1 elt (cdr lst) :test test)))))

(defun variable$ (thing)
  "Is THING a variable name?"
  (and (symbolp thing) (char= #\$ (elt (symbol-name thing) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 9. Print functions

(defun PRINT-PRED (pred &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "[~a]" (term-name pred)))

(defun DISPLAY-PRED (pred &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "[~a: ~a]" (term-name pred) 
	  (if (term-generator pred) 'GENERATOR (term-vectors pred))))
	  
(defun PRINT-RULE (rule &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "[rule<~a> ~a -> ~a]" (rule-name rule)
	  (rule-antecedents rule) 
	  (if (= 1 (length (rule-consequents rule)))
	      (car (rule-consequents rule))
	    (cons :and (rule-consequents rule)))))

(defun PRINT-NET (n &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "%NET(rules:~a~%    predicates:~a"
	  (rule-net-rules n)
	  (rule-net-predicates n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test2 ()
  "Test of whole system"
  (let ((n (defnet 'test2)))
    (defrule n 'a1 :effect '(above $y $x) :when '((on $x $y)))
    (defrule n 'a2 :effect '(above $x $y) :when '((above $x $z)(above $z $y)))
    (defdemon n (above x y)
      (format t "~S is above ~S~%" x y))
    (assertion n '(on table book))
    (assertion n '(on book pen))
    (assertion n '(on floor table))
    (format t "n = ~a~%" n)
  ;; The args of a macro are not evaled until whole macro is expanded
    (format t "*** Reseting Database ***~%")
    (clear-firings n)
    (format t "*** Reseted Database ***~%")
    (assertion n '(on floor table))))

