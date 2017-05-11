" (c) 1992,1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "SC")

(export '(make-sc rule def-clause def-action isrch srch assertion sort-entries))

(defvar *verbose* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables and structures

(defstruct (SC (:constructor make-sc*)
	    (:print-function print-sc))
  name
  flaw-fn
  repair-fn
  (nodes nil)
  (flaws nil)
  (control-net nil))

(defun PRINT-SC (sc &optional (stream t) depth)
  (declare (ignore depth))
  (if *verbose*
      (format stream "#sc<name = ~a : nodes = ~a : flaws = ~a : net = ~a>"
	      (sc-name sc) (sc-nodes sc) (sc-flaws sc) (sc-control-net sc))
    (format stream "#sc<~a>" (sc-name sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Interface functions.

(defun make-sc (&key name flaw-fn repair-fn)
  (new-sc name flaw-fn repair-fn))

(defun rule (sc name &key when effect)
  (rule-net:defrule (sc::sc-control-net sc)
      name :when when :effect effect))

(defmacro def-clause (sc clause &body body)
  (macroexpand `(rule-net:defclause (sc::sc-control-net ,sc)
		    ,clause ,@body)))

(defmacro def-action (sc clause &body body)
  (macroexpand `(rule-net:defdemon (sc::sc-control-net ,sc)
		    ,clause ,@body)))

(defmacro assertion (sc clause)
  (macroexpand `(rule-net:assertion (sc::sc-control-net ,sc) ,clause)))

(defun isrch (initial-state sc limit)
  (catch 'SC-SEARCH
    (isc `(0 0 . ,initial-state)
	 most-positive-single-float sc (list limit))))

(defun srch (initial-state sc limit &aux (search-queue nil)(branches nil))
  (catch 'SC-SEARCH
    (do ((current-state initial-state (cdr (pop search-queue))))
	((or (null current-state)
	     (> 0 limit))
	 (values current-state
		 (if (null branches) 0
                   ;; avoids long arg list blowing up
                   (/ (reduce #'+ branches) (length branches)))
;;; 		   (/ (apply #'+ branches) (length branches))
		 0))
      (let ((children (sc-refinements current-state sc)))
#+:clim-2 (dolist (c children) (vcr:vcr-record-rank (car c) (cdr c)))
	(push (length children) branches)
	(setf limit (- limit (length children)))
	(setf search-queue 
	  (my-merge search-queue children
		    #'(lambda (x y) (< (car x) (car y)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initializing a new SC network

(defun NEW-SC (name flaw-fn repair-fn)
  (let ((sc (make-sc* :name name 
			:flaw-fn flaw-fn
			:repair-fn repair-fn
			:control-net (rule-net:defnet 'search-control))))
    (rule-net:defdemon (sc-control-net sc) (:reject type n)
      (when (eq type :flaw) (error "attempt to reject a flaw"))
      (let ((x (get-choice n type sc)))
	(if x (choice:reject x)
	  (error "attempt to (reject ~a)" n))))
    (rule-net:defdemon (sc-control-net sc) (:select type n)
      (when (eq type :flaw) (error "attempt to select a flaw"))
      (let ((x (get-choice n type sc)))
	(if x (choice:select x)
	  (error "attempt to (select ~a)" n))))
    (rule-net:defdemon (sc-control-net sc) (:rank type i n)
      (let ((x (get-choice n type sc)))
	(if (and x (numberp i)) (choice:rank i x)
	  (error "attempt to (rank ~a [~a])" i n))))
    (rule-net:defdemon (sc-control-net sc) (:prefer type a b)
      (let ((x (get-choice a type sc))
	    (y (get-choice b type sc)))
	(if (and x y) (choice:prefer x y)
	  (error "attempt to (prefer [~a] [~a] [~a])" type a b))))
    (rule-net:defdemon (sc-control-net sc) (:node a r)
      (declare (ignore r))
      (push (choice:new-choice a) (sc-nodes sc)))
    (rule-net:defdemon (sc-control-net sc) (:flaw a)
      (push (choice:new-choice a) (sc-flaws sc)))
    sc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Top level search controller.

(defun ISC (vnode bound sc limit) ;; (new-rank rank . node)
  (let ((children (sc-refinements (cddr vnode) sc)))
    (cond
     ((null children)
      (setf (car vnode) most-positive-single-float))
     (t (dolist (c children)
	  (push (car c) (cdr c))
	  (when (< (cadr vnode) (car vnode))
	    (setf (car c) (max (car vnode) (car c)))))
	(decf (car limit) (length children))
	(setf (car vnode) (caar children))
	(do () ((> (car vnode) bound))
	  (let* ((best children)
		 (rest (cdr children))
		 (new-bound (if rest (min bound (caar rest)) bound)))
	    (setf (cdr best) nil)
	    (isc (car best) new-bound sc limit)
	    (setf children 
	      (my-merge best rest
			#'(lambda (x y) (< (car x) (car y)))))
	    (setf (car vnode) (caar children))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Refining a plan

(defun SC-REFINEMENTS (node sc)
  (setf (sc-nodes sc) nil
	(sc-flaws sc) nil)
  (rule-net:clear-firings (sc-control-net sc))
  (rule-net:assertion (sc-control-net sc) `(:current :node ,node))
  ;; Compute the candidate flaws
  (let ((flaw (funcall (sc-flaw-fn sc) sc node)))
    (when (null flaw) (throw 'SC-SEARCH (values node 0 0)))
    (rule-net:assertion (sc-control-net sc) `(:current :flaw ,flaw))
    (funcall (sc-repair-fn sc) sc node flaw)
    (sort-entries :node sc)))
    
(defun SORT-ENTRIES (type sc)
  (let ((part (choice:partition (case type 
				  (:node (sc-nodes sc))
				  (:flaw (sc-flaws sc))
				  (otherwise (error "illegal type"))))))
    (dolist (p part)
      (dolist (n p)
	(rule-net:assertion 
	 (sc-control-net sc) 
	 `(:candidate ,type ,(choice::choice-ranking n) 
		      ,(choice::choice-entry n)))))
    (choice:p-sort part)))
    
(defun get-choice (x type sc)
  (case type 
    (:node (find x (sc-nodes sc) :key #'choice::choice-entry))
    (:flaw (find x (sc-flaws sc) :key #'choice::choice-entry))
    (t (error "illegal type [~a]" type))))

(defun match* (pat lst &aux (ret nil))
  (dolist (l lst ret)
    (let ((a (rule-net::match pat l)))
      (when (listp a) (push a ret)))))

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
