" (c) 1992,1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "CHOICE")

(export '(reject select rank prefer partition p-clear p-sort p-best 
	  new-choice choice-entry *select-reject*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following code is used to implement search controlers
;;; 
;;; Calling conventions:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Structures and global variables

;;;;;;;;;;;;;;;;;;;;;;;;
;;; A node in a preference graph
(defstruct (choice (:print-function print-choice))
  entry					; this is the proposition label
  (status nil)				; Has choice been rejected or selected?
  (ranking 0)				; Ranking of the entry
  (edges nil)				; those choices with <= priority
  (dfs-number 0)			; used to find connected components
  (high nil)				; used to find connected components
  (component nil))			; Connected component

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A strongly connected component in a preference graph
(defstruct component
  (indegree 0))				; indegree to strongly connected subgr.

(defvar *select-reject* :rejected)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Handy interface for using controler

(defun REJECT (n)
  "Reject choice N"
  (unless (eq *select-reject* (choice-status n))
    (setf (choice-status n) :rejected)))

(defun SELECT (n)
  "Select choice N"
  (unless (eq *select-reject* (choice-status n))
    (setf (choice-status n) :selected)))

(defun RANK (i n)
  "Give choice N a ranking of I"
  (incf (choice-ranking n) (eval i)))

(defun PREFER (n1 n2)
  (unless (= (choice-ranking n1) (choice-ranking n2))
    (error "Attempt to establish preference when rankings differ"))
  (push n2 (choice-edges n1)))

(defun PARTITION (lst)
  "Parition choices in LST according to their ranks"
  (setf lst (choice-delete  (sort lst #'< :key #'choice-ranking)))
  (when lst
    (let ((part `((,(car lst)))))
      (dolist (l (cdr lst) part)
	(if (= (choice-ranking (caar part)) (choice-ranking l))
	    (push l (car part))
	  (push (list l) part))))))

(defun P-CLEAR (part)
  "Clear a partition in preparation for preferences"
  (dolist (a part)
    (setf (choice-edges a) nil
	  (choice-dfs-number a) 0
	  (choice-high a) nil
	  (choice-component a) nil)))
  
(defun P-SORT (ps &aux (ret nil))
  "Return a list of entries in sorted order (with rankings)"
  (dolist (p ps ret)
    (setf ret (nconc ret (mapcar #'(lambda (c)
				     (cons (choice-ranking c)
					   (choice-entry c)))
				 (sort-prefers p))))))

(defun P-BEST (part)
  "get the best entry (according to preferences) in part"
  (choice-entry (prefered-entry part)))

(defun NEW-CHOICE (a)
  "Create a selection choice"
  (get-choice a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. deleting choices

(defun CHOICE-DELETE (lst &aux (ret nil))
  "Delete rejected and non-selected choices."
  (unless (null (find :selected lst :key #'choice-status))
    (dolist (l lst)
      (when (not (eq :selected (choice-status l)))
	(setf (choice-status l) :rejected))))
  (dolist (l lst ret)
    (if (eq :rejected (choice-status l))
	(save-choice l)
      (push l ret))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Handling preference graphs

(defun PREFERED-ENTRY (enodes)
  "Return a single prefered enode"
  (save-components enodes)
  (dolist (v enodes)
    (setf (choice-dfs-number v) 0
          (choice-high v) nil
          (choice-component v) nil))
  (scc enodes)
  (dolist (v enodes)
    (dolist (w (choice-edges v))
      (when (not (eq (choice-component v) (choice-component w)))
        (incf (component-indegree (choice-component w))))))
  (find-if #'(lambda (e) (zerop (component-indegree (choice-component e))))
	   enodes :from-end t))

(defun SORT-PREFERS (enodes &aux (ret nil))
  "Return enodes in their preference order"
  (save-components enodes)
  (dolist (v enodes)
    (setf (choice-dfs-number v) 0
	  (choice-high v) nil
	  (choice-component v) nil))
  (scc enodes)
  (dolist (v enodes)
    (dolist (w (choice-edges v))
      (when (not (eq (choice-component v) (choice-component w)))
	(incf (component-indegree (choice-component w))))))
  (let ((temp nil))
    (do () ((null enodes))
      (dolist (v (reverse enodes))
	(cond ((zerop (component-indegree (choice-component v)))
	       (dolist (w (choice-edges v))
		 (when (not (eq (choice-component v) (choice-component w)))
		   (decf (component-indegree (choice-component w)))))
	       (push v ret))
	      (t (push v temp))))
      (setf enodes temp)
      (setf temp nil)))
  (reverse ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. Computing strongly connected components.
;;;
;;; This algorithm computes the strongly connected components of a graph.
;;; VS is a list of nodes in the graph.  These nodes are assumed to be atoms,
;;; and each node N's EDGES property list entry contains a list.  Each entry
;;; M of this list corresponds to a directed edge (N -> M) of the graph.
;;; The result is a side effect.  All of the nodes get COMPONENT numbers.
;;; The function returns a set of component roots.
;;;
;;; See pages 226-231 of "Introduction to Algorithms: A Creative Approach"
;;;     by Udi Manber for a discussion of this algorithm.

(defun SCC (vs &aux stack dfs-n)
  (labels
      ((SCC* (v)
         (setf (choice-dfs-number v) dfs-n)
         (decf dfs-n)
         (push v stack)
         (setf (choice-high v) (choice-dfs-number v))
         (dolist (w (choice-edges v))
           (cond ((= 0 (choice-dfs-number w))
                  (scc* w)
                  (setf (choice-high v) (max (choice-high v) (choice-high w))))
                 ((and (null (choice-component w))
                       (> (choice-dfs-number w) (choice-dfs-number v)))
                  ;; (v, w) is a cross or back edge
                  (setf (choice-high v)
                        (max (choice-high v) (choice-dfs-number w))))))
         (when (= (choice-high v) (choice-dfs-number v)); v is a component root
           (setf (choice-component v) (get-component))
           (do ((x (pop stack) (pop stack)))
               ((eq x v))
             (setf (choice-component x) (choice-component v))))
         ))
    (setf stack nil
          dfs-n (length vs))
    (dolist (v vs)
      (when (= 0 (choice-dfs-number v))
        (scc* v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. Space saving functions

;;;;;;;;;;;;;;;;;;;;;
;;; Manage space used for creating choice structures
(defvar *free-choices* nil)

(defun SAVE-CHOICE (c)
  "Save choice(s) C in the *free-choices* list"
  (if (choice-p c) (push c *free-choices*)
    (setf *free-choices* (nconc c *free-choices*))))

(defun GET-CHOICE (entry)
  "Get choice entry from the *free-choices* list"
  (let ((n (if *free-choices* (pop *free-choices*)
	     (make-choice :entry entry))))
    (setf (choice-entry n) entry
	  (choice-ranking n) 0
	  (choice-status n) nil
	  (choice-edges n) nil
	  (choice-dfs-number n) 0
	  (choice-high n) nil
	  (choice-component n) nil)
    n))

;;;;;;;;;;;;;;;;;;;;;
;;; Manage space used for creating component structures
(defvar *free-components* nil)

(defun SAVE-COMPONENTS (lst)
  "Save component entries in the *free-components* list"
  (dolist (l lst)
    (when (and (choice-component l) 
	       (numberp (component-indegree (choice-component l))))
      (setf (component-indegree (choice-component l)) *free-components*)
      (setf *free-components* (choice-component l)))))

(defun GET-COMPONENT ()
  "Get component entry from the *free-components* list"
  (cond (*free-components*
	 (let ((l *free-components*))
	   (setf *free-components* (component-indegree *free-components*))
	   (setf (component-indegree l) 0)
	   l))
	(t (make-component))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7. Print functions

(defun PRINT-CHOICE (n &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "[choice<~a>:~a]" (choice-entry n) (choice-ranking n)))

