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

(in-package 'VCR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plan tree computations


(defun DO-TREE (function tree)
  (funcall function tree)
  (mapc #'(lambda (c) (do-tree function c)) (plan-tree-children tree)))

(defun SHOW-GRAPH-TREE (tree)
  (setf (graphic-shown tree) T)
  (mapc #'show-graph-tree (graphic-node-nodes tree)))

(defun HIDE-GRAPH-TREE (tree)
  (setf (graphic-shown tree) nil)
  (mapc #'hide-graph-tree (graphic-node-nodes tree)))


(defun ANCESTOR? (node1 node2)
  (when node1
    (or (eq node1 node2)
	(ancestor? (plan-tree-parent node1) node2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. Set tree display specs

(defun EXPAND-DISPLAY (node graphic table)
  (mapc #'hide-graph-tree (graphic-node-nodes graphic))
  (setf (graphic-node-nodes graphic)
    (mapcar #'(lambda (n) (get-translation table n))
	    (plan-tree-children node)))
  (show-graph-tree graphic))

(defun RETRACT1-DISPLAY (node graphic table)
  (declare (ignore table node))
  (mapc #'(lambda (g) (setf (graphic-shown g) nil)) (graphic-node-nodes graphic))
  (setf (graphic-node-nodes graphic)
    (mapcan #'(lambda (g) (copy-list (graphic-node-nodes g)))
	    (graphic-node-nodes graphic)))
  (show-graph-tree graphic))

(defun RETRACT-DISPLAY (node graphic table)
  (declare (ignore table node))
  (mapc #'hide-graph-tree (graphic-node-nodes graphic))
  (setf (graphic-node-nodes graphic) nil))

(defun FULL-DISPLAY (node graphic table)
  (setf (graphic-shown graphic) T)
  (setf (graphic-node-nodes graphic)
    (mapcan #'(lambda (n)
		(full-display n (get-translation table n) table))
	    (plan-tree-children node)))
  (list graphic))
  
(defun SOLN-DISPLAY (node graphic table)
  (let ( (last (do ((a node (car (plan-tree-children a))))
		   ((or (null (plan-tree-children a))
			(null (get-plan-flaws (plan-tree-entry a))))
		    a))) )
    (hide-graph-tree graphic)
    (unless (get-plan-flaws (plan-tree-entry last))
      (setf (graphic-node-nodes graphic) (list (get-translation table last))))
    (show-graph-tree graphic)))

(defun SOLN-PATH-DISPLAY (node graphic table)
  (cond ((and (null (plan-tree-children node))
	      (null (get-plan-flaws (plan-tree-entry node))))
	 (setf (graphic-shown graphic) T)
	 T)
	((soln-path-display (car (plan-tree-children node))
			    (get-translation table (car (plan-tree-children node)))
			    table)
	 (setf (graphic-shown graphic) T)
	 (mapc #'(lambda (e) (hide-graph-tree (get-translation table e)))
	       (cdr (plan-tree-children node)))
	 (setf (graphic-node-nodes graphic) 
	   (list (get-translation table (car (plan-tree-children node)))))
	 T)
	(T
	 (hide-graph-tree graphic)
	 nil)))

(defun STEP-DISPLAY (node graphic table)
  (cond ((get-plan-step? (plan-tree-entry node))
	 (setf (graphic-shown graphic) T)
	 (setf (graphic-node-nodes graphic)
		(mapcan #'(lambda (n)
			    (step-display n (get-translation table n) table))
			(plan-tree-children node)))
	 (list graphic))
	(T
	 (setf (graphic-shown graphic) nil)
	 (mapcan #'(lambda (n)
		     (step-display n (get-translation table n) table))
		 (plan-tree-children node)))))

(defun SEARCH-DISPLAY (node graphic table)
  (cond ((not (null (plan-tree-flaw node)))
	 (setf (graphic-shown graphic) T)
	 (setf (graphic-node-nodes graphic)
	   (mapcan #'(lambda (n)
		       (search-display n (get-translation table n) table))
		   (plan-tree-children node)))
	 (list graphic))
	(T
	 (setf (graphic-shown graphic) nil)
	 nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Compute the search tree from a tape
(defun COMPUTE-TREE (tape ranks firings &key (nodes nil))
  (let ((size 0) (max-rank 0) (min-rank 0) (tape-nodes nil))
    (dolist (entry tape)
      (let ( (n (find (car entry) nodes :key #'plan-tree-entry :test #'eq)) )
	(cond ((null n)
	       (push (make-instance `plan-tree
		       :entry    (car entry) 
		       :flaw     (cadr entry)
		       :children (caddr entry))
		     tape-nodes))
	      (T
	       (setf (plan-tree-flaw n) (cadr entry))
	       (setf (plan-tree-children n) (caddr entry))))
	(incf size)))
    (setq nodes (nconc nodes tape-nodes))
  
    (do* ((p nil (car n))
	  (n nodes (cdr n)))
	((null n) nil)
      (setf (plan-tree-next (car n)) (when (cdr n) (cadr n))
	    (plan-tree-prev (car n)) p))
  
    (do ((ns nodes (cdr ns)))
	((null ns) nil)
      (setf (plan-tree-children (car ns))
	(mapcar #'(lambda (child)
		    (let ((a (find child (cdr ns) 
				   :key #'plan-tree-entry :test #'eq)))
		      (unless a 
			(setf a (make-instance 'plan-tree
				  :entry child 
				  :flaw nil 
				  :children nil
				  :next (car ns)
				  :prev (car ns)))
			(incf size))
		      (setf (plan-tree-parent a) (car ns))
		      (setf (plan-tree-diff a) (get-plan-diff (plan-tree-entry a)))
		      a))
		(plan-tree-children (car ns)))))
    
    (dolist (n nodes)
      (let (
	    (rank (car (find (plan-tree-entry n) ranks :test #'eq :key #'cdr)))
	    (firings (assoc (plan-tree-entry n) firings))
	    )
	(when rank
	  (setf (plan-tree-rank n) rank)
	  (when (> rank max-rank) (setf max-rank rank))
	  (when (< rank min-rank) (setf min-rank rank)))
	(when firings
	  (setf (plan-tree-firings n) (cdr firings)))))
    (sort-tree (car nodes))
    (order-tree (car nodes))
    
    ;; number nodes.
    (do ((i 0)
	 (n (car nodes) (plan-tree-next n))
	 )
	((null n))
      (setf (plan-tree-number n) (incf i)))
    
    (values (car nodes) size max-rank min-rank)))


;;; Sort tree by rank when available
(defun SORT-TREE (node)
  (dolist (c (plan-tree-children node))
    (sort-tree c))
  (setf (plan-tree-children node)
    (mapcar #'cdr
	    (sort (mapcar #'(lambda (c) 
			      (cons (cdr (assoc (plan-tree-entry c) *ranks*))
				    c))
			  (plan-tree-children node))
		  #'< :key #'(lambda (x)
			       (if (car x) (car x) most-positive-fixnum))))))

;;; Order a tree so that the winning path is left justified
(defun ORDER-TREE (node)
  (cond ((get-plan-flaws (plan-tree-entry node))
         (dolist (c (plan-tree-children node))
           (when (order-tree c)
             (setf (plan-tree-children node)
               (cons c (remove c (plan-tree-children node))))
             (return t))))
        (t t)))



