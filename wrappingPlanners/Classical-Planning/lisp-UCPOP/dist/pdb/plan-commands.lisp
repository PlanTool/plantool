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

(in-package 'vcr)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inspecting things

(define-plan-command com-inspect-object ((graphic 'graphic))
  #+:composer(when *composer-started* (composer:winspect (graphic-object graphic)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving steps around

(define-plan-command (com-move-graphic) ((graphic 'graphic))
   (with-slots (frame stream object x y) graphic
    (multiple-value-setq (x y)
      (clim:dragging-output (stream :finish-on-release T)
			    (clim:draw-line* stream
					     (- x 10) y
					     (+ x 10) y
					     :line-dashes T)
			    (clim:draw-line* stream
					     x (- y 10)
					     x (+ y 10)
					     :line-dashes T)))
    (layout-step object graphic stream
		 :x x
		 :y y
		 :plan (plan-tree-entry (plan-window-node frame))
		 :detail (get-option-value :plan-layout-detail (plan-window-options frame))
		 :table (get-translation (plan-window-graphics frame)
					 (plan-tree-entry (plan-window-node frame))))
    (show-window frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;;; Change when you click on a plan node

(defmethod select-plan (node (frame plan-window) graphic)
  (declare (ignore graphic))
  (let ((vcr (plan-window-vcr frame)))
    (push node (plan-window-history-b frame))
    (setf (plan-window-history-f frame) nil)
    (setf (plan-window-node frame) node)
    (move-window node frame vcr)))

;;; History uses a netscape-like mechanism
(define-plan-command com-plan-window-select-plan ((graphic 'graphic)) 
  (let* ((node (graphic-object graphic))
	 (frame (graphic-frame graphic))
	 (vcr (plan-window-vcr frame)))
    (if (eq (vcr-current vcr) node)
	(plan-from-node node nil frame)
      (progn (push node (plan-window-history-b frame))
	     (setf (plan-window-history-f frame) nil)
	     (setf (plan-window-node frame) node)
	     (move-window node frame vcr)))))

(defun plan-window-left (button &aux (frame clim:*application-frame*) node)
  (declare (ignore button))
  (if (< (length (plan-window-history-b frame)) 2)
      (window-msg "No previous history" :style :warning :window frame)
    (progn
      (setf node (pop (plan-window-history-b frame)))
      (push node (plan-window-history-f frame))
      (move-window (car (plan-window-history-b frame)) frame 
		   (plan-window-vcr frame)))))

(defun plan-window-right (button &aux (frame clim:*application-frame*) node)
  (declare (ignore button))
  (if (plan-window-history-f frame)
      (progn 
	(setf node (pop (plan-window-history-f frame)))
	(push node (plan-window-history-b frame))
	(move-window (car (plan-window-history-b frame)) frame 
		     (plan-window-vcr frame)))
    (window-msg "No next history" :style :warning :window frame)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Refinement

;;; Do plan refinement(dialog defined elsewhere)
(defmethod plan-from-node (node use-flaw (frame plan-window))
  (let* (
	 (vcr (plan-window-vcr frame))
	 (session (vcr-session vcr))
	 (tree (session-tree session))
	 (size (session-size session))
	 (result (session-result session))
	 (problem (session-name session))
	 (sc (session-sc session))
	 (search-f (session-search-f session))
	 (rank-f (session-rank-f session))
	 
	 (plan  (plan-tree-entry node))
	 (flaws (get-plan-flaws plan))

	 )
    (multiple-value-bind (flaw limit record)
	(plan-from-node-dialog plan use-flaw flaws sc search-f rank-f)
      (enable-vcr)
      (setf *show-status* T)
      (if is-xii
	  (progn (setf (session-nodes session) (list node))
		 (ucpop-bug plan flaw limit 
				  sc search-f rank-f 
				  nil
				  problem
				  record)
		 (setf (session-tree session) tree
		       (session-size session) (+ size (session-size session))
		       (session-result session) result))
	(progn
	  (ucpop-bug plan flaw limit sc search-f rank-f 
			   (get-problem-domain problem) nil nil)
	  (multiple-value-bind (root size)
	      (compute-tree *tape* *ranks* *firings* :nodes (list node))
	    (declare (ignore root))
	    (setf (session-size session) (+ (session-size session) size)))))
      (setf *show-status* nil)
      (reset-vcr)
      
      ;; update windows
      (dolist (vcr-frame (shell-debug-windows *shell*))
	(when (eq (vcr-session vcr-frame) session)
	  (dolist (plan-window (vcr-windows vcr-frame))
	    (when (ancestor? (plan-tree-parent (plan-window-node plan-window))
			     node)
  	      (move-window node plan-window vcr-frame))
;;; 	    (when (eq (plan-window-node plan-window) node))
	    (initialize-window plan-window)
	    (show-window plan-window))
	  (initialize-window vcr-frame)
	  (when *vcr-update-after-replan*
	    (clim:raise-frame vcr-frame)
	    (show-window vcr-frame))
	  (set-current vcr-frame node 
		       (get-translation (vcr-graphics (plan-window-vcr frame)) 
					node))
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shortcut for selecting flaws
(define-plan-command com-select-flaw ((graphic 'graphic)) 
  (let* (
	(node (plan-window-node (graphic-frame graphic)))
	 (flaw (graphic-object graphic))
	 (frame (graphic-frame graphic))
	 (vcr (plan-window-vcr frame))
	 
	)
    (if (eq (vcr-current vcr) node)
	(plan-from-node node flaw frame)
      (select-plan node frame graphic))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plan window Menu

(defun install-plan-menus ()
  ;; the plan menu
  (with-command-table ('plan-plan-tab)
    (inst-menu "Refine" :command '(com-plan-plan :refine)
	       :documentation "Refine this plan")
    (inst-menu "Inspect" :command '(com-plan-plan :inspect)
	       :documentation "Inspect this plan")
    (inst-menu "Print" :command '(com-plan-plan :print)
	       :documentation "Print this plan"))
  (with-command-table ('plan-options-tab)
    (inst-menu "Plan options..." :command '(com-options :plan)
	       :documentation "Options for Plan window"))
  )

(define-plan-command com-plan-plan ((choice t))
  (let ((frame clim:*application-frame*))
    (when choice
      (case choice
	(:refine
	 (plan-from-node (plan-window-node frame) nil frame))
	(:inspect
	 #+:composer(composer:winspect (plan-tree-entry (plan-window-node
							 frame)))
	 )
	(:print
	 (let ( (filename (file-dialog)) )
	   (when filename
	     (let ( (stream (open filename
				  :direction :output
				  :if-exists :overwrite
				  :if-does-not-exist :create)) )
	       (cond ((not (null stream))
		      (print-window frame stream)
		      (close stream))
		     (T
		      nil))))))))))

(defmethod exit-frame ((frame plan-window))
  (delete-window frame *shell*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plan Window Translators

(clim:define-presentation-to-command-translator select-a-plan
    (graphic-node com-plan-window-select-plan plan :gesture :select
	       :documentation "Select Plan"
	       :pointer-documentation "Select plan/Replan")
  (object)
  `(,object))

(clim:define-presentation-to-command-translator select-a-flaw
    (graphic-flaw com-select-flaw plan :gesture :select 
		:documentation "Replan this flaw"
		:pointer-documentation  "Replan this flaw")
  (object)
  `(,object))

(clim:define-presentation-to-command-translator inspect-a-graphic
    (graphic com-inspect-object plan
	     :documentation "Inspect Object" :gesture :describe
	     :pointer-documentation "Inspect Object")
  (object)
  `(,object))

(clim:define-presentation-to-command-translator move-a-graphic
    (graphic-step com-move-graphic plan 
	     :documentation "Move" :gesture :move
	     :pointer-documentation "Move")
  (object)
  `(,object))
 
