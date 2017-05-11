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

(in-package "VCR")

(clim:define-presentation-type graphic ())
(clim:define-presentation-type graphic-node () :inherit-from 'graphic)
(clim:define-presentation-type plan-window ())

(defconstant display-options 
    '((:full . "All nodes")
      (:steps . "Steps only")
      (:search . "Seached only")
      (:soln . "Solution only")
      (:soln-path . "Solution-path only"))
      "A map for display option keywords and captions")

(define-option :vcr-present-tree-scale
    1/8 '(1/16 1/8 1/4 1/2 1))

(define-option :vcr-present-branch-label
    :all '((:multiple "Show branching conditions on multiple branches only")
	   (:all "Show all branching conditions in tree")
	   (:none "Do not display branching conditions")))

(define-option :vcr-present-node-color
    :order '((:rank "Color tree by rank order")
	     (:order "Color tree by search order")))

(define-option-set :vcr-window
    '(:vcr-present-tree-scale :vcr-present-branch-label :vcr-present-node-color))

(defmethod initialize-window ((frame vcr-window))
  (let (
	(graph    (clim:get-frame-pane frame 'graph))
	(graphics (vcr-graphics frame))
	(tree     (session-tree (vcr-session frame)))
	)
    (do-tree #'(lambda (n)
		 (when (null (get-translation graphics n))
		   (translate-plan-tree graphics n :frame frame 
					:stream graph)))
      tree)
    (let ( (graphic (get-translation graphics tree)) )
      (full-display tree graphic graphics)
      (layout-window frame)
    (values))))

(defmethod initialize-node ((vcr vcr-window) node)
  (let (
	(graph    (clim:get-frame-pane vcr 'graph))
	(graphics (vcr-graphics vcr))
	)
    (do-tree #'(lambda (n)
		 (unless (get-translation graphics n)
		   (translate-plan-tree graphics n :frame vcr :stream graph)))
      node)
    (let ( (graphic (get-translation graphics node)) )
      (full-display node graphic graphics)
      (layout-window vcr))
    (values)))

(defmethod layout-window ((vcr vcr-window))
  (let (
	(graph    (clim:get-frame-pane vcr 'graph))
	(graphics (vcr-graphics vcr))
	(tree     (session-tree (vcr-session vcr)))
	(scale    (get-option-value :vcr-present-tree-scale (vcr-options vcr)))
	)
    (let ( (graphic (get-translation graphics tree)) )
      (clim:with-scaling (graph scale)
	(layout-tree tree graphic graph :x 0 :y 0 :scale (/ 1 scale))))
    (values)))

(defmethod show-window ((frame vcr-window))
  (let (
	(graph    (clim:get-frame-pane frame 'graph))
	(graphics (vcr-graphics frame))
	(current  (vcr-current frame))
	(session  (vcr-session frame))
	(windows  (vcr-windows frame))
	(scale    (get-option-value :vcr-present-tree-scale (vcr-options
							     frame))) 
	(branch   (get-option-value :vcr-present-branch-label (vcr-options
							       frame))) 
	(coloring (get-option-value :vcr-present-node-color (vcr-options
							     frame))) 
	)
    (clim:window-clear graph)
    (setf (clim:gadget-value (clim:find-pane-named frame 'tree-desc))
      (cdr (assoc (vcr-tree-desc frame) display-options)))
    (when (and current (not (graphic-shown (get-translation graphics
							    current)))) 
      (setf (vcr-current frame) nil))
    (setf *max-rank* (session-max-rank session)
	  *min-rank* (session-min-rank session))
    (clim:with-scaling (graph scale)
      (draw-tree frame graph (get-translation graphics (session-tree session))
		 :current (vcr-current frame)
		 :session session
		 :branch branch
		 :coloring (if (eql coloring (vcr-color-mode frame))
			       :no-change
			      (setf (vcr-color-mode frame) coloring)))
      (draw-window-markers frame graph windows)
      )
    (values)))

(defmethod print-window ((vcr vcr-window) file-stream)
  (let (
	(graphics (vcr-graphics vcr))
	(session  (vcr-session vcr))

	(scale    (get-option-value :vcr-present-tree-scale (vcr-options vcr)))
	(branch   (get-option-value :vcr-present-branch-label (vcr-options vcr)))
	(coloring (get-option-value :vcr-present-node-color (vcr-options vcr)))
	)

    (clim:with-output-to-postscript-stream (stream file-stream)
      (clim:with-scaling
	  (stream scale)
	(draw-tree vcr stream (get-translation graphics (session-tree session))
		   :session session
		   :branch branch
		   :coloring coloring)))
    (values)))


(defun draw-window-markers (frame stream windows)
  (dolist (w windows)
    (draw-window-marker frame stream w)))

(defun draw-window-marker (frame stream w)
  (ignore-errors
   (let* (
	  (node (plan-window-node w))
	  (graphic (get-translation (vcr-graphics frame) node))
	  (label (format nil "~A" (plan-window-id w)))
	  (record (clim:with-new-output-record
		      (stream)
		    (clim:with-translation (stream (graphic-x graphic) (graphic-y graphic))
		     (clim:with-output-as-presentation
			 (stream w 'plan-window :single-box T)
		       (clim:draw-text* stream
					label
					120 0
					:align-y :top
					:align-x :left
					:text-size :large)))))
	  )
     (setf (plan-window-record w) record))))

(defun erase-window-marker (frame stream w)
  (declare (ignore frame))
  (ignore-errors
   (clim:erase-output-record (plan-window-record w) stream)))
  

(defun draw-tree (frame stream graphic &key session current (branch :all) (coloring :order))
;;; This seems to conflict with the standard stream for output.  Disabling it.
    (let (
	  (nodes (if (not (null session)) (session-size session) 0))
	  (node-count 0)
	  (node-report 15)
	  )
      (labels 
	  (
	   (do-drawing (frame stream graphic nv)
	   
	     (let ( (node (graphic-object graphic)) )
	     
	       (draw-node frame stream node graphic :current current :coloring
			  coloring :node-count nodes) 
	     
	       (when session
		 (incf node-count)
		 (decf node-report)
		 (when nv
		   (when (or (= node-count nodes) (zerop node-report))
		     (clim:note-progress node-count nodes nv)
		     (setf node-report 15))))

	       (let ((children (graphic-node-nodes graphic)))
		 (when children
		   (let ((xstart (+ (graphic-x graphic) 55))
			 (ystart (+ (graphic-y graphic) 240))
			 (xend   (+ (graphic-x (car (last children))) 55)))
		     (clim:draw-line* stream
				      xstart (+ (graphic-y graphic) 160)
				      xstart ystart)
		     (clim:draw-line* stream
				      xstart ystart
				      xend ystart)
		     (case branch
		       ((:all :multiple)
			(when (or (eq branch :all) (< 1 (length children)))
			  (let ((flaw-label
				 (format nil "~A" (condition-label (get-bound-clause
								    (plan-tree-flaw node)
								    (plan-tree-entry node))))))
			    (clim:draw-text* stream
					     flaw-label
					     (floor (+ xend xstart) 2)
					     ystart
					     :align-x :center
					     :align-y :baseline
					     :ink clim:+black+)))))
		     (dolist (c children) 
		       (clim:draw-line* stream
					(+ (graphic-x c) 55) (- (graphic-y c)
								20) 
					(+ (graphic-x c) 55) (graphic-y c))
		       (do-drawing frame stream c nv))))))
 	     )
	   )
  (clim:noting-progress ((clim:get-frame-pane frame 'documentation) "Drawing tree..." nv)
	(do-drawing frame stream graphic nv)
)
	)))
   


(defun DRAW-NODE (frame stream node graphic &key current (coloring :order)
						 (node-count nil) (label T)) 
  (declare (ignore frame))
  (labels
      (
       (do-drawing ()
	 (let ((r (case coloring
		    (:no-change (plan-tree-color node))
		    (:rank (when (plan-tree-rank node)
			     (truncate (* 29 (/ (- (plan-tree-rank node) 
						   *min-rank*)
						(- *max-rank* *min-rank*))))))
		    
		    (:order (if (and (plan-tree-flaw node) node-count)
				(truncate (* 29 (/ (plan-tree-number node) 
						   node-count)))))
		    (otherwise nil))))
	   (setf (plan-tree-color node) r)
	   (clim:draw-rectangle* stream 0 0 110 110
				 :ink (cond 
				       ((eq node current) *current-color*)
				       (r (nth r *colors*))
				       (T clim:+white+)))
	   (unless r (clim:draw-polygon* 
		     stream '(0 0 110 0 110 110 0 110)
		     :closed t :filled nil :line-thickness 1)
		   ))
	 (unless (plan-tree-flaw node)
	  (clim:draw-polygon* stream '(0 0 110 0 110 110 0 110)
			      :closed t :filled nil :line-thickness 1))
	 (when label
	   (clim:draw-text stream (get-iconic-reason-label (plan-tree-entry node))
			   (clim:make-point 55 115) 
			   :align-x :center :align-y :top
			   :text-style *tree-font*)))
	  )
     (clim:with-translation (stream (graphic-x graphic) (graphic-y graphic))
       (clim:with-output-as-presentation
 	   (stream graphic 'graphic-node :single-box T)
	 (do-drawing)))))




