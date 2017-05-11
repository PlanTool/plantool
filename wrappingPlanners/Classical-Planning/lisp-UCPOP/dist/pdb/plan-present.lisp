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

(clim:define-gesture-name :move :pointer-button (:left :shift))

(clim:define-presentation-type graphic-flaw () :inherit-from 'graphic)
(clim:define-presentation-type graphic-step () :inherit-from 'graphic)
(clim:define-presentation-type graphic-link () :inherit-from 'graphic)
(clim:define-presentation-type graphic-effect () :inherit-from 'graphic)
(clim:define-presentation-type graphic-cond () :inherit-from 'graphic)

(define-option :plan-layout-detail :verbose '((:verbose "Maximal detail")
					      (:terse "Minimal detail")))
(define-option :plan-present-mode  :text    '((:text "Textual representation")
					      (:graphics "Graphical representation")))

(define-option-set :plan-window 
    '(:plan-layout-detail :plan-present-mode))


(defmethod initialize-window ((frame plan-window))
  (let (
	(graph (clim:get-frame-pane frame 'graph))
	(tree  (clim:get-frame-pane frame 'tree))
	
	(node (plan-window-node frame))
	(plan (plan-tree-entry (plan-window-node frame)))
	(parent (plan-tree-parent (plan-window-node frame)))
	(graphics (plan-window-graphics frame))
	)
    
    ;;; set up these translations for navigational purposes
    (unless (get-translation graphics node)
      (translate-plan-tree graphics node :frame frame :stream tree))
    (dolist (c (plan-tree-children node))
      (unless (get-translation graphics c)
	(translate-plan-tree graphics c :frame frame :stream tree)))
    (when  parent
      (unless (get-translation graphics parent)
	(translate-plan-tree graphics parent :frame frame :stream tree))
      (dolist (c (plan-tree-children parent))
	(unless (get-translation graphics c)
	  (translate-plan-tree graphics c :frame frame :stream tree))))
    
    ;; set up plan translation of current plan
    (when (null (get-translation graphics plan))
      (translate-plan graphics plan :frame frame :stream graph))
    (layout-window frame)
    (values)))

(defmethod layout-window ((frame plan-window))
  (let (
	(graph (clim:get-frame-pane frame 'graph))
	(tree  (clim:get-frame-pane frame 'tree))
	
	(node (plan-window-node frame))
	(plan (plan-tree-entry (plan-window-node frame)))
	(parent (plan-tree-parent (plan-window-node frame)))
	(graphics (plan-window-graphics frame))
	
	(detail (get-option-value :plan-layout-detail (plan-window-options frame)))
	(style (get-option-value :plan-layout-style (plan-window-options frame)))
	)
    
    ;; layout parent tree
    (when parent
      (expand-display parent (get-translation graphics parent) graphics)
      (dolist (c (plan-tree-children parent))
	(retract-display c (get-translation graphics c) graphics)))
    (expand-display node (get-translation graphics node) graphics)
    (dolist (c (plan-tree-children node))
      (retract-display c (get-translation graphics c) graphics))
    
    (clim:with-scaling (tree 1/8)
      (layout-tree (or parent node)
		   (get-translation graphics (or parent node))
		   tree
		   :x 0 :y 0 :scale 8))
    
    ;; layout plan
    (layout-plan plan (get-translation graphics plan) graph
		 :detail detail :style style)
    (values)))

(defmethod show-window ((frame plan-window))
  (let (
	(graph (clim:get-frame-pane frame 'graph))
	(tree (clim:get-frame-pane frame 'tree))
	
	(node (plan-window-node frame))
	(plan (plan-tree-entry (plan-window-node frame)))
	(parent (plan-tree-parent (plan-window-node frame)))
	
	(graphics (plan-window-graphics frame))
	(detail (get-option-value :plan-layout-detail (plan-window-options frame)))
	(mode (get-option-value :plan-present-mode (plan-window-options frame)))
	)
    
    (clim:window-clear graph)
    (clim:window-clear tree)
    
    (let ( (graphic (get-translation graphics (or parent node))) )
      (clim:draw-line* tree 0 0 0 90 :ink clim:+white+)
      ;; This is added so that we can traverse the window along with the vcr.
      (set-current (plan-window-vcr frame) 
		   node
		   (get-translation (vcr-graphics (plan-window-vcr frame)) 
				    node))
      (clim:with-scaling (tree 1/8)
	(draw-tree frame tree graphic :current node :coloring :no-change)))

    (let ( (plan-table (get-translation graphics plan)) )
      (clim:with-end-of-line-action (graph :wrap)
	(format graph "Reason:  ~A~%" (get-full-reason-label (plan-tree-entry node)))
	(format graph "Rank:    ~A~%" (or (plan-tree-rank node) 
					  "N/A"))
	(format graph "Firings: ~A~%" (or (plan-tree-firings node) 
					  "N/A"))
	(format graph "Flaw:    ~A~%" (or (plan-tree-flaw node) 
					  "None Selected"))
	(format graph "Difference:   ~A~%" (plan-tree-diff node)))
      (case mode
	(:graphics
	 (multiple-value-bind (w h)
	     (clim:stream-cursor-position graph)
	   (declare (ignore w))
	   (clim:with-translation (graph 0 h)
	     (dolist (step (get-plan-steps plan))
	       (draw-step frame graph node step (get-translation plan-table step)
			  :detail detail :graphics plan-table))
	     (dolist (link (get-plan-links plan))
	       (draw-link frame graph node link (get-translation plan-table link)
			  :detail detail :graphics plan-table)))))
	(:text
	 (print-plan frame graph node plan :detail detail :graphics plan-table))))
    (values)))


(defmethod outline-tree ((frame plan-window) stream)
  (declare (ignore stream))
  (initialize-window frame)
  (show-window frame))

(defmethod print-window ((frame plan-window) file-stream)
  (let (
	(node (plan-window-node frame))
	(plan (plan-tree-entry (plan-window-node frame)))
	
	(graphics (plan-window-graphics frame))
	(detail (get-option-value :plan-layout-detail (plan-window-options frame)))
	(mode (get-option-value :plan-present-mode (plan-window-options frame)))
	)
    
    (clim:with-output-to-postscript-stream (graph file-stream)
      (let ( (plan-table (get-translation graphics plan)) )
	(case mode
	  (:graphics
	   (dolist (step (get-plan-steps plan))
	     (draw-step frame graph node step (get-translation plan-table step)
			:detail detail :graphics plan-table))
	   (dolist (link (get-plan-links plan))
	     (draw-link frame graph node link (get-translation plan-table link)
			:detail detail :graphics plan-table)))
	  (:text
	   (print-plan frame graph node plan :detail detail :graphics plan-table)))))
    (values)))



(defun print-plan (frame stream node plan &key (detail :verbose) graphics)
  (let (
	(steps (get-plan-steps plan))
	(ordering (get-plan-ordering plan))
	)
    (let (
	  (init (get-plan-step plan (if is-xii 0 :init)))
	  (goal (get-plan-step plan :goal))
	  )

  
      ;; HOOK TO EXTERNALS (define in vcr*-externals.lisp)
      (display-planner-specific-objects frame stream node plan detail 
                                        graphics)
      ;; END HOOK
    
      (when  init
	(print-step frame stream node init (get-translation graphics init)
		    :detail detail :graphics graphics))
      (dotimes (i (get-plan-num-steps plan))
	(let ( (step (find (nth i ordering) steps :key #'get-step-id)) )
	  (when step
	    (print-step frame stream node step (get-translation graphics step)
			:detail detail :graphics graphics))))
      (when goal
	(print-step frame stream node goal (get-translation graphics goal)
		    :detail detail :graphics graphics)))))

(defmethod print-step (frame stream node step graphic 
		       &key (detail :verbose) graphics)
  (declare (ignore frame detail))
  (let (
	(execute? (and is-xii 
		       (get-step-execute? step (plan-tree-entry node))))
	(plan (plan-tree-entry node))
	(self-id (get-step-id step))
	)
    (clim:with-output-as-presentation
	(stream graphic (if execute? 'graphic-flaw 'graphic-step) :single-box T)
      (clim:with-drawing-options
	  (stream :ink *step-color*)
	(if (eq self-id :init)
	    (let ( (effects (get-step-effects step)) )
		 (format stream "~%Initial Step:")
		 (dolist (effect effects)
		   (clim:with-output-as-presentation
		       (stream (get-translation graphics effect)
			       'graphic-effect :single-box T)
		     (dolist (add (get-effect-add effect))
		       (clim:with-output-as-presentation
			   (stream (get-translation graphics add)
				   'graphic-cond :single-box T)
			 (format stream "~%   ~20a" 
				 (get-bound-clause add plan)))))))
	  (progn 
	    (clim:with-drawing-options
		(stream :ink (if execute? *flaw-color* *step-color*))
	      (format stream "~%Step ~A: ~A" self-id 
		      (get-bound-clause step plan)))
	    (dolist (link (get-plan-links plan))
		 (when (eq (get-link-to link) self-id)
		   (let (
			 (threats (get-link-threats link plan))
			 (cond (get-link-openc link))
			 )
		     (clim:with-output-as-presentation
			 (stream (get-translation graphics cond)
				 'graphic-cond :single-box T)
		       (if (or (not is-xii)
			       (get-step-executed? (get-link-from link) plan))
			   (format stream "~%   ~2a => ~20a"
				   (get-link-from link)
				   (get-bound-clause
				    cond plan))
			 (format stream "~%   ~2a -> ~20a"
				 (get-link-from link)
				 (get-bound-clause
				  cond plan)))
		       (dolist (threat threats)
			 (clim:with-output-as-presentation
			     (stream (get-translation graphics threat)
				     'graphic-flaw :single-box T)
			   (clim:with-drawing-options
			       (stream :ink *flaw-color*)
			     (format stream "<~a>" (get-threat-id threat)))))))))
	       (dolist (openc (get-step-openc step plan))
		 (clim:with-output-as-presentation
		     (stream (get-translation graphics openc)
			     'graphic-flaw :single-box T)
		   (clim:with-drawing-options
		       (stream :ink *flaw-color*)
		     (format stream "~%   ?? -> ~20a"
			     (get-bound-clause
			      openc plan)))))))
	(terpri stream)))))



(defmethod draw-step (frame stream node step graphic &key (detail :verbose) graphics)
  (let (
	(execute? (and is-xii (get-step-execute? step (plan-tree-entry node))))
	(x (graphic-x graphic))
	(y (graphic-y graphic))
	(label (case (get-step-id step)
		 (:init (format nil "Initial"))
		 (:goal (format nil "Goal"))
		 (T (format nil "~A" (car (get-bound-clause 
					   step 
					   (plan-tree-entry node)))))))
	)
    (ignore-errors 
     (clim:with-output-as-presentation
	 (stream graphic (if execute? 'graphic-flaw 'graphic-step) :single-box T)
       (clim:with-drawing-options
	   (stream :text-style (if execute? *flaw-font* *step-font*)
		   :ink (if execute? *flaw-color* *step-color*))
	 (case detail
	   (:verbose
	    (clim::draw-line* stream
			      (- x 40) (- y 20)
			      (+ x 40) (- y 20))
	    (clim::draw-line* stream
			    x (- y 20)
			    x (+ y 20))
	    (clim::draw-text* stream
			      label
			      x (- y 20)
			      :align-x :center
			      :align-y :bottom))
	   (T
	    (clim:draw-rectangle* stream
				  (- x 20) (- y 20)
				  (+ x 20) (+ y 20)
				  :filled nil)
	    (clim:draw-text* stream
			     label
			     x (+ y 21)
			     :align-x :center
			     :align-y :top)))
	 (dolist (effect (get-step-effects step))
	   (draw-effect frame stream node effect (get-translation graphics effect)
			:detail detail :graphics graphics))
	 (dolist (cond (get-step-cond step (plan-tree-entry node)))
	   (draw-pre-cond frame stream node cond (get-translation graphics cond)
			  :detail detail :graphics graphics))
	 (dolist (openc (get-step-openc step (plan-tree-entry node)))
	   (draw-pre-cond frame stream node openc (get-translation graphics openc)
			  :detail detail :graphics graphics :flaw T)))))))

(defmethod draw-effect (frame stream node effect graphic &key (detail :verbose) graphics)
   (case detail
     (:verbose
      (clim:with-output-as-presentation
	  (stream graphic 'graphic-effect :single-box T)
	(let ( (adds (get-effect-add effect)) )
	  (dolist (add adds)
	    (draw-post-cond frame stream node add (get-translation graphics add)
			    :detail detail :graphics graphics)))))
      (T nil)))

(defmethod draw-post-cond (frame stream node cond graphic 
			   &key (detail :verbose) graphics)
  (declare (ignore graphics frame))
  (case detail
    (:verbose
     (clim:with-output-as-presentation
	 (stream graphic 'graphic-cond :single-box T)
       (let (
	     (x (graphic-x graphic))
	     (y (graphic-y graphic))
	     (label (condition-label (get-bound-clause 
				      cond (plan-tree-entry node))))
	     )
	 (clim:draw-text* stream
			  label
			  x y
			  :ink *step-color*
			  :align-y :top
			  :align-x :right
			  :text-style *step-font*))))
     (T nil)))

(defmethod draw-pre-cond (frame stream node cond graphic 
			  &key (detail :verbose) (flaw nil) graphics)
  (declare (ignore graphics frame))
  (case detail
    (:verbose
     (clim:with-output-as-presentation
	 (stream graphic
		 (if flaw 'graphic-flaw 'graphic-cond) :single-box T)
       (let (
	     (x (graphic-x graphic))
	     (y (graphic-y graphic))
	     (label (condition-label (get-bound-clause 
				      cond (plan-tree-entry node))))
	     )
	 (clim:draw-text* stream
			    label
			    x y
			    :ink (if flaw *flaw-color* *step-color*)
			    :align-y :top
			    :align-x :left
			    :text-style (if flaw *flaw-font* *step-font*)))))
    (T nil)))

(defmethod draw-flag (text stream x y ink)
  (let* (
	 (width  (clim:stream-string-width stream text :text-style *step-font*))
	 (height (- (clim:text-style-height *step-font* stream) 1))
	 (top (- y (floor height 2)))
	 (left (- x width 7))
	 )
    (clim:draw-line* stream left top (+ left width) top :ink ink)
    (clim:draw-line* stream (+ left width) top x y :ink ink)
    (clim:draw-line* stream x y (+ left width) (+ top height) :ink ink)
    (clim:draw-line* stream (+ left width) (+ top height) left (+ top height) :ink ink)
    (clim:draw-line* stream left (+ top height) left top :ink ink)
    (clim:draw-text* stream text (+ left 2) top :align-x :left :align-y :top :text-style *step-font* :ink ink)))


(defmethod draw-reverse-flag (text stream x y ink)
  (let* (
	 (width  (clim:stream-string-width stream text :text-style *step-font*))
	 (height (- (clim:text-style-height *step-font* stream) 1))
	 (top (- y (floor height 2)))
	 (right (+ x width 7))
	 )
    (clim:draw-line* stream x y (- right width) top :ink ink)
    (clim:draw-line* stream (- right width) top right top :ink ink)
    (clim:draw-line* stream right top right (+ top height) :ink ink)
    (clim:draw-line* stream right (+ top height) (- right width) (+ top height) :ink ink)
    (clim:draw-line* stream (- right width) (+ top height) x y :ink ink)
    (clim:draw-text* stream text (+ (- right width) 2) top :align-x :left :align-y :top :text-style *step-font* :ink ink)))


(defmethod draw-link (frame stream node link graphic 
		      &key (detail :verbose) graphics)
  (declare (ignore frame detail))
  (let* (
	 (threats (get-link-threats link (plan-tree-entry node)))
	 (post (get-link-add-cond link))
	 (pre  (get-link-openc link))
	 (pre-graphic (get-translation graphics pre))
	 (post-graphic (get-translation graphics post))
	 (height (clim:text-style-height *step-font* stream))
	 )
    (ignore-errors
     (clim:with-output-as-presentation
	 (stream graphic 'graphic-link :single-box nil)
       (cond ((or (null post) (null post-graphic))
	      (if is-xii
		  (draw-flag "KNOWN" stream (graphic-x pre-graphic) (+ (graphic-y pre-graphic)
								       (floor height 2)) *step-color*)
		(draw-flag "CWA" stream (graphic-x pre-graphic) (+ (graphic-y pre-graphic)
								   (floor height 2)) *step-color*)))
	     (T
	      (let* (
		     (x1 (+ (graphic-x post-graphic) 5))
		     (y1 (+ (graphic-y post-graphic) (floor height 2)))
		     (x2 (- (graphic-x pre-graphic) 5))
		     (y2 (+ (graphic-y pre-graphic) (floor height 2)))
		     )
		(clim:draw-line* stream
				x1 y1 x2 y2
				:ink *step-color*)
		(dolist (threat threats)
		  (let* (
			 (threat-cond (get-threat-cond threat))
			 (threat-graphic (get-translation graphics threat))
			 (cond-graphic (get-translation graphics threat-cond))
			 )
		    (clim:with-output-as-presentation
			(stream threat-graphic
				'graphic-flaw :single-box nil)
		      (clim:draw-line* stream
				       (floor (+ x1 x2) 2)
				       (floor (+ y1 y2) 2)
				       (+ (graphic-x cond-graphic) 5)
				       (+ (graphic-y cond-graphic) (floor height 2))
				       :line-dashes T
				       :ink *flaw-color*)))))))))))






