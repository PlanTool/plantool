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

(defvar *plan-display-left-border* 100)
(defvar *plan-display-top-border* 100)
(defvar *plan-display-x-offset* 300)
(defvar *plan-display-y-offset* 200)

(defun layout-tree (node graphic stream &key x y (shift 0) scale)
  (setf (graphic-y graphic) y)
  (let* (
	 (label-width
	  (if (>= scale 16) 0
	    (floor (- (* (clim:stream-string-width
			  stream (get-iconic-reason-label (plan-tree-entry node)))
			 scale)
		      110) 2)))
	 (real-shift (max label-width shift))
	 )
    (if (graphic-node-nodes graphic)
	   (let ( (children (graphic-node-nodes graphic)) )
	     (multiple-value-bind
		 (max-shift width)
		 (layout-tree (graphic-object (car children)) (car children) stream
			      :x x :y (+ y 260) :shift real-shift :scale scale)
	       (setf (graphic-x graphic) (+ x max-shift))
	       (setf x width)
	       (dolist (child-graphic (cdr children))
		 (multiple-value-bind (rest-shift width)
		     (layout-tree (graphic-object child-graphic) 
				  child-graphic stream
				  :x x :y (+ y 260) :shift 0 :scale scale)
		   (declare (ignore rest-shift))
		   (setf x width)))
	       (values max-shift x)))
      (progn
	(setf (graphic-x graphic) (+ x real-shift))
	(values real-shift (+ x 155 (* real-shift 2)))))))


(defun layout-plan (plan plan-table stream &key (detail :verbose) style)
  (declare (ignore style))
  (let (
	(left     *plan-display-left-border*)
	(top      *plan-display-top-border*)
	(x-offset *plan-display-x-offset*)
	(y-offset *plan-display-y-offset*)
	
	(order             (get-plan-ordering plan))	
	)
    
    
    ;; push init and goal graph-steps on list.
    (let ( (init (get-plan-step plan (if is-xii 0 :init))) )
      (if init		;if exists
	  (let ( (init-graphic (get-translation plan-table init)) )
	    (layout-step init init-graphic stream
			    :x left :y (+ top (floor y-offset 2))
			    :plan plan
			    :table plan-table
			    :detail detail))
	(setq left (- left x-offset))))

    (let ( (goal (get-plan-step plan :goal)) )
      (cond (goal		;if exists
	     (let ( (goal-graphic (get-translation plan-table goal)) )
	       (layout-step goal goal-graphic stream
			    :x (+ left (* (+ (get-plan-num-steps plan) 1)
					  x-offset))
			    :y (+ top (floor y-offset 2))
			    :plan plan
			    :table plan-table
			    :detail detail)))))
    
    ;; push all other steps onto graph-step list
    (dotimes (i (get-plan-num-steps plan))
      (let* (
	     (step-id (nth i order))
	     (step (get-plan-step plan step-id))
	     )
	(let ( (step-graphic (get-translation plan-table step)) )
	  (layout-step step step-graphic stream
		       :x (+ left (* x-offset (+ i 1)))
		       :y (+ top (* (rem i 2) y-offset))
		       :plan plan
		       :table plan-table
		       :detail detail))))))


(defun layout-step (step graphic stream &key x y plan (detail :verbose) table)
  (setf (graphic-x graphic) x)
  (setf (graphic-y graphic) y)
  (let (
	(effects (get-step-effects step))
	(open    (get-step-openc step plan))
	(cond    (get-step-cond step plan))
	)
    (case detail
      (:verbose
       (let (
	     (ex (+ x 2))
	     (ey (- y 19))
	     )
	 (dolist (effect effects)
	   (multiple-value-bind
	       (width height)
	       (layout-effect effect (get-translation table effect) stream
			      :x ex :y ey :table table :detail detail)
	     (declare (ignore width))
	     (setq ey height))))
       (let (
	     (cx (- x 2))
	     (cy (- y 19))
	     (height (clim:text-style-height *step-font* stream))
	     )
	 (dolist (condition cond)
	   (layout-pre-condition condition (get-translation table condition) stream
				 :x cx :y cy :plan plan :detail detail)
	   (setq cy (+ cy height)))
	 (dolist (openc open)
	   (layout-pre-condition openc (get-translation table openc) stream
				 :x cx :y cy :plan plan :detail detail)
	   (setq cy (+ cy height)))))
    (:terse
     (let (
	   (ex (+ x 20))
	   (cx (- x 20))
	   )
       	 (dolist (effect effects)
	   (multiple-value-bind (width height)
	       (layout-effect effect (get-translation table effect) stream
			      :x ex :y y :table table :detail detail)
	     (declare (ignore height width))))
  	 (dolist (condition cond)
	   (layout-pre-condition condition (get-translation table condition) stream
				 :x cx :y y :plan plan :detail detail))
	 (dolist (openc open)
	   (layout-pre-condition openc (get-translation table openc) stream
				 :x cx :y y :plan plan :detail detail)))))))


(defun layout-pre-condition (cond graphic stream &key x y plan (detail :verbose))
  (case detail 
    (:verbose
     (let* (
	    (label (condition-label (get-bound-clause cond plan)))
	    (width (clim:stream-string-width stream label :text-style *step-font*))
	    )
       (setf (graphic-x graphic) (- x width))
       (setf (graphic-y graphic) y)))
    (:terse
     (setf (graphic-x graphic) x)
     (setf (graphic-y graphic) y)))
  (values x y))

(defun layout-post-condition (cond graphic stream &key x y plan (detail :verbose))
  (case detail 
    (:verbose
     (let* (
	    (label (condition-label (get-bound-clause cond plan)))
	    (width (clim:stream-string-width stream label :text-style *step-font*))
	    )
       (setf (graphic-x graphic) (+ x width))
       (setf (graphic-y graphic) y)))
    (:terse
     (setf (graphic-x graphic) x)
     (setf (graphic-y graphic) y)))
  (values x y))


(defun layout-effect (effect graphic stream 
		      &key x y table plan (detail :verbose))
  (declare (ignore graphic))
  (let (
	(tx x)
	(ty y)
	)
    (case detail
      (:verbose
       (let ( (height (clim:text-style-height *step-font* stream)) )
	 (dolist (add (get-effect-add effect))
	   (layout-post-condition add (get-translation table add) stream 
			       :x tx :y ty :plan plan :detail detail)
	   (setf ty (+ ty height)))))
      (:terse
       (dolist (add (get-effect-add effect))
	 (layout-post-condition add (get-translation table add) stream 
				:x tx :y ty :plan plan :detail detail))))
    (values x ty)))




