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

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object Inspection

(define-vcr-command com-inspect-object ((graphic 'graphic))
  #+:composer(when *composer-started* (composer:winspect (graphic-object graphic)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation support

;;; brings up plan window
(defmethod locate-plan-window (node (frame vcr-window))
  (let ((window (find node (vcr-windows frame)
		      :key #'plan-window-node)))
    (cond (window
	   ;; XXX: CLIM bug. enable-frame doesn't work if user iconified the
	   ;; frame 
;;; 	   (clim:shrink-frame window)
;;; 	   (clim:enable-frame window)
	   (clim:raise-frame window)
	   )
	  (T
	   (open-plan-debug node frame *shell*)))))

;;; centering window around cursor
(defun scroll-to-center-if-lost-view (x y window) 
  (clim:with-bounding-rectangle* 
      (left top right bottom) (clim:window-viewport window)
      (when (or (< x left) (> x right) (< y top) (> y bottom))
	(clim:window-set-viewport-position 
	 window
	 (max 0 (- (floor x) (floor (- right left) 2)))
	 (max 0 (- (floor y) (floor (- bottom top) 2)))))))

;;; set cursor
(defun set-current (frame node graphic)
  (let (
	(graph (clim:get-frame-pane frame 'graph))
	(current (vcr-current frame))
	
	(color (get-option-value :vcr-present-node-color (vcr-options frame)))
	(scale (get-option-value :vcr-present-tree-scale (vcr-options frame)))
	)
    (setf (vcr-current frame) node)
    (when current
      (let ( (current-graphic (get-translation 
			       (vcr-graphics frame) 
			       current)) )
	(clim:with-scaling (graph scale)
	  (draw-node frame graph current current-graphic :coloring color :node-count (session-size (vcr-session frame))))))
    (scroll-to-center-if-lost-view (floor (* (graphic-x graphic) scale))
				   (floor (* (graphic-y graphic) scale))
				   graph)
    (clim:with-scaling (graph scale)
      (draw-node frame graph node graphic :current node))))


(defmethod select-plan (node frame graphic)
  (let ( (current (vcr-current frame)) )
    (cond ((not (eq node current))
	   (set-current frame node graphic))
	  (T
	   (locate-plan-window node frame)))))


;;; When you click on a node
(define-vcr-command com-vcr-window-select-plan ((graphic 'graphic)) 
  (let ((node (graphic-object graphic))
	(frame (graphic-frame graphic)))
    (select-plan node frame graphic)))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving window markers and clicking on them

(defun move-window (new-node window frame)
  (let (
	(graph (clim:get-frame-pane frame 'graph))	
	(scale    (get-option-value :vcr-present-tree-scale (vcr-options
							     frame))) 
	)
    (erase-window-marker frame graph window)
    (setf (plan-window-node window) new-node)
    (clim:with-scaling
	(graph scale)
      (draw-window-marker frame graph window))
    (initialize-window window)
    (show-window window)
    (setf (vcr-current frame) new-node)))

(defmethod select-window (window)
  (let* ((vcr (plan-window-vcr window))
	 (graph (clim:get-frame-pane vcr 'graph))
	 (node-graphic
	  (clim:tracking-pointer
	     (graph :context-type 'graphic-node)
	     (:pointer-motion
	      (window)
	      (message vcr "Move Window: Select node to view. Scroll window to view tree")
	      (clim:unhighlight-highlighted-presentation window))
	     (:presentation
	      (presentation)
	      (clim:set-highlighted-presentation graph presentation)
	      (message vcr "Move Window: Click left to select node")
	      )
	     (:presentation-button-press
	      (presentation event)
	      (let ( (object (clim:presentation-object presentation)) )
		;; how to allow a modifier key here?  
		(if (eq (clim:pointer-event-button event)
			clim:+pointer-left-button+)
		    (progn 
		      (message vcr "Move Window: Node Selected")
		      (return object))
		  (progn
		    (message vcr "Move Window: Aborted.")
		    (return nil)))))
	     (:pointer-button-press 
	      (event)
	      (declare (ignore event))
	      (message vcr "Move Window: Aborted.")
	      (return nil)))))
      (when node-graphic
	(select-plan (graphic-object node-graphic) window node-graphic))))

;;; When you click on a window
(define-vcr-command (com-select-window) ((window 'plan-window)) 
  (select-window window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Animation/Navigation support for Buttons

;;; locate prev in graph
(defun plan-tree-graph-prev (node graphic table)
  (let (
	(prev (plan-tree-prev node))
	)
    (cond (prev
	   (let (
		 (prev-graphic (get-translation table prev))
		 )
	     (if (graphic-shown prev-graphic)
		 (values prev prev-graphic)
	       (plan-tree-graph-prev prev prev-graphic table))))
	  (T
	   (if (and graphic (graphic-shown graphic))
	       (values node graphic)
	     (values nil nil))))))

;;; locate next in graph
(defun plan-tree-graph-next (node graphic table)
  (let (
	(next (plan-tree-next node))
	)
    (cond (next
	   (let (
		 (next-graphic (get-translation table next))
		 )
	     (if (graphic-shown next-graphic)
		 (values next next-graphic)
	       (plan-tree-graph-next next next-graphic table))))
	  (T
	   (if (and graphic (graphic-shown graphic))
	       (values node graphic)
	     (values nil nil))))))

;;; Pause during animation
(defun vcr-sleep (ws)
  (multiple-value-bind (gesture type)
      (clim:read-gesture :stream ws :timeout (/ 1 10))
    (declare (ignore gesture))
    (eq type :timeout)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The VCR Button Controls

(defun stop (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*))
    (setf (vcr-interrupt frame) t)))
    
(defun rewind (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*)
	 (current (vcr-current frame))
	 (graphic (get-translation (vcr-graphics frame) current))
	 (graph   (clim:get-frame-pane frame 'graph))
	 )
    (setf (vcr-interrupt frame) nil)
    (loop
      (unless (and current (vcr-sleep graph))
	(return nil))
      (when (vcr-interrupt frame) 
	(setf (vcr-interrupt frame) nil)
	(return nil))
      (multiple-value-bind 
	  (prev prev-graphic)
	  (plan-tree-graph-prev current graphic (vcr-graphics frame))
	(when (and (not (null prev)) (not (eq prev current)))
	  (set-current frame prev prev-graphic))
	(cond ((or (null prev) (eq prev current))
	       (setq current nil)
	       (setq graphic nil))
	      (T
	       (setq current prev)
	       (setq graphic prev-graphic)))))))
 

(defun backward (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*)
	 (current (vcr-current frame))
	 (graphic (get-translation (vcr-graphics frame) current))
	)
    (when current
      (multiple-value-bind 
	  (prev prev-graphic)
	  (plan-tree-graph-prev current graphic (vcr-graphics frame))
	(when prev
	  (set-current frame prev prev-graphic))))))

(defun forward (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*)
	 (current (vcr-current frame))
	 (graphic (get-translation (vcr-graphics frame) current))
	)
    (when current
      (multiple-value-bind 
	  (next next-graphic)
	  (plan-tree-graph-next current graphic (vcr-graphics frame))
	(when next
	  (set-current frame next next-graphic))))))

(defun fast-forward (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*)
	 (current (vcr-current frame))
	 (graphic (get-translation (vcr-graphics frame) current))
	 (graph   (clim:get-frame-pane frame 'graph))
	 )
    (setf (vcr-interrupt frame) nil)
    (loop
      (unless (and current (vcr-sleep graph))
	(return nil))
      (when (vcr-interrupt frame) 
	(setf (vcr-interrupt frame) nil)
	(return nil))
      (multiple-value-bind 
	  (next next-graphic)
	  (plan-tree-graph-next current graphic (vcr-graphics frame))
	(when (and (not (null next)) (not (eq next current)))
	  (set-current frame next next-graphic))
	(cond ((or (null next) (eq next current))
	       (setq current nil)
	       (setq graphic nil))
	      (T
	       (setq current next)
	       (setq graphic next-graphic)))))))


;;;;;;;;;;;;;;;;;;;;
;;; Menus

(defun install-vcr-menus ()
  (with-command-table ('vcr-tree-tab)

    (inst-menu (cdr (assoc :full display-options))  :command '(com-vcr-tree :full) 
	       :documentation "View all nodes")
    
    (inst-menu (cdr (assoc :steps display-options)) :command '(com-vcr-tree :steps ) 
	       :documentation
	       "View steps only") 
    (inst-menu (cdr (assoc :search display-options))  :command '(com-vcr-tree :search) 
	       :documentation "View nodes searched only") 
    (inst-menu (cdr (assoc :soln display-options)) :command '(com-vcr-tree :soln ) 
	       :documentation
	       "View start and finish only") 
    (inst-menu (cdr (assoc :soln-path display-options)) :command '(com-vcr-tree :soln-path) 
	       :documentation 
	       "View nodes along solution only")  

    (inst-menu "vd1" :divider :line)
    
    (inst-menu "expand node" :command '(com-vcr-tree :expand1) 
	       :documentation 
	       "View children of current node") 
    (inst-menu "contract node" :command '(com-vcr-tree :contract1) 
	       :documentation 
	       "Hide children of current node") 
    (inst-menu "expand branch" :command '(com-vcr-tree :expand) 
	       :documentation 
	       "View subtree of current node")
    (inst-menu "contract branch" :command '(com-vcr-tree :contract) 
	       :documentation 
	       "Hide subtree of current node")

    (inst-menu "vd2" :divider :line)
    
    (inst-menu "Print Tree" :command '(com-vcr-tree :print) 
	       :documentation "Print Tree")
    )
  (with-command-table ('vcr-options-tab)
    (inst-menu "VCR options..." :command '(com-options :vcr)
	       :documentation "Options for VCR window"))
  )





(define-vcr-command com-vcr-tree ((temp t))
;;;   (documentation)
  (let* ((frame clim:*application-frame*)
	 (graphics        (vcr-graphics frame))
	 (node            (session-tree (vcr-session frame)))
	 (graphic         (get-translation graphics node))
	 (current-node    (vcr-current frame))
	 (current-graphic (if (not (null current-node))
			      (get-translation graphics current-node))))
    (when temp
      (if (eq temp :print)
	(let* ((filename (file-dialog))
	       (stream (and filename
			    (open filename
				  :direction :output
				  :if-exists :overwrite
				  :if-does-not-exist :create))))
	  (when stream
	    (print-window frame stream)
	    (close stream))))
      (when (member temp '(:full :steps :search :soln :soln-path))
	(setf (vcr-tree-desc frame) temp))
      (case temp
	  (:full (full-display node graphic graphics))
	  (:steps (step-display node graphic graphics))
	  (:search (search-display node graphic graphics))
	  (:soln (soln-display node graphic graphics))
	  (:soln-path (soln-path-display node graphic graphics))
	  (:contract1
	   (when current-node
	     (retract1-display current-node current-graphic graphics)))
	  (:expand1
	   (when current-node
	     (expand-display current-node current-graphic graphics)))
	  (:contract
	   (when current-node
	     (retract-display current-node current-graphic graphics)))
	  (:expand
	   (when current-node
	     (full-display current-node current-graphic graphics))))
	(layout-window frame)
	(show-window frame))))

(defmethod exit-frame ((frame vcr-window))
  (delete-window frame *shell*))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Translators

(clim:define-presentation-to-command-translator select-a-plan
    (graphic-node com-vcr-window-select-plan vcr 
	       :gesture :select :documentation "Select Node"
	       :pointer-documentation 
	       ((object stream) . ((cond ((and (eq (type-of (graphic-frame
							     object))
						   'vcr-window) 
					       (eq (vcr-current (graphic-frame
								 object)) 
						   (graphic-object object)))
					  (format stream "Open Window"))
					 (T
					  (format stream "Select Node"))))))

  (object)
  `(,object))

;;; Right now shift-pointer-1 is not the official way of selecting.
;;; Rather it's an "aid" that helps to aim.
(clim:define-presentation-to-command-translator select-a-window
    (plan-window com-select-window vcr 
		 :gesture :move :documentation "Move Window"
		 :pointer-documentation "Move Window")
  (object)
  `(,object))

(clim:define-presentation-to-command-translator select-a-window-2
    (plan-window com-select-window vcr 
		 :gesture :select :documentation "Move Window"
		 :pointer-documentation "Move Window")
  (object)
  `(,object))

(clim:define-presentation-to-command-translator inspect-a-tree
    (graphic com-inspect-object vcr 
	     :documentation "Inspect Object" :gesture :describe
	     :pointer-documentation "Inspect Object")
  (object)
  `(,object))

