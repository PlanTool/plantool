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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File requester

(defun file-dialog (&optional name)
  (let ((filename (or name "./")))
    (setf filename (clim:select-file *shell* :pattern name))
    (if (and filename
	     (probe-file filename)
	     (clim:notify-user 
	      *shell*
	      (format nil "File ~a already exists. Should I overwrite?" 
		      filename)
	      :title "File dialog"
	      :text-style *small-font*
	      :style :question
	      :exit-boxes '((:exit "Overwrite") (:abort "Cancel")))
	     filename)
	filename)
    filename))
      

;;; --- Stuff to assist in making options dialogs.

;;; Creates a button for use in the options dialog.
;;; Note that the value I pass in is a cons cell pointing
;;; to the value to be used, so I can have the value
;;; be changeable in the body of my call to accept(ugly).

(defun option-button (cell option stream)
  (let* ((value (car cell))
	 (choice value))

    (clim:with-text-style (stream *small-font*)
      (setf choice (clim:accept (cons 'member (option-range option))
		 :stream stream
		 :prompt (format nil "~a" (option-name option))
		 :default choice
		 :view `(clim:list-pane-view
			 :name-key ,#'range-pair-documentation
 			 :value-key ,#'range-pair-value
			 :text-style ,*small-font*
			 :background ,+light-gray+
			 ))))
    (when choice
      (rplaca cell choice))
    (terpri stream)))


(defmacro yes-no-button (stream prompt value)
  `(setf ,value (clim:accept 'clim:boolean
			     :default ,value
			     :stream ,stream
			     :prompt-mode :raw
			     :prompt ,prompt
			     :view '(clim:toggle-button-view
				     :text-style ,*menu-item-font*)
			     )))

;;; Creates a dialog for adjusting options.
;;; I use a list to hold the new values of the options prior
;;; to approval.
(defun options-dialog (options)
  (let (
	(default nil)
	(dialog *query-io*)
	(values (mapcar #'option-value options))
	)
    
    (clim:accepting-values (dialog :own-window T
				   :label "Options dialog"
				   :exit-boxes '((:exit "Apply") 
						 (:abort "Cancel"))
				   :background +cadet-blue+
				   :align-prompts t
				   :resynchronize-every-pass t)
     (let ((i 0))
       (dolist (option options)
	 (option-button (nthcdr i values) option dialog) 
	 (terpri dialog)
	 (incf i)))
     
     (yes-no-button dialog "Make these options be the default? " default)
     (terpri dialog))
    
    (let ( (i 0) )
      (dolist (option options)
	(setf (option-value option)
	  (nth i values))
	(incf i)))
    
    (when default
      (set-option-set options)))
  
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Planning Commands

(defmacro domain-button (domain domains problem subproblems problems stream)
  `(progn
     (clim:with-text-style (,stream ',*menu-item-font*)
       (format ,stream "Domains~%"))
     (setf ,domain (clim:accept (cons 'member ,domains)
				:prompt nil
				:stream ,stream
				:default ,domain
				:view `(clim:list-pane-view
					:background ,+light-gray+
					:visible-items 8
					:name-key ,#'(lambda (item) 
						       (princ-to-string
							(if (eq item T)
							    "Any"
							  (get-domain-name
							   item))))
					:text-style ,*menu-item-font*)))
     (when ,domain
       (if (eq ,domain T)
	   (setf ,subproblems ,problems)
	 (setf ,subproblems (remove-if-not 
			     #'(lambda (p)
				 (eq (get-problem-domain p)
				     (get-domain-name ,domain)))
			     ,problems)
	       ,problem (car ,subproblems))))))

(defmacro text-box (var type stream default prompt width 
		    &key (font *small-font*))
  `(clim:with-text-style (,stream ',font)
     (setf ,var (clim:accept ,type :stream ,stream 
			     :default ,default
			     :prompt ,prompt
			     :view '(clim:text-field-view
				     :background ,+light-gray+
				     :width ,width
				     :editable-p t
				     :text-style ,font))
	   )))

  
(defmacro hbox (stream &rest code)
  `(clim:formatting-table (,stream)
     (clim:formatting-row (,stream)
       ,@(let ((cc nil))
	   (dolist (c code (reverse cc))
	     (push (list 'clim:formatting-cell 
			 `(,stream :align-y :top) c)
		   cc))))))


(defmacro problem-button (problem problems stream)
  `(progn 
     (clim:with-text-style (,stream ',*menu-item-font*)
       (format ,stream "Problems~%"))
     (setf ,problem (clim:accept (cons 'member ,problems)
				 :prompt nil
				 :stream ,stream
				 :view `(clim:list-pane-view
					 :background ,+light-gray+
					 :visible-items 8
					 :name-key ,#'(lambda (item) 
							(princ-to-string
							 (get-problem-name
							  item)))
					 :text-style ,*menu-item-font*)))))
			  
(defun plan-dialog ()
  (setf *enable-vcr* t)  ;; enabled by default
  (let* (
	 (problems      (get-problems))
	 (subproblems   problems)
	 (domains       (append (get-domains) '(T)))
	 (domain         T)
	 (problem        (car problems))
	 (limit          (get-search-limit))
	 (search-control nil)
	 (search-f       'bf-control)
	 (controller-f   nil)
	 (debug          *enable-vcr*)
	 (debug-firings  nil)
	 (dialog *query-io*)
	 (str "")
	 )
    (clim:accepting-values
	(dialog :own-window T  :label "Planning session"
		:background +cadet-blue+ 
		:exit-boxes '((:exit "Plan") (:abort "Abort"))
		:resynchronize-every-pass t)
	 
      ;; enforce constraints
      (when (< limit 1) (setf limit 1))
	 
      (terpri dialog)
      (hbox dialog 
	    (domain-button domain domains problem 
			   subproblems problems dialog)
	    (problem-button problem subproblems dialog))

      (terpri dialog)
      (when (and problem domain)
	(setf str (with-output-to-string (str)
		    (format str "Init: ~A~%" (get-problem-inits problem))
		    (format str "Goal: ~A~%" (get-problem-goal problem))
		    (format str "Rank Function: ~A~%" 
			    (get-problem-rank-f problem)))))
      (clim:present str 'string 
		    :stream dialog 
		    :view `(clim:text-editor-view
			    :background ,+light-gray+
			    :nlines 12
			    :ncolumns 35
			    :text-style ,*small-font*))
      (terpri dialog)
      (hbox dialog
	    (yes-no-button dialog "Search Control: " search-control)
	    (yes-no-button dialog "Record for debugging: " debug)
	    (yes-no-button dialog "Record search-control: " debug-firings))
      (terpri dialog)
      (hbox dialog
	    (if search-control (text-box controller-f 'symbol dialog 
					 controller-f "Search Controller" 250)
	      (text-box search-f 'symbol dialog search-f 
			"Search Function" 250))
	    (text-box limit 'integer dialog limit "Search Limit" 100 )
	    ))
	
    ;; dunno why the old accept-value dialog works but notify-user doesn't,
    ;; so I return nil here to indicate failure.
    (if (or (and (null search-control) (null search-f))
	    (and search-control (null controller-f)))
	(progn (window-msg "Error: Search function not specified."
			   :window *shell* :style :error)
	       nil)
      (values problem limit controller-f search-f debug debug-firings))))

(defmacro flaw-button (flaw flaws plan stream)
  `(progn 
     (clim:with-text-style (,stream ',*small-font*)
       (format ,stream "Flaws:~%"))
     (setf ,flaw (clim:accept (cons 'member ,flaws)
				 :prompt nil
				 :stream ,stream
				 :default ,flaw
				 :view `(clim:list-pane-view
					 :scroll-bars :vertical
					 :background ,+light-gray+
					 :visible-items 8
					 :name-key ,#'(lambda (item) 
							(princ-to-string
							 (get-bound-clause item ,plan)))
					 :text-style ,*menu-item-font*)))))


(defun plan-from-node-dialog (plan use-flaw flaws sc search-f rank-f)
  (let (
	(choose-flaw t)
	(flaw (or use-flaw (car flaws)))
	(limit 1)
	(dialog *query-io*)
	(record nil)
	)
      (clim:accepting-values
	  (dialog
	   :own-window T
 	   :background +cadet-blue+
 	   :label "Plan from node"
	   :width *plan-replan-window-width*
  	   :scroll-bars :vertical
	   :exit-boxes '((:exit "Plan")
			 (:abort "Abort"))
 	   :resynchronize-every-pass t
           )
    (clim:with-text-style (dialog *small-font*)
      
      ;; enforce constraints
;;; 	(when choose-flaw (setf limit 1))
	(when (< limit 1) (setf limit 1))
	 
	(terpri dialog)
	(format dialog "Plan: ~a~%" plan)
	(if sc (format dialog "Search Controller: ~a~%" sc)
	  (progn (format dialog "Search Function: ~a~%" search-f)
		 (format dialog "Rank Function: ~a~%" rank-f)))
 	(text-box limit 'integer dialog limit "Search Limit" 100)

	(terpri dialog)
	(hbox dialog
;;; 	      (yes-no-button dialog "Choose Flaw: " choose-flaw)
	      (yes-no-button dialog "Record Firings: " record))
	(terpri dialog)
	(when choose-flaw
	  (flaw-button flaw flaws plan dialog))
	(terpri dialog)
	(terpri dialog))
	
      (values (if choose-flaw flaw) limit record))))





