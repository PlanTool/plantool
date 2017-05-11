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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following code defines the framework of the CLIM application VCR.

(in-package 'vext)
(defvar is-xii nil)

(in-package 'vcr)

(import '(vext::is-xii
	  vext::ucpop-bug
	  vext::ucpop-plan
	  vext::get-problems
	  vext::get-problem-name
	  vext::get-problem-domain
	  vext::get-problem-inits
	  vext::get-problem-goal
	  vext::get-problem-rank-f
	  vext::get-domains
	  vext::get-domain-name
	  vext::get-domain-operators
	  vext::get-domain-axioms
	  vext::get-domain-facts
	  vext::get-search-functions
	  vext::get-controller-functions
	  vext::set-search-limit
	  vext::get-search-limit
	  vext::get-plans-searched
	  vext::get-bound-clause
	  vext::get-iconic-reason-label
	  vext::get-full-reason-label
	  vext::get-plan-diff
	  vext::get-plan-step?
	  vext::get-plan-unsafe
	  vext::get-plan-open
	  vext::get-plan-ordering
	  vext::get-plan-num-steps
	  vext::get-plan-links
	  vext::get-plan-flaws
	  vext::get-plan-steps
	  vext::get-plan-step
	  vext::get-step-effects
	  vext::get-step-cond
	  vext::get-step-openc
	  vext::get-step-precond
	  vext::get-step-id
	  vext::get-step-action
	  vext::get-step-execute?
	  vext::get-step-executed?
	  vext::get-effect-id
	  vext::get-effect-add
	  vext::get-effect-forall
	  vext::get-effect-precond
	  vext::get-link-to
	  vext::get-link-from
	  vext::get-link-effect
	  vext::get-link-openc
	  vext::get-link-add-cond
	  vext::get-link-threats
	  vext::get-threat-id
	  vext::get-threat-cond
	  vext::get-openc-id
	  vext::get-fact-condition
	  vext::get-fact-function
	  vext::get-sc-rules
	  vext::get-sc-rule-name
	  vext::display-plan-stat
	  vext::display-text-plan
	  vext::condition-label
	  vext::get-planning-result
	  vext::display-planner-specific-objects
	  ))


(export '(;; user level functions
	  *enable-vcr*
	  initialize-plan-debugger
	  run-plan-debugger
	  window-msg
	  display-scrolled-text
	  ;; utility
	  set-mono-defaults
	  set-color-defaults
	  vcr-display-stat
	  shell-display-status
	  shell-show-busy

	  *vcr-recording*
	  *vcr-record-firings*	  
	  *pdb-running*

	  ;;User customizable variables
	  ;; Miscellaneous fonts.
	  *tree-font*
	  *small-font*
	  *menu-font*
	  *menu-item-font*
	  *doc-font*
	  
	  ;; Window sizes
	  *shell-width*
	  *vcr-width*
	  *vcr-height*
	  *plan-height*
	  *plan-width*
	  *browser-height*
	  *browser-width*
	  *plan-min-height*
	  *plan-min-width*
	  *tree-desc-width*
	  *plan-replan-window-width*
	  
	  ;; session colors
	  *session-color*
	  *session-font*
	  *current-color*
	  *current-font*

	  ;; plan display colors
	  *flaw-color*
	  *flaw-font*
	  *step-color*  
	  *step-font*
	  *diff-color*  
	  *diff-font*

	  ;; misc colors
	  *text-bg-color*
	  *info-color*
	  *menubar-fg-color*
	  *menubar-bg-color*
	  *plan-window-color*
	  *vcr-bg-color*
	  *movie-bg-color*
	  *doc-bg-color*
	  *doc-fg-color*
	  *vcr-show-progress*		; if null, don't display progress note
	  *progress-threshold*		; how many plans to draw before we show
					; progress  
	  *db-arrow-color*		;  VCR button colors
	  *vcr-update-after-replan*	;recompute & redisplay plan tree after replanning?
                     *pdb-start-composer*
	  *arrow-color*
	  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0.5 VCR package user customizable variables

;;; The idea here is to have user to (setf *pdb-use-colors* nil) if black/white
;;; display is wanted.  

(in-package 'user)
(defvar *pdb-use-colors* t)
(defvar *pdb-startup-hook* nil)
(in-package 'vcr)
(import '(user::*pdb-use-colors* user::*pdb-startup-hook*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this is used for creating pixmaps during compile time.
  (clim:define-application-frame dummy ()
    () 
    (:menu-bar nil) 
    (:panes (dum :application)) 
    (:layouts (default dum)))

  (defvar *dummy-frame* (clim:make-application-frame 'dummy))

  (defvar +light-gray+ (clim:make-gray-color 0.7))
  (defvar +cadet-blue+ (clim:find-named-color "cadetblue"  (clim:frame-palette
						   *dummy-frame*)))
  (defvar +gray+ (clim:make-gray-color 0.5))

;;; User customizable variables  

  ;; Miscellaneous fonts.
  (defvar *tree-font* '(:serif :roman :tiny))
  (defvar *small-font* '(:sans-serif :roman :small))
  (defvar *menu-font* '(:serif :bold :normal))
  (defvar *menu-item-font* '(:serif :bold :small))
  (defvar *doc-font*  '(:san-serif nil :small))

  ;; Window sizes
  (defvar *shell-width* 600)   ; It's best not to get below this threshold.
  (defvar *shell-height* nil)  ; default height
  (defvar *vcr-width* 800)
  (defvar *vcr-height* 600)
  (defvar *plan-height* 500)
  (defvar *plan-width* 400)
  (defvar *browser-height* 500)
  (defvar *browser-width* 400)
  (defvar *plan-min-height* 500)
  (defvar *plan-min-width* 400)
  (defvar *tree-desc-width* 200)
  (defvar *plan-replan-window-width* 400)
  ;; session colors
  (defparameter *session-color* clim:+black+)
  (defparameter *session-font* (clim:make-text-style :sans-serif :roman :small))
  (defparameter *current-color* clim:+red+)
  (defparameter *current-font* (clim:make-text-style :sans-serif :roman :tiny))

  ;; plan display colors
  (defparameter *flaw-color* clim:+red+)
  (defparameter *flaw-font*  (clim:make-text-style :sans-serif :roman :small))
  (defparameter *step-color*   clim:+blue+)
  (defparameter *step-font*  (clim:make-text-style :sans-serif :roman :small))
  (defparameter *diff-color*   clim:+yellow+)
  (defparameter *diff-font*  (clim:make-text-style :sans-serif :roman :small))
  (defparameter *small-fixed-font*  (clim:make-text-style :fix :bold 10))
  
  ;; misc colors
  (defvar *text-bg-color* +gray+)
  (defvar *info-color* clim:+blue+)
  (defvar *menubar-fg-color* clim:+black+)
  (defvar *menubar-bg-color* +cadet-blue+)
  (defvar *plan-window-color* +gray+)
  (defvar *vcr-bg-color* clim:+white+)
  (defvar *movie-bg-color* clim:+white+)
  (defvar *doc-bg-color* +gray+)
  (defvar *doc-fg-color* clim:+black+)
  (defvar *db-arrow-color* clim:+red+)		;  VCR button colors
  (defvar *arrow-color* clim:+blue+)

  ;;Others
  (defvar *vcr-show-progress* t)	; if null, don't display progress note
  (defvar *vcr-update-after-replan* t)	; if null,  just redisplay the plan window (much faster)
  (defvar *progress-threshold* 200)	; how many plans to draw before we show
					; progress 
  (defvar *pdb-start-composer* t)
  (defvar *session-name-limit* 30)      ; max length of session names in listbox widget.
;;; End user variables
  )

;; set vcr to use black & white display instead of colors
(defun SET-MONO-DEFAULTS ()
  (setf *session-color* clim:+black+)
  (setf *current-color* clim:+black+)
  (setf *current-font* (clim:make-text-style :sans-serif :italic :tiny))
  (setf *flaw-color*   clim:+black+)
  (setf *step-color*   clim:+black+)
  (setf *diff-color*   clim:+black+)
  (setf *text-bg-color* clim:+white+)
  (setf *info-color*    clim:+black+)
  (setf *menubar-fg-color* clim:+black+)
  (setf *menubar-bg-color* +light-gray+)
  (setf *plan-window-color* clim:+white+)
  (setf *vcr-bg-color* clim:+white+)
  (setf *movie-bg-color* clim:+white+)
  (setf *db-arrow-color* clim:+white+)
  (setf *arrow-color* clim:+black+)
  (setf +cadet-blue+ +light-gray+)
  (setf +gray+ +light-gray+)
  (setf *doc-bg-color* +light-gray+)
  )

#+:composer (defvar *composer-started* (and (member :composer-v2.0 *features*) (wt:composer-initialized-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Internal variables and structures
(clim:define-command-table shell-debug-tab)
(clim:define-command-table shell-options-tab)
(clim:define-command-table shell-windows-tab)
(clim:define-command-table shell-help-tab)

;;; --- Internal variables
(defvar *root*          nil)		; Where to show the movie
(defvar *shell*         nil)                    ; ??
(defvar *colors*        nil)                   ; color table for general use
(defvar *help-window*   nil)
(defvar *vcr-palette* nil "Palette used by VCR")

(defvar *pdb-running*   nil) ; is pdb running?
(defvar *vcr-recording* nil)
(defvar *vcr-record-firings* nil)
(defvar *enable-vcr* nil)

(defvar *b-defined*     nil)
(defvar *l-triangle*    nil)
(defvar *r-triangle*    nil)
(defvar *db-l-triangle* nil)
(defvar *db-r-triangle* nil)
(defvar *s-rectangle*   nil)
(defvar *arrow-size*    5)

(defmacro make-pixmap (medium polygon color)
  `(clim:with-output-to-pixmap (mv ,medium)
     (setf (clim:medium-background medium) ,+gray+)
     (clim:with-scaling (mv *arrow-size*)
       (clim:draw-polygon* mv
			   ,polygon
			   :closed t :filled t 
			   :clipping-region clim:+nowhere+
			   :ink ,color))))


(defun initialize-pixmaps (medium)
  (unless *pdb-use-colors*
    (setf *db-arrow-color* clim:+white+)
    (setf *arrow-color* clim:+black+))

  (setf *db-l-triangle* (make-pixmap medium '(0 2 2 0 2 2 4 0 4 4 2 2 2 4)
                                     *db-arrow-color*))
  (setf *db-r-triangle* (make-pixmap medium '(0 0 0 4 2 2 2 4 4 2 2 0 2 2)
                                     *db-arrow-color*))
  (setf *l-triangle*    (make-pixmap medium '(0 2 4 0 4 4) *arrow-color*))
  (setf *r-triangle*    (make-pixmap medium '(0 4 0 0 4 2) *arrow-color*))
  (setf *s-rectangle*   (make-pixmap medium '(0 0 0 4 4 4 4 0) *db-arrow-color*))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.5 Load time execution

;;; This gives you a GC cursor in motif
#+:clim-motif (setq xm-silica::*use-clim-gc-cursor* t)

;;; Make the pixmaps in advance
(initialize-pixmaps (clim:sheet-medium 
		     (clim:get-frame-pane *dummy-frame* 'dum)))

(unless *pdb-use-colors* (SET-MONO-DEFAULTS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Defining the display interface


(clim:define-application-frame shell (clim:activity-frame)
  (
   (current-session    :initarg :c-session :initform nil 
		       :accessor shell-current-session)
   (sessions           :initarg :sessions :initform nil 
		       :accessor shell-sessions)
   (windows            :initform nil :accessor shell-windows)
   (debug-windows      :initform nil :accessor shell-debug-windows)
   (plan-debug-windows :initform nil :accessor shell-plan-debug-windows)
   (options            :initarg :options :initform nil :accessor shell-options)
   )
  (:command-table (shell
		   :inherit-from (clim:accept-values-pane shell-debug-tab 
							  shell-options-tab 
							  shell-windows-tab
							  shell-help-tab)
		   :menu (("Windows" :menu shell-windows-tab)
			  ("Debugger" :menu shell-debug-tab)
			  ("Options" :menu shell-options-tab)
			  ("Help" :menu shell-help-tab))))
  (:menu-bar nil)
  (:panes (menu :menu-bar
		:text-style *menu-font*
		:background *menubar-bg-color*
		:foreground *menubar-fg-color*
		)
	  ;; it's useless to specify scrollbars here
	  (graph clim:text-editor
		 :background +light-gray+
		 :width '(15 :character)
   		 :min-height '(15 :line)
		 :editable-p nil
 		 :initial-cursor-visibility :inactive
  		 :scroll-bars :vertical
		 :text-style *session-font*
		 :word-wrap t
		 :end-of-line-action :allow
		 :end-of-page-action :allow
		 )
	  (info-button clim:push-button
		       :label "Info"
		       :id 'info
		       :activate-callback 'shell-button-callback
		       )
	  (vcr-button clim:push-button
		      :id 'vcr
		      :label "VCR"
		      :activate-callback 'shell-button-callback
		      )
	  (rm-button clim:push-button
		     :id 'rm
		     :label "Remove"
		     :activate-callback 'shell-button-callback
		     )
	  (r-button clim:toggle-button
		    :id 'recording
		    :label "Recording"
		    :value-changed-callback 'shell-button-callback
		    )
	  (selected-session clim:text-editor
			    :scroll-bars :horizontal
 			    :min-height '(2 :line)
			    :editable-p nil
			    :text-style *small-fixed-font*
			    :background +light-gray+)
	  (documentation :pointer-documentation
			 :height '(2 :line)
  			 :min-height '(2 :line)
			 :max-height '(2 :line)
			 :background *doc-bg-color*
			 :text-style *doc-font*)
	  )
  ;; default and d2 are exactly the same.  This hacks the resizing behavior
  ;; There's almost no other way to force relayout...
  (:layouts (default
		(clim:vertically (:background +gray+)
 		  menu
		  (clim:horizontally (:text-style *menu-item-font*)
		    (:fill (clim:labelling (:background +gray+
							:label "Planner Status")
			     (clim:vertically (:background +gray+)
			       (:fill graph)
			       (clim:horizontally (:background +gray+)
				 (1/4 info-button)
				 (1/4 vcr-button)
				 (1/4 rm-button) 
				 (1/4 r-button) 
				 ))
			     ))
		    (clim:labelling (:background +gray+
						 :label "Sessions")
		      (clim:scrolling (:scroll-bars :vertical)
		      (clim:make-pane 'clim:list-pane 
				      :name-key #'(lambda (item) 
						    (princ-to-string (car item)))
				      :value-key #'cdr
				      :items (session-submenu)
				      :text-style *session-font*
				      :visible-items 15
				      :background +light-gray+
				      :value-changed-callback 'shell-select-session
				      :scroll-bars :vertical
				      ))))
		  (:fill selected-session)
  		  documentation
		  ))
	    (d2 
		(clim:vertically (:background +gray+)
 		  menu
		  (clim:horizontally (:text-style *menu-item-font*)
		    (:fill (clim:labelling (:background +gray+
							:label "Planner Status")
			     (clim:vertically (:background +gray+)
			       (:fill graph)
			       (clim:horizontally (:background +gray+)
				 (1/4 info-button)
				 (1/4 vcr-button)
				 (1/4 rm-button) 
				 (1/4 r-button) 
				 ))
			     ))
		    (clim:labelling (:background +gray+
						 :label "Sessions")
		      (clim:scrolling (:scroll-bars :vertical)
		      (clim:make-pane 'clim:list-pane 
				      :name-key #'(lambda (item) 
						    (princ-to-string (car item)))
				      :value-key #'cdr
				      :items (session-submenu)
				      :text-style *session-font*
				      :visible-items 15
				      :background +light-gray+
				      :value-changed-callback 'shell-select-session
				      :scroll-bars :vertical
				      ))))
		  (:fill selected-session)
  		  documentation
		  ))))

(clim:define-command-table vcr-tree-tab)
(clim:define-command-table vcr-options-tab :inherit-from (shell-options-tab))

(clim:define-application-frame vcr-window (clim:activity-frame)
  (
   (session :initform nil :initarg :session :accessor vcr-session 
	    :type session)
   (graphics :initform (make-hash-table) :accessor vcr-graphics)
   (interrupt :initform nil :accessor vcr-interrupt)
   (current-window :initform nil :accessor vcr-current)
   (windows  :initform nil :accessor vcr-windows)
   (options :initform nil :initarg :options :accessor vcr-options)
   (tree-desc :initform :full :initarg :tree-desc :accessor vcr-tree-desc)
   (color-mode :initform nil :accessor vcr-color-mode)
   )
  (:menu-bar nil)
  (:command-table (vcr
		   :inherit-from (vcr-tree-tab vcr-options-tab 
					       shell-windows-tab
					       shell-help-tab)
		   :menu (("Windows" :menu shell-windows-tab)
			  ("Tree" :menu vcr-tree-tab)
			  ("Options" :menu vcr-options-tab)
			  ("Help" :menu shell-help-tab))))

  (:command-definer define-vcr-command)
  (:panes (graph :application
		 :text-style *tree-font*
		 :initial-cursor-visibility :inactive
 		 :incremental-redisplay t
		 :borders nil)
	  (rw-button clim:push-button
		     :label *db-l-triangle*
		     :activate-callback 'rewind)
	  (ff-button clim:push-button
		     :label *db-r-triangle*
		     :activate-callback 'fast-forward)
	  (b-button clim:push-button
		    :label *l-triangle*
		    :activate-callback 'backward)
	  (f-button clim:push-button
		    :label *r-triangle*
		    :activate-callback 'forward)
	  (s-button clim:push-button
		    :label *s-rectangle*
		    :activate-callback 'stop)
	  (menu :menu-bar
		:text-style *menu-font*
		:background *menubar-bg-color*
		:foreground *menubar-fg-color*)
	  (tree-desc clim:text-editor
		 :background +light-gray+
		 :width *tree-desc-width*
   		 :height '(1 :line)
		 :editable-p nil
 		 :initial-cursor-visibility :inactive
		 :text-style *session-font*
		 )
	  (documentation :pointer-documentation
			 :height '(1 :line)
			 :max-height '(1 :line)
			 :min-height '(1 :line)
			 :scroll-bars nil
			 :background *doc-bg-color*
			 :foreground *doc-fg-color*
			 :text-style *doc-font*)	  
	  )
  (:layouts (default
		(clim:vertically (:spacing 0)
		  menu
		  graph
		  (clim:horizontally (:background +gray+)
		    rw-button
		    b-button
		    s-button
		    f-button
		    ff-button
		    tree-desc
		    documentation)))))

(clim:define-command-table browser-object-tab)
(clim:define-application-frame browser-window (clim:activity-frame)
  ( 
   (id   :initarg :id   :accessor browser-id)
   (current-item :initarg :item :accessor browser-current-item)
   (stack :initarg :stack :accessor browser-stack)
   (current-stack :initarg :current :accessor browser-current-stack)
   (graphics :initform (make-hash-table) :accessor browser-graphics)
   (options :initarg :options :accessor browser-options) 
   )
  (:command-table (browser
		   :inherit-from (clim:accept-values-pane
				  browser-object-tab 
				  shell-windows-tab
				  shell-help-tab)
		   :menu (("Windows" :menu shell-windows-tab)
			  ("Object" :menu browser-object-tab)
			  ("Help" :menu shell-help-tab))))
  (:command-definer define-browser-command)
  (:menu-bar nil)
  (:panes (graph :application
		 :text-style *small-font*
		 :end-of-page-action :allow
		 :end-of-line-action :allow
		 :initial-cursor-visibility :inactive
		 :borders nil)
	  (left-arrow clim:push-button
		      :label *l-triangle*
		      :activate-callback 'browser-left)
	  (right-arrow clim:push-button
		       :label *r-triangle*
		       :activate-callback 'browser-right)
	  (browser1 :application
		   :scroll-bars :vertical
		    :end-of-line-action :allow
		    :end-of-page-action :allow
		    :initial-cursor-visibility :inactive
		    :text-style *small-font*)
	  (browser2 :application
		    :scroll-bars :vertical
		    :end-of-line-action :allow
		    :end-of-page-action :allow
		    :initial-cursor-visibility :inactive
		    :text-style *small-font*)
	  (browser3 :application
		    :end-of-line-action :allow
		    :end-of-page-action :allow
		    :initial-cursor-visibility :inactive
		    :scroll-bars :vertical
		    :text-style *small-font*)

	  (menu :menu-bar
		:text-style *menu-font*
		:background *menubar-bg-color*
		:foreground *menubar-fg-color*)
	  (documentation :pointer-documentation
			 :height '(1 :line)
			 :max-height '(1 :line)
			 :min-height '(1 :line)
			 :scroll-bars nil
			 :borders nil
			 :text-style *doc-font*)
	  )
  (:layouts (default
		(clim:vertically (:spacing 0 :background +gray+)
		  menu
		  (clim:horizontally ()
		    (1/3 browser1)
		    (1/3 browser2)
		    (1/3 browser3))
		  graph
		  (clim:horizontally ()
		    left-arrow
		    right-arrow
		    documentation)))))

(clim:define-command-table plan-plan-tab)
(clim:define-command-table plan-options-tab)

(clim:define-application-frame plan-window (clim:activity-frame)
  (
   (id   :initarg :id   :accessor plan-window-id)
   (node :initarg :node :accessor plan-window-node :type plan-tree)
   (vcr :initarg :vcr :accessor plan-window-vcr)
   (record :initarg :record :accessor plan-window-record)
   (graphics :initform (make-hash-table) :accessor plan-window-graphics)
   (options :initarg :options :accessor plan-window-options)
   (history-f :initform nil :accessor plan-window-history-f)
   (history-b :initform nil :accessor plan-window-history-b)
   )
  (:menu-bar nil)
  (:command-table (plan
		   :inherit-from (plan-plan-tab plan-options-tab 
				      shell-windows-tab
				      shell-help-tab vcr)
		   :menu (("Windows" :menu shell-windows-tab)
			  ("Plan" :menu plan-plan-tab)
			  ("Options" :menu plan-options-tab)
			  ("Help" :menu shell-help-tab))))

  (:command-definer define-plan-command)
  (:panes (graph :application
		 :text-style *small-font*
		 :end-of-page-action :allow
		 :end-of-line-action :allow
		 :background +light-gray+
		 :initial-cursor-visibility :inactive
		 :incremental-redisplay t
		 :borders nil)
	  (tree :application
 		:incremental-redisplay t
		:initial-cursor-visibility :inactive
		:scroll-bars :horizontal
		:max-height '(20 :line)	; should better be :compute,  but we
					; don't have a display function
		:text-style *tree-font*)
	  (menu :menu-bar
		:text-style *menu-font*
		:background *menubar-bg-color*
		:foreground *menubar-fg-color*)
	  (left-arrow clim:push-button
		      :label *l-triangle*
		      :background +gray+
		      :activate-callback 'plan-window-left)
	  (right-arrow clim:push-button
		       :label *r-triangle*
		       :background +gray+
		       :activate-callback 'plan-window-right)
	  (documentation :pointer-documentation
			 :scroll-bars nil
			 :height '(1 :line)
			 :max-height '(1 :line)
			 :min-height '(1 :line)
			 :borders nil
			 :background *doc-bg-color*
			 :foreground *doc-fg-color*
			 :text-style *doc-font*))
  (:layouts (default
		(clim:vertically (:spacing 0)
		  menu
		  tree
		  (:fill graph)
		  (clim:horizontally ()
		    left-arrow
		    right-arrow
		    documentation)))))


(clim:define-application-frame help-window (clim:activity-frame)
  ()
  (:menu-bar nil)
  (:command-table (help
		   :inherit-from (vcr shell-windows-tab shell-help-tab)
		   :menu (("Windows" :menu shell-windows-tab)
			  ("Help" :menu shell-help-tab))))

  (:command-definer define-help-command)
  (:panes (menu :menu-bar
		:text-style *menu-font*
		:background *menubar-bg-color*
		:foreground *menubar-fg-color*)
	  (text clim:text-editor
		 :background +light-gray+
		 :width '(30 :character)
   		 :min-height '(15 :line)
		 :height '(25 :line)
		 :width '(40 :characters)
		 :editable-p nil
 		 :initial-cursor-visibility :inactive
  		 :scroll-bars :vertical
		 :text-style *session-font*
		 :end-of-line-action :allow
		 :end-of-page-action :allow
		 ))
  (:layouts (default
		(clim:vertically (:spacing 0)
		  menu
		  (clim:scrolling (:scroll-bars :vertical)
				   text)
		  ))))


(defclass s-activity (clim:activity) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Shell startup
(defvar *port* nil)
(defvar *frame-manager* nil)
(defun INITIALIZE-PLAN-DEBUGGER (&key (display nil))
  (setf *enable-vcr* t) ;; enabled by default
  (clim:noting-progress (t  "Bringing up..." nv)
    (clim:note-progress 1 100 nv)
    ;; open the display if it's not the default
    ;; Actually this doesn't have to be motif specific,  but anyway...
    (setf *port* 
      #+:clim-motif (if display	
			(clim:find-port :server-path `(:motif :display
							      ,display))
		      (clim:find-port))
      #-:clim-motif (clim:find-port)
      )
   ;; (setf *vcr-palette* (clim:make-palette *port*))
      
    #+:clim-motif (setf *frame-manager* (clim:find-frame-manager :port *port*))
  
  ;; colors
    (if *pdb-use-colors*
	(unless *colors*
	  (dotimes (i 30)
	    (push (clim:make-rgb-color 0 (+ .2 (* .8 (/ (- 30 i) 30))) 0)
		  *colors*)))
      (unless *colors*
	(dotimes (i 30)
	  (push +light-gray+ *colors*)))
      )
    
  
  ;; preferences
  (read-option-file)

  (install-shell-menus)
  (install-vcr-menus)
  (install-plan-menus)
  (install-browser-menus)
  (run-plan-debugger)
  (values)))


(defvar *shell-activity* nil)

(defmethod clim:start-initial-application-frame ((activity s-activity))
  ;; the first time you start the activity construct the app frame
  ;; (but not before)
  (let ((frame *shell*))
    (setf *shell* 
      ;; The reason for this dumb piece of code is because if you set *shell-height* to below 
      ;; the required space,  CLIM will blow!
      (if (numberp *shell-height*) 
	  (clim:start-application-frame 
	   activity
	   'shell
	   :pretty-name "Plan System Overview"
	   :sessions (when frame (shell-sessions frame))
	   :options (when frame (shell-options frame))
	   :height *shell-height*
	   :width *shell-width*
	   )
	(clim:start-application-frame 
	 activity
	 'shell
	 :pretty-name "Plan System Overview"
	 :sessions (when frame (shell-sessions frame))
	 :options (when frame (shell-options frame))
	 :width *shell-width*)))
    (show-window *shell*)))

(defun RUN-PLAN-DEBUGGER ()
  ;; enable-activity-frames will automatically display all frames
  (cond (*pdb-running* (show-window *shell*))
	(*shell-activity* (CLIM-INTERNALS::ENABLE-ACTIVITY-FRAMES 
			   *shell-activity*)
#+:multiprocessing (setf *pdb-running* (mp:process-run-function 
					"shell" 
					#'clim:run-frame-top-level  *shell-activity*))
#-:multiprocessing (run-frame-top-level *shell-activity*))
	(t (setf *shell-activity* (make-instance 's-activity :auto-select t
						 :initial-frame *shell*))
#+:multiprocessing (setf *pdb-running* (mp:process-run-function 
					"shell" 
					#'clim:run-frame-top-level  *shell-activity*))
#-:multiprocessing (run-frame-top-level *shell-activity*))
	   )
  (values)
  )



;;;;;;;;;;;;;;;;;;;;;;;
;;;  Window opening methods.

(defmacro window-msg (text &key (window *shell*) (style :inform))
  `(clim:notify-user ,window ,text :style ,style 
		     :exit-boxes '((:abort "OK"))))

(defmethod OPEN-DEBUG (session (frame shell))
  (unless (session-tree session)
      (window-msg "Error: No trace saved for this session." :style :error :window frame)
      (return-from open-debug nil))
  (message frame "Opening VCR Window")
  (let ((new-window 
	 (clim:start-application-frame 
	  (clim:frame-activity clim:*application-frame*)
	  'vcr-window
	  :pretty-name "Vcr Window"
	  :session session
	  :height *vcr-height*
	  :width *vcr-width*
	  :options (get-option-set
		    :vcr-window))))
;;;     (setf (clim:frame-palette new-window) *vcr-palette*)
    (push new-window (shell-windows frame))
    (push new-window (shell-debug-windows frame))
    (setf (clim:command-enabled 'com-clear-all new-window) nil)
    (initialize-window new-window)
    (show-window new-window)
    (values)))

(defvar *plan-window-counter* 0)

(defmethod OPEN-PLAN-DEBUG (node (vcr vcr-window) (frame shell))
  (message vcr "Opening Plan Window")
  (let ((new-window (clim:start-application-frame 
		     (clim:frame-activity clim:*application-frame*)
		     'plan-window
		     :vcr vcr
		     :node node
		     :id (incf
			  *plan-window-counter*)
		     :pretty-name (format nil "Plan Window ~A" 
					  *plan-window-counter*)
		     :options (get-option-set
			       :plan-window) 
		     :height *plan-height*
		     :width *plan-width*)))
;;;     (setf (clim:frame-palette new-window) *vcr-palette*)
    (push new-window (shell-windows frame))
    (push new-window (shell-plan-debug-windows frame))
    (push new-window (vcr-windows vcr))
    (push node (plan-window-history-b new-window))
    (setf (clim:command-enabled 'com-clear-all new-window) nil)
    (initialize-window new-window)
    (show-window new-window)
    (clim:with-scaling ((clim:get-frame-pane vcr 'graph) 1/8)
      (draw-window-marker vcr (clim:get-frame-pane vcr 'graph) new-window))
    (values)))

(defmethod OPEN-BROWSER (object (frame shell))
  (let ((new-window (clim:start-application-frame 
		     (clim:frame-activity clim:*application-frame*)
		     'browser-window
		     :item object
		     :stack nil
		     :current nil
		     :height *browser-height*
		     :width *browser-width*
		     )))
    (push new-window (shell-windows frame))
    (initialize-window new-window)
    (setf (clim:command-enabled 'com-clear-all new-window) nil)
    (show-window new-window)
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;  Window closing methods.

(defmethod DELETE-WINDOW ((window browser-window) (frame shell))
  (setf (shell-windows frame) (delete window (shell-windows frame)))
  (clim:frame-exit window)
;;; thread is dead here
  )

(defmethod DELETE-WINDOW ((window help-window) (frame shell))
  (clim:frame-exit window)
;;; thread is dead here
  )

(defmethod DELETE-WINDOW ((window plan-window) (frame shell))
  (let ( (vcr (plan-window-vcr window)) )
    (setf (vcr-windows vcr) (delete window (vcr-windows vcr))) 
    (setf (shell-plan-debug-windows frame)
      (delete window (shell-plan-debug-windows frame)))
    ;; Reclaim the max number
    (when (= (plan-window-id window) *plan-window-counter*)
      (decf *plan-window-counter*))
    (setf (shell-windows frame) (delete window (shell-windows frame)))
    (erase-window-marker vcr (clim:get-frame-pane vcr 'graph) window)
    (clim:frame-exit window)
    ))


(defmethod DELETE-WINDOW ((window vcr-window) (frame shell))
  (setf (shell-windows frame) (delete window (shell-windows frame)))
  (clim:frame-exit window)
  ;; doesn't return
  )



(defmethod CLEAR-WINDOWS ((frame shell))
  (dolist (w (shell-windows frame))
    (clim:disable-frame w)
    )
  (setf (shell-windows frame) nil)
  (setf (shell-debug-windows frame) nil)
  (setf (shell-plan-debug-windows frame) nil)
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Utility functions
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-list-select ((chosen list label) &body body)
  `(let ((,chosen
	  ;; this seems to be a clim bug,  :background is not a keyword in the
	  ;; manual 
	  (clim:accepting-values (t :own-window t :background +gray+
				    :label ,label)
	    (clim:accept (cons 'member ,list)
			 :prompt nil
			 :view `(clim:list-pane-view 
				 :mode :exclusive
				 :scroll-bars ,:vertical
				 :visible-items 10
				 :text-style ,*menu-item-font*))))) 
     (when ,chosen
       ,@body)))


(defun display-scrolled-text (t1 t2 &key (lines 20) &aux dialog)
  (clim:accepting-values  
      (dialog :own-window T  :label "Planning result"
	      :background +cadet-blue+ 
	      :exit-boxes '((:exit "OK"))
	      :resynchronize-every-pass t)
    (clim:present t1 'string 
		  :stream dialog 
		  :view `(clim:text-editor-view
			  :background ,+light-gray+
			  :nlines ,lines
			  :ncolumns 35
			  :text-style ,*small-font*))
    (terpri dialog)
    (clim:present t2 'string 
		  :stream dialog 
		  :view `(clim:text-editor-view
			  :background ,+light-gray+
			  :nlines 10
			  :ncolumns 35
			  :text-style ,*small-font*))))

(defun VCR-DISPLAY-STAT (p s &aux pl st)
  (setf pl 
    (with-output-to-string (pl)
      (display-text-plan p pl)))
  (setf st 
    (with-output-to-string (st)
      (display-plan-stat s st)))
  (display-scrolled-text pl st)
  )

;;; This doesn't work right in clim.  It doesn't set the pointer back to arrow! 
(defun shell-show-busy (busy)  ;; busy = t or nil
;  (setf (clim:sheet-pointer-cursor (clim:frame-top-level-sheet *shell*))
;    (if busy :busy :default))
  )

(defun shell-display-status (msg) 
  (setf (clim:gadget-value (clim:find-pane-named *shell* 'graph))
    msg))

(defmacro with-command-table ((table) &body body)
 `(let ((tab ,table))
    ,@body))

(defmacro inst-menu (&rest args)
  `(clim:add-menu-item-to-command-table tab ,@args 
					:errorp nil
					:text-style 
					*menu-item-font*))

(defun message (frame string)
  (let ((documentation (clim:get-frame-pane frame 'documentation)))
    (format documentation string))
  (values))


(defun reinit ()
  (setf *shell* nil)
  (setf *shell-activity* nil)
  (setf *pdb-running* nil)
  )


