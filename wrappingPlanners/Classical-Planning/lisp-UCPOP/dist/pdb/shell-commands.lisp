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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Session commands

(defmethod locate-session (session (frame shell))
  (let ( (window (find session (shell-debug-windows frame)
		       :key #'vcr-session)) )
    (if window (clim:raise-frame window)
      (open-debug session frame))))

(defmethod delete-session (session (frame shell))
  (setf (shell-sessions frame)
    (delete session (shell-sessions frame)))
  (dolist (w (shell-debug-windows frame))
    (when (eq session (vcr-session w))
      (delete-window w frame)))
  (setf (shell-current-session *shell*) nil)
  (setf (clim:gadget-value (clim:find-pane-named *shell* 'selected-session)) "")
  (show-window *shell*)
  )

(defmethod clear-sessions ((frame shell))
  (clear-windows frame)
  (setf (shell-sessions frame) nil)
  (setf (shell-current-session *shell*) nil)
  (show-window *shell*)
  )

(defun session-submenu (&aux string)
  (when *shell*
    (mapcar #'(lambda (s) 
		(setf string (get-problem-name (session-name s)))
		(cons (subseq string 0 (min (length string) *session-name-limit*))
		      s))
	    (shell-sessions *shell*))))

(define-shell-command (com-locate-session) ((session session))
  (locate-session session clim:*application-frame*))

(clim:define-presentation-to-command-translator locate-translator
    (session com-locate-session shell :gesture :select
	     :documentation "Find/open"
	     :pointer-documentation 
	     ((object stream) . ((cond ((find object (shell-debug-windows
						      clim:*application-frame*)
					      :key #'vcr-session) 
					(format stream "Find Window"))
				       (T (format stream "Open Window"))))))
  (object) 
  `(,object))


(defun install-shell-menus ()
  ;; debug menu
  (with-command-table ('shell-debug-tab)
    (inst-menu "New Session" :command '(com-debug :new)
	       :documentation "Start Planning")
    (inst-menu "New VCR Window" :command '(com-debug :open)
	       :documentation "Open Session")
    (inst-menu "New Browser" :command '(com-debug :browse)
	       :documentation "Open Browser" )

    (inst-menu "dd1" :divider :line)
    
    (inst-menu "Remove Session" :command '(com-debug :delete)
	       :documentation "Remove Planning Session")
    (inst-menu "Remove All Sessions" :command '(com-debug :clear)
	       :documentation "Remove All Planning Sessions"))
  ;; options menu
  (with-command-table ('shell-options-tab)
    (inst-menu "Reset Options" :command '(com-options :reset)
	       :documentation "Read in options from .plandebugrc")
    (inst-menu "Save Options" :command '(com-options :save)
	       :documentation "Save display option in .plandebugrc")
    )
  (with-command-table ('shell-windows-tab)
    (inst-menu "Refresh" :command '(com-window :refresh)
	       :documentation "Refresh this window")
    (inst-menu "Find" :command '(com-window :find)
	       :documentation "Pop window to the top")
    (inst-menu "Remove all" :command '(com-clear-all)
	       :documentation "Remove all windows")
    (inst-menu "wd1" :divider :line)
    (inst-menu "Quit" :command 'com-quit
	       :documentation "Exit this window"))
  (with-command-table ('shell-help-tab)
    (inst-menu "About..." :command '(com-shell-help :about)
	       :documentation "About PDB")
    (inst-menu "dd2" :divider :line)
    (inst-menu "Overview..." :command '(com-shell-help :overview)
	       :documentation "PDB overview")
    (inst-menu "On Shell..." :command '(com-shell-help :shell)
	       :documentation "Help on Plan System Overview window")
    (inst-menu "On VCR..." :command '(com-shell-help :vcr)
	       :documentation "Help on VCR/Plan Debug window")
    (inst-menu "On Browser..." :command '(com-shell-help :browser)
	       :documentation "Help on browser window")
    (inst-menu "On Plan Window..." :command '(com-shell-help :plan-window)
	       :documentation "Help on Plan window"))
  )

;;; generic handle-all for debug menu
(define-shell-command com-debug ((key t))
  (let ((frame clim:*application-frame*))
    (when key
      (case key
	(:new
	 (unless is-xii
	   (multiple-value-bind
	       (problem limit controller-f search-f debug debug-firings)
	       (plan-dialog)
	     (declare (ignore debug-firings))
	     (when problem
	       (enable-vcr)
	       (ucpop-plan problem limit controller-f search-f debug)
	       (reset-vcr)))))
	(:browse
	 (unless is-xii
	   (open-browser nil clim:*application-frame*)))
	(:open   (with-list-select (chosen (session-submenu) "Open session")
		   (open-debug (cdr chosen) frame)))
	(:delete (with-list-select (chosen (session-submenu) "Delete session")
		   (delete-session (cdr chosen) frame)))
	(:clear  (clear-sessions frame))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Options

(define-shell-command com-options ((key t))
  (if (eq key :reset)
      (read-option-file)
    (write-option-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window commands

(defmethod print-window-menu-item (frame stream)
  (format stream "~A" (clim:frame-pretty-name frame)))

(defmethod print-window-menu-item ((frame vcr-window) stream)
  (format stream "VCR Window: ~A"
	  (get-problem-name (session-name (vcr-session frame)))))

(defmethod print-window-menu-item ((frame plan-window) stream)
  (format stream "             Plan Window ~A" (plan-window-id frame)))


(defmethod make-window-menu ((frame shell))
  (let (
	(windows (shell-windows frame))
	(debug (shell-debug-windows frame))
	(plan-debug (shell-plan-debug-windows frame))
	)
    (nconc
     (mapcan #'(lambda (d)
		 (cons d (copy-list (vcr-windows d))))
	     debug)
     (remove-if #'(lambda (w) (or (member w debug) (member w plan-debug)))
		windows))))

(define-shell-command com-window ((key t))
  (let ((frame clim:*application-frame*))
    (case key
      (:refresh (show-window frame))
      (:find (with-list-select (tmp (make-window-menu *shell*) "Find window")
	       (clim:raise-frame tmp)))
      )))

(define-shell-command com-clear-all ()
  (let ((frame clim:*application-frame*))
    (clear-windows frame)))

(define-shell-command (com-quit) ()
  (exit-frame (CLIM-INTERNALS::ACTIVITY-ACTIVE-FRAME *shell-activity*)))

(defmethod exit-frame ((frame shell))
  (let ((activity *shell-activity*)
	(process *pdb-running*))
    ;; CLIM Undocumented: if :exit is "OK",  return t, else :abort is nil.
    (setf *pdb-running* nil)
    (CLIM-INTERNALS::DISABLE-ACTIVITY-FRAMES activity)
#+:multiprocessing (mp:process-kill process)
    (clim:activity-quit *shell-activity*)
    ))


(defun shell-select-session (tf session)
  (declare (ignore tf))
  (setf (shell-current-session *shell*) session)
  (setf (clim:gadget-value (clim:find-pane-named *shell* 'selected-session))
    (get-problem-name (session-name session)))
  (show-planner *shell*))


(defun shell-button-callback (button &optional value)
  (let ((id (clim:gadget-id button))
	(session (shell-current-session *shell*))
	(frame *shell*)
	)
    (unless (shell-sessions frame)
      (window-msg "No session in shell" :window *shell* :style
		  :error)
      (return-from shell-button-callback nil))
    (unless (or (eq id 'recording) session)
      (window-msg "Please select a session first" :window *shell* :style
		  :error) 
      (return-from shell-button-callback nil))
    (case id
      (info (vcr-display-stat (session-solution session) (session-result session)))
      (vcr  (open-debug session frame))
      (rm   (delete-session session frame))
      (recording (setf vcr:*enable-vcr* value)))
    ))


;;
;; NOTE: we should use CLOS to handle typing, since all windows
;; use the same code
(define-shell-command com-options ((type t))
  (let ((frame clim:*application-frame*))
    (case type
      (:vcr (options-dialog (vcr-options frame)))
      (:plan (options-dialog (plan-window-options frame))))
    (layout-window frame)
    (show-window frame)))

(defmethod exit-frame ((frame help-window))
  (setf *help-window* nil)
  (delete-window frame *shell*)
  )

(define-shell-command com-shell-help ((type t))
  (unless *help-window*
    (setf *help-window* (clim:start-application-frame 
			 (clim:frame-activity clim:*application-frame*)
			 'help-window
			 )))
  (setf (clim:command-enabled 'com-clear-all *help-window*) nil)
  (setf (clim:gadget-value (clim:find-pane-named *help-window* 'text)) 
    (cadr (assoc type *pdb-help-text*)))
  (clim:raise-frame *help-window*))


