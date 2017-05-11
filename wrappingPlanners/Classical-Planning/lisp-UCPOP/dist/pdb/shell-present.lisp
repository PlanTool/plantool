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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; shell-present.lisp: drawing routines for shell
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package 'vcr)

(defmethod show-window ((frame shell))
  (let (
	(layout (clim:frame-current-layout frame))
	)
    (show-planner frame)
    ;; shows the button
    (setf (clim:gadget-value (clim:find-pane-named *shell* 'r-button))
      *enable-vcr*)
   (if (eql layout 'default)
	(setf (clim:frame-current-layout frame) 'd2)
      (setf (clim:frame-current-layout frame) 'default))
   (values)
    ))

(defmethod show-sessions ((frame shell) stream)
  (clim:accept (cons 'member (session-submenu)) 
	       :prompt nil
	       :stream stream
	       :view `(clim:list-pane-view 
 		       :mode :exclusive
 		       :scroll-bars ,:vertical
		       :visible-items 10
		       :text-style ,*menu-item-font*)))

(defun show-planner (frame &aux (pane  (clim:find-pane-named frame 'graph)))
  (if (shell-current-session frame)
    (let* ((session (shell-current-session frame))
	   )
      (setf (clim:gadget-value pane)
	(with-output-to-string (string)
	  (format string "SC: ~A~%" (if (null (session-sc session)) 
					"No" "Yes"))
	  (unless (session-sc session)
	    (format string "SRCH: ~A~%RANK: ~A"
		    (session-search-f session)
		    (session-rank-f session)))
	  (unless is-xii (display-plan-stat (session-result session) string))
	  (format string "~%~%Click on Info button to see the complete plan and statistics.")
	  )))
    (setf (clim:gadget-value pane) "")
    ))


(defmethod layout-window ((shell shell))
  ())
