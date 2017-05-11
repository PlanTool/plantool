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
(use-package 'vext)

(export '(VCR-FRAME
	  VCR-RECORD-RANK
	  PDB-NEW
	  PDB-LOAD
	  PDB-END
	  PDB-RECORD
	  PDB-STOP
	  PDB-SETUP
	  PDB-DISPLAY
	  PDB-SAVE
	  PDB-RESTART
	  ))
	  
(defun VCR-FRAME (parent flaw children)
  (when (and *enable-vcr* *vcr-recording*)
    (when *vcr-record-firings*
      (vcr-add-firings parent (rule-net:dump-firings)))
    (when *show-status* (show-planner *shell*))
    (push (list parent flaw children) *tape*)))

;;; --- Higher level PDB functions to be called to start search

(defun PDB-NEW (ps &key sc (search-f 'bestf-search))
  (when *vcr-recording*
    (vcr-new-session
     ps
     sc
     search-f
     (unless sc (get-problem-rank-f ps)))))

(defun PDB-LOAD (ps)
  (when *vcr-recording*
    (vcr-switch-sessions ps)))

(defun PDB-END (ps &optional stat plan)
  (when *vcr-recording*
    (vcr-end-session ps stat plan)))

(defun PDB-RECORD ()
  (setf *vcr-recording* t)
  (when *vcr-record-firings*
    (rule-net:trace-rule)
    (rule-net:collect-firings))
  (enable-vcr))

(defun PDB-STOP ()
  (setf *vcr-recording* nil)
  (when *vcr-record-firings*
    (rule-net:ignore-firings)
    (rule-net:untrace-rule))
  (disable-vcr))


(defun PDB-SETUP (&key display (composer t))
  (format t "PDB is coming up,  please wait...~%")
  (reinit)
#+:composer  (if (and (not (wt:composer-initialized-p)) composer)
                 (progn (format t "Starting composer...~%")
                        (wt:start-composer)
                        (setq *composer-started* t))
               (setq *composer-started* t))
  (when *pdb-startup-hook* (funcall *pdb-startup-hook*))
  (initialize-plan-debugger :display display)
  nil)

(defun PDB-DISPLAY (&optional (display nil))   ;;display is for compatibility
  (declare (ignore display))
  (run-plan-debugger)
  nil)

(defun PDB-SAVE ()
  (when (and *vcr-recording* (shell-current-session *shell*))
    (vcr-end-session (session-name (shell-current-session *shell*)))))

;;; If pdb crashes, use this function to restart it.
(defun PDB-RESTART ()
  (setf *shell-activity* nil)
  (setf *shell* nil)
  (pdb-setup))
