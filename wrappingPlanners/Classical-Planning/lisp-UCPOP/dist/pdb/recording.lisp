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


(defvar *show-status* nil)

(defvar *tape* nil)			; A SNLP run trace
(defvar *ranks* nil)                    ; Rank trace
(defvar *firings* nil)			; firings

(defvar *max-rank* 0)			; Keep track of rank range 
(defvar *min-rank* 0)


;;; turning vcr on and off.
(defun ENABLE-VCR ()
  (setf *enable-vcr* t))

(defun RESET-VCR ()
  (setf *tape*     nil
	*ranks*    nil
	*firings*  nil
	*max-rank* 0
	*min-rank* 0))

(defun DISABLE-VCR ()
  (setf *enable-vcr* nil))


;;; Record scr firings
(defun VCR-ADD-FIRINGS (node firings)
  (when *enable-vcr*
    (push (cons node firings) *firings*)))

;;;  Record the rank of a single node
(defun VCR-RECORD-RANK (node rank)
  (when *enable-vcr*
    (push (cons node rank) *ranks*)))


;;; new session
(defun VCR-NEW-SESSION (problem sc search-f rank-f)
  (when *enable-vcr*
    (let ( (session (make-instance 'session
		      :problem  problem
		      :limit    (get-search-limit)
		      :sc       sc
		      :search-f search-f
		      :rank-f   rank-f
		      :tape     nil
		      :tree     nil
		      :result   nil)) )
      (push session (shell-sessions *shell*)))))

;;; switch sessions
(defun VCR-SWITCH-SESSIONS (problem)
  (when *enable-vcr*
    (let ((new-session (find problem (shell-sessions *shell*) 
			     :key #'session-name)) )
      (setf (shell-current-session *shell*) new-session))))

;;; end session
(defun VCR-END-SESSION (problem &optional stat plan)
  (when *enable-vcr*
    (let ( (current-session (shell-current-session *shell*)) )
      (when (and current-session
                 (eq problem (session-name current-session)))
        (unless *tape* 
          (window-msg "Error: No trace saved for this session." :style :error :window *shell*)
          (reset-vcr)
          (return-from vcr-end-session nil))
        (multiple-value-bind
            (tree size max-rank min-rank)
            (compute-tree *tape* *ranks* *firings*
                          :nodes (session-nodes current-session))
          (setf (session-max-rank current-session) max-rank)
          (setf (session-min-rank current-session) min-rank)
          (setf (session-nodes current-session) nil)
          (setf (session-size current-session) size)
          (setf (session-tape current-session) *tape*)
          (setf (session-tree current-session) tree)
          (setf (session-result current-session) stat)
          (setf (session-solution current-session) plan)
          (reset-vcr)
          (setf (shell-current-session *shell*) current-session)
          )))))

(export '(enable-vcr
	  disable-vcr
	  vcr-add-firings
	  vcr-record-rank
	  vcr-new-session
	  vcr-switch-sessions
	  vcr-end-session
	  ))
