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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Browser navigation

(defun select-item (frame stream item)
  (declare (ignore stream))
  (let (
	(item-stack (graphic-browser-item-stack item))
	)
    (when (and (not (null (graphic-browser-item-submenu item)))
	       (not (member item (browser-current-stack frame))))
      (setf (browser-stack frame)
	(cons item item-stack))
      (setf (browser-current-stack frame)
	(browser-stack frame)))
    (when (not (symbolp (graphic-object item)))
      (setf (browser-current-item frame) item))
    (show-window frame)))

;;; When you click on a browser item
(define-browser-command com-select-item ((item 'graphic-browser-item))
  (select-item (graphic-frame item) (graphic-stream item) item))

(define-browser-command com-inspect-item ((item 'graphic-browser-item))
  #+:composer(when *composer-started* (composer:winspect (graphic-object item)))
  )

;;; The translator
(clim:define-presentation-to-command-translator select-an-item
    (graphic-browser-item com-select-item browser :gesture :select 
			   :documentation "Select Item")
  (object)
  `(,object))

(clim:define-presentation-to-command-translator inspect-an-item
    (graphic-browser-item  com-inspect-item browser :gesture :describe
			   :documentation "Inspect Item") 
  (object)
  `(,object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus

(define-browser-command com-bobject ((choice t))
  (let* ((frame clim:*application-frame*)
	 (current (browser-current-item frame)))
    (when (and choice
	       current)
      (case choice
	(:inspect
	 #+:composer(composer:winspect (graphic-object current))
	 )
	(:print
	 (let ( (filename (file-dialog)) )
	   (when filename
	     (let ( (stream (open filename
				  :direction :output
				  :if-exists :overwrite
				  :if-does-not-exist :create)) )
	       (cond (stream
		      (print-window frame stream)
		      (close stream))
		     (T
		      nil))))))))))


(defun browser-left (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*)
	 (current-stack (browser-current-stack frame)))
    (when (> (length current-stack) 3)
      (setf (browser-current-stack frame)
	(cdr current-stack))
      (show-window frame))))


(defun browser-right (button)
  (declare (ignore button))
  (let* ((frame clim:*application-frame*)
	 (current-stack  (browser-current-stack frame))
	 (stack          (browser-stack frame))
	 (current-length (length current-stack)) 
	 (stack-length   (length stack))
	 )
    (when (< current-length stack-length)
      (setf (browser-current-stack frame)
	(nthcdr (- stack-length current-length 1) stack))
      (show-window frame))))

(defun install-browser-menus ()
  (with-command-table ('browser-object-tab)
    (inst-menu "Inspect" :command '(com-bobject :inspect)
	       :documentation "Inspect current object")
    (inst-menu "Print" :command '(com-bobject :print)
	       :documentation "Print current object")
    ))

(defmethod exit-frame ((frame browser-window))
  (delete-window frame *shell*))
