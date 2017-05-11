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

(defmethod initialize-window ((frame browser-window))
  (let ( 
	(graphics (browser-graphics frame))
	(root-item (make-instance 'graphic-browser-item
		     :name "Browse"
		     :object :browser-root
		     :stack nil
		     :submenu #'browser-root-submenu
		     :frame frame
		     :stream (clim:get-frame-pane frame 'browser1)))
	)
    (put-translation graphics root-item :browser-root)
    (setf (browser-stack frame) (list root-item))
    (setf (browser-current-stack frame) (browser-stack frame))))


(defmethod show-window ((frame browser-window))
  (let* (
	 (graph   (clim:get-frame-pane frame 'graph))
	 (stream1 (clim:get-frame-pane frame 'browser1))
	 (stream2 (clim:get-frame-pane frame 'browser2))
	 (stream3 (clim:get-frame-pane frame 'browser3))
	 
	 (current-item  (browser-current-item frame))
	 (current-stack (browser-current-stack frame))
	 (stack         (browser-stack frame))
	 (current-depth (length current-stack))
	 (depth         (length stack))
	 )
    (clim:window-clear graph)
    (clim:window-clear stream1)
    (clim:window-clear stream2)
    (clim:window-clear stream3)
    (cond ((<= current-depth 1)
	   (draw-browser frame stream1 current-stack nil))
	  ((= current-depth 2)
	   (draw-browser frame stream1 (cdr current-stack) (car current-stack))
	   (draw-browser frame stream2 current-stack nil))
	  ((>= current-depth 3)
	   (draw-browser frame stream1 (cddr current-stack) (cadr current-stack))
	   (draw-browser frame stream2 (cdr current-stack) (car current-stack))
	   (draw-browser frame stream3 current-stack (if (> depth current-depth)
							 (nth (- depth current-depth 1) stack)
						       nil))))
    (when (not (null current-item))
      (cond ((null (graphic-browser-item-printer current-item))
	     (format graph "~A" (graphic-object current-item)))
	    (T
	     (funcall (graphic-browser-item-printer current-item)
		      graph
		      (graphic-object current-item))))))
  (values))

(defmethod print-window ((frame browser-window) stream)
  (let* (
	 (current-item  (browser-current-item frame))
	 )
    (when (not (null current-item))
      (cond ((null (graphic-browser-item-printer current-item))
	     (format stream "~A" (graphic-object current-item)))
	    (T
	     (funcall (graphic-browser-item-printer current-item)
		      stream
		      (graphic-object current-item))))))
  (values))

(defun draw-browser (frame stream stack current)
  (let ( (item (car stack)) )
    (funcall (graphic-browser-item-submenu item)
	     frame stream stack current)))

(defun browse (frame stream stack object current &key name submenu printer)
  (labels
      (
       (make-item ()
	   (make-instance 'graphic-browser-item
	     :name    name
	     :object  object
	     :stack   stack
	     :submenu submenu
	     :frame   frame
	     :stream  stream
	     :printer printer))
       (output-graphic (graphic)
	 (clim:with-output-as-presentation
	     (stream graphic 'graphic-browser-item)
	   (clim:with-drawing-options
	       (stream :text-face (when (eq graphic current) :bold))
	     (format stream "~A~%" name))))
       )
    (let* (
	   (graphics (browser-graphics frame))
	   (items (get-translation graphics stack))
	   )
      (cond ((null items)
	     (let (
		   (items (make-hash-table))
		   (graphic (make-item))
		   )
	       (put-translation items object graphic)
	       (put-translation graphics stack items)
	       (output-graphic graphic)))
	    (T
	     (let ( (graphic (get-translation items object)) )
	       (cond ((null graphic)
		      (let ( (graphic (make-item)) )
			(put-translation items object graphic)
			(output-graphic graphic)))
		     (T
		      (setf (graphic-browser-item-stack graphic) stack
			    (graphic-stream graphic) stream)
		      (output-graphic graphic)))))))))

(defun browser-root-submenu (frame stream stack current)
  (browse frame stream stack :browse-domains current
	   :name "Domains"
	   :submenu #'domain-submenu)
  (browse frame stream stack :browse-problems current
	  :name "Problems"
	  :submenu #'problem-submenu)
  (browse frame stream stack :browse-sessions current
	  :name "Sessions"
	  :submenu #'browse-sessions-submenu)
  )

(defun domain-submenu (frame stream stack current)
  (dolist (domain (get-domains))
    (browse frame stream stack domain current
	    :name (format nil "~A" (get-domain-name domain))
	    :submenu #'domain-items-submenu
	    :printer #'print-domain)))

(defun domain-items-submenu (frame stream stack current)
  (browse frame stream stack :domain-operators current
	  :name "Operators"
	  :submenu #'domain-operators-submenu)
  (browse frame stream stack :domain-axioms current
	  :name "Axioms"
	  :submenu #'domain-axioms-submenu)
  (browse frame stream stack :domain-facts current
	  :name "Facts"
	  :submenu #'domain-facts-submenu)
  (format stream "-------~%")
  (browse frame stream stack :domain-problems current
	  :name "Problems"
	  :submenu #'domain-problem-submenu))

(defun print-domain (stream domain)
  (format stream "Domain ~A~%" (get-domain-name domain))
  (format stream "Number of operators:~A~%" (length (get-domain-operators domain)))
  (format stream "Number of axioms:~A~%" (length (get-domain-axioms domain)))
  (format stream "Number of facts:~A~%" (length (get-domain-facts domain))))


;;;;;;;;;;;;;;;;;;;;;
;;; Operators

(defun domain-operators-submenu (frame stream stack current)
  (let* (
	 (domain (graphic-object (cadr stack)))
	 (operators (get-domain-operators domain))
	 )
    (cond ((not (null operators))
	   (dolist (operator operators)
	     (browse frame stream stack operator current
		     :name (format nil "~A" (get-bound-clause operator))
		     :printer #'print-operator)))
	  (T
	   (format stream "<nil>")))))

(defun print-operator (stream operator)
  (format stream "~A~%~%" (get-bound-clause operator))
  (format stream "Preconditions:~%")
  (dolist (p (get-step-precond operator))
    (format stream "  ~S~%" p))
  (format stream "Effects:~%")
  (dolist (e (get-step-effects operator))
    (format stream "  [~a:~a->~a]~%"
	    (get-effect-id e)
	    (get-effect-precond e)
	    (get-effect-add e))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms

(defun domain-axioms-submenu (frame stream stack current)
  (let* (
	 (domain (graphic-object (cadr stack)))
	 (axioms (get-domain-axioms domain))
	 )
    (cond ((not (null axioms))
	   (dolist (axiom axioms)
	     (browse frame stream stack axiom current
		     :name (format nil "~A" (get-effect-id axiom))
		     :printer #'print-axiom)))
	  (T
	   (format stream "<nil>")))))

(defun print-axiom (stream axiom)
  (format stream "Axiom ~a:~%" (get-effect-id axiom))
  (format stream "   ~a->~a~%"
	  (get-effect-precond axiom)
	  (get-effect-add axiom)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Facts

(defun domain-facts-submenu (frame stream stack current)
  (let* (
	 (domain (graphic-object (cadr stack)))
	 (facts (get-domain-facts domain))
	 )
    (cond ((not (null facts))
	   (dolist (fact facts)
	     (browse frame stream stack fact current
		     :name (format nil "~A" (get-fact-condition fact)))))
	  (T
	   (format stream "<nil>")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problems

(defun domain-problem-submenu (frame stream stack current)
  (let*
      (
       (domain (graphic-object (cadr stack)))
       (problems (remove-if-not #'(lambda (p)
				    (eq (get-problem-domain p)
					(get-domain-name domain)))
				(get-problems)))
       )
    (cond ((not (null problems))
	   (dolist (problem problems)
	     (browse frame stream stack problem current
		     :name (format nil "~A" (get-problem-name problem))
		     :submenu #'problem-items-submenu)))
	  (T
	   (format stream "<nil>")))))

(defun problem-submenu (frame stream stack current)
  (dolist (problem (get-problems))
    (browse frame stream stack problem current
	    :name (format nil "~A" (get-problem-name problem))
	    :submenu #'problem-items-submenu)))

(defun problem-items-submenu (frame stream stack current)
  (let* (
	 (problem (graphic-object (car stack)))
	 (domain-name (get-problem-domain problem))
	 (domain (find domain-name (get-domains) :key #'get-domain-name))
	 )
    (browse frame stream stack (get-problem-inits problem) current
	  :name "Inits")
    (browse frame stream stack (get-problem-goal problem) current
	    :name "Goal")
    (browse frame stream stack (or domain domain-name) current
	    :name "Domain"
	    :submenu (if (not (null domain))
			 #'domain-items-submenu
		       nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sessions

(defun browse-sessions-submenu (frame stream stack current)
  (dolist (session (shell-sessions *shell*))
    (browse frame stream stack session current
	    :name (format nil "~A" (get-problem-name (session-name session)))
	    :submenu #'session-items-submenu)))

(defun session-items-submenu (frame stream stack current)
  (let* (
	 (session (graphic-object (car stack)))
	 )
    (browse frame stream stack (session-name session) current
	    :name "Problem"
	    :submenu #'problem-items-submenu)
    (browse frame stream stack (session-result session) current
	    :name "Result")
    (browse frame stream stack (session-sc session) current
	    :name "Search Control"
	    :submenu (if (not (null (session-sc session)))
			 #'sc-items-submenu))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search Control

(defun sc-items-submenu (frame stream stack current)
  (let* (
	 (sc (graphic-object (car stack)))
	 (rules (get-sc-rules sc))
	 )
    (cond ((not (null rules))
	   (dolist (rule (get-sc-rules sc))
	     (browse frame stream stack rule current
		     :name (format nil "~A" (get-sc-rule-name rule))
		     :printer #'print-sc-rule)))
	  (T
	   (format stream "<nil>")))))

(defun print-sc-rule (stream rule)
  (format stream "Rule: ~A~%" rule))
