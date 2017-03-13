;;;; File: demo-tf-support.lsp
;;; Contains: Support code for Web demos using standard demo TF
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Mon Sep 15 20:25:47 1997 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

;;; An assumption here is that the httpd is configured so that any
;;; executable file in a CGI directory can be executed.  They don't
;;; have to end in ".cgi" (for instance).  Typically (at least),
;;; such configurations don't let you use the same base URL for both
;;; executables and non-executables.  So if the output of something
;;; run via the CGI contains a relative URL, it should refer to another
;;; executable.  This is a fairly minor restriction in practice, since
;;; that other executable can just copy out whatever file it likes.
;;;
;;; The URL to plan for task T in domain D is "plan-for-demo-tf/D/T"
;;; The task name, T must begin with "task_".
;;;
;;; "plan-for-demo-tf" is a shell script that runs O-Plan with a
;;; parameter that tells O-Plan to load "demo-tf-support" (the file
;;; you're reading now, or a compiled version of it).  The script has
;;; to be in a CGI area.  When it's run, the remainder of the URL,
;;; ie "/D/T", is in the environment variable PATH_INFO.
;;;
;;; The URL is, obviously, a relative URL.  There's a full URL that
;;; works, of course, but it will be different for different sites.
;;; So the idea is to make the relative URL work.
;;;
;;; To make the relative URL "plan-for-demo-tf/D/T" work, the text that
;;; contains the URL has to come from something in the same CGI directory
;;; that contains "plan-for-demo-tf".  This can be a script that copies
;;; out a file that contains the relative URL.  (See above.)
;;; 
;;; The rest of the explanation is probably best given by example.
;;; For this, we'll use the "Standard TF demonstrations" as set up
;;; at the AIAI.  The "index.html" that introduces the O-Plan demos
;;; contains a URL that looks like this:
;;;
;;;    /cgi-bin/.../standard-examples.cgi
;;;
;;; "standard-examples.cgi" is a shell script that looks like this:
;;;
;;;    #!/bin/sh
;;;
;;;    echo Content-type: text/html
;;;    echo
;;;
;;;    cat .../standard-examples.html
;;;
;;; "standard-examples.html" contains relative "plan-for-demo-tf" URLs.
;;;

(defparameter *demo-tf-planning-time-limit* (* 3 60)) ;3 minutes real time

(set-parameter :new-psgraph t)
; (set-parameter :psgraph-all-links t)
; (set-parameter :psgraph-all-nodes t)

(defun demo-tf ()
  (with-web-environment "demo-tf"
    (multiple-value-bind (dom task) (get-path-info-domain-and-task)
      (set-parameter :oplan-tf-dir
	(web-demo-filename "standard-tf"))
      (html-line "<HR>")
      (html-print-oplan-greeting)
      (if (prog1 (html-time (plan-for-demo-tf dom task))
	    (html-line "<HR>"))
	  (report-demo-tf-success dom task)
	(report-demo-tf-failure dom task)))))

(defun get-path-info-domain-and-task ()
  ;; PATH_INFO will be /domain/task
  (let ((parts (get-path-info)))
    (values
      (first parts)			;s.b. domain
      (second parts))))			;s.b. task

(defun plan-for-demo-tf (dom task)
  (with-timeout *demo-tf-planning-time-limit*
    (handler-case
        (plan-for dom task)
      (timeout ()
	(error "Planning took more than the time limit of ~S seconds"
	       *demo-tf-planning-time-limit*)))))


;;; Success results

(defun report-demo-tf-success (dom task)
  (html-tag-line "H2" "~@(~A: ~A~)" dom task)
  (html-report-plan-statistics)
  (report-demo-tf-results dom task)
  (html-anchor (web-demo-url "output-formats.html")
    "Explain outputs")
  (html-line "<BR>")
  (web-mail-comment-link)
  (web-note-success))

(defun report-demo-tf-results (dom task)

  (html-paragraph
    (html-line "Results:")
    (html-block "UL"

      ;; The TF file

      (html-item "LI"
	(html-anchor (web-demo-url (concat-string "standard-tf/" dom ".tf"))
		     "The TF file"))

      (html-standard-result-links dom task
	'(:psgraph :narrative :world-2)))))


;;; Failure results

(defun report-demo-tf-failure (dom task)

  (html-tag-line "H2" "~@(~A: ~A~)" dom task)

  (html-paragraph
    (html-line "No plan is possible."))

  (html-report-plan-statistics)

  (html-paragraph
    (html-line "Referece links:")
    (html-block "UL"

      ;; The TF file

      (html-item "LI"
	(html-anchor (web-demo-url (concat-string "standard-tf/" dom ".tf"))
		     "The TF file"))))

  (web-mail-comment-link)
  (web-note-failure "no plan was possible"))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
