;;;; File: gpdt-support.lsp
;;; Contains: Support code for the "go places and do things" Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1997
;;; Updated: Thu Jun  3 18:03:07 1999 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

(in-package :oplan)


;;;; CGI entry point

(defun gpdt-cgi ()
  (coa-matrix-cgi "gpdt"))


;;;; Parameters

(defparameter *max-objectives* 5)


;;; Query-arg types

(define-query-arg-name-type :city (|| abyss barnacle calypso delta))

(define-query-arg-name-type :weather (clear rain storm))

(define-query-arg-name-type :road-status (open landslide flooding))

(define-query-arg-type :action (argd argval)
  (funcall (get :text 'query-arg-converter) argd argval))


;;; There are no task parameters

(defparameter *task-parameters* nil)


;;; Situation-definition parameters

(defparameter *situation-definition-parameters*
  '((:weather                (:weather)     "weather")
    (:road-delta-abyss       (:road-status) "road status")
    (:road-abyss-barnacle    (:road-status) "road status")
    (:road-barnacle-calypso  (:road-status) "road status")
    (:road-calypso-delta     (:road-status) "road status")))


;;; COA-definition parameters

;;; /\/: The *max-objectives* should not be built-in.

(defun gen-coa-objective-parameters (n)
  `((,(coa-arg-key n "ACTION") (:text)       "objective action")
    (,(coa-arg-key n "CITY")   (:city)       "objective city")))

(defparameter *coa-definition-parameters*
  (append '((:n    (:int 0)    "COA number"))
	  (loop for i from 1 to *max-objectives*
		append (gen-coa-objective-parameters i))
	  *situation-definition-parameters*))

(defun properly-defined-objective-p (coa objective-number)
  (not (or (equal (coa-arg coa objective-number :action) "")
	   (eq (coa-arg coa objective-number :city) '||))))


;;;; Initialization

(advice+ 'initialize-session-data :gpdt-demo
  #'(lambda (previous)
      #'(lambda ()
	  (funcall previous)
	  (setq *task-query-args*
		(default-situation-args)))))

(defun default-situation-args ()
  (alist->hash-table
    '((:weather                . clear)
      (:road-delta-abyss       . open )
      (:road-abyss-barnacle    . open )
      (:road-barnacle-calypso  . open )
      (:road-calypso-delta     . open ))
    :test #'eq))


;;;; Matrix page items

;;; Buttons

(defun write-matrix-page-buttons ()
  (html-button-bar
   `(("Help"               ,(web-demo-url "gpdt-help.html"))
     ("Map"                ,(web-demo-url "image/pacifica-map.gif"))
     ("View TF file"       ,(web-demo-url "gpdt.tf"))
     ("Select evaluations" ,(path-action-url :select-evals))
;    ("Select COAs"        ,(path-action-url :select-coas))
;    ("New COA"            ,(path-action-url :new-coa))
     )))


;;;; COA description table

(defun write-coa-description-table ()
  (when (some #'coa-parameters *coas*)
    (html-tag-line "h2" "COA objectives")
    (html-aiai-table ("width=100%")
      ;; Header row
      (html-item "tr"
        (html-item "th" "COA")
	(loop for i from 1 to *max-objectives* do
	   (html-item "th"
	     (html-line "Objective ~D" i))))
      ;; COA description rows
      (dolist (coa *coas*)
	(when (coa-parameters coa)
          (html-item "tr"
            (html-item "td align=center" (html-format "~D" (coa-number coa)))
	    (loop for i from 1 to *max-objectives* do
	       (if (properly-defined-objective-p coa i)
		   (html-item "td align=left"
		     (html-line "~A ~A"
				(coa-arg coa i :action)
				(coa-arg coa i :city)))
		 (html-empty-td))))))))
  (write-coa-situation-table :include-default t))


;;;; COA situation table

(defun write-coa-situation-table (&key include-default)
  (html-tag-line "h2" "COA initial situations")
  (html-aiai-table ("width=100%")
    ;; A row containing the query arg names as headings
    (html-item "tr"
      (html-item "th" "COA")
      (dolist (argd *situation-definition-parameters*)
	(html-item "th align=center"
          (html-format "~A" (keyword->text (q-arg-name argd))))))
    ;; If requested, a row giving the defaul values.
    (when include-default
      (html-item "td align=center"
        (html-anchor (path-action-url :def-situation) "Default"))
      (dolist (argd *situation-definition-parameters*)
	(html-item "td align=center"
	  (html-format "~A" (task-arg (q-arg-name argd))))))
    ;; One row per COA to give the parameter values.
    (dolist (coa *coas*)
      (when (coa-parameters coa)
	(html-item "tr"
	  (html-item "td align=center" (html-format "~D" (coa-number coa)))
	  (dolist (argd *situation-definition-parameters*)
	    (html-item "td align=center"
	      (html-format "~A"
		(situation-arg coa (q-arg-name argd))))))))))


;;;; Situation table

(defun write-situation-description-table ()
  ;; When we want the description, it's handled by :include-default
  ;; in write-coa-situation-table.
  nil)

#+:undef
(defun write-situation-description-table ()
  (html-anchor (path-action-url :def-situation)
    (with-output-to-string (*html-out*)
      (html-tag-line "h2" "Default initial situation")))
  (html-horizontal-parameter-table
    *situation-definition-parameters*
    #'task-arg))


;;;; Changing the default initial situation

(defun write-situation-definition-page ()
  (html-standard-page (:title "Define default initial situation")
    (html-form "post" (path-action-url :set-situation)
      (write-situation-definition-table nil)
      (html-line "<p>")
      (html-line "<input type=\"submit\" value=\"Define situation\">")
      (html-line "<input type=\"reset\">"))))

(defun set-situation ()
  (parse-query-args)
  (convert-query-args *situation-definition-parameters*)
  (setq *task-query-args* *query-arg-table*))


;;;; COA selection page

(defun write-coa-selection-page ()
  (html-standard-page (:title "Select COAs")
    (html-form "post" (path-action-url :set-coas)
      (html-block "table border=1 cellspacing=0"
	(dolist (coa *coas*)
	  ;; COA description row
	  (html-item "tr"
	    (html-item "td" (html-checkbox (keep-coa-arg coa) t))
	    (html-item "td align=right" (html-format "~D" (coa-number coa)))
	    (loop for i from 1 to *max-objectives* do
	       (if (and (coa-parameters coa)
			(properly-defined-objective-p coa i))
		   (html-item "td"
		     (html-line "~A ~A"
				(coa-arg coa i :action)
				(coa-arg coa i :city)))
		 (html-empty-td))))))
      (html-line "<p>")
      (html-line "<input type=\"submit\" value=\"Set COAs\">")
      (html-line "<input type=\"reset\"  value=\"Reset form\">"))))


;;;; COA definition page

(defun write-coa-definition-page (&optional (coa (get-coa (query-arg :n))))
  (let ((n (coa-number coa)))
    (html-standard-page (:title (format nil "COA ~D definition" n))
      (html-form "post" (path-action-url :define-coa)
        (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">" n)
	(write-coa-definition-table coa)
	(write-situation-definition-table coa)
	(html-line "<p>")
	(html-line "<input type=\"submit\" value=\"Define COA ~D\">" n)
	(html-line "<input type=\"reset\">"))
      (html-line "<hr>")
      (write-coa-description-table))))


;;; COA definition table

;;; /\/: At present, we always present a table in which 5 objectives can
;;; be defined, and there is no way to add more.

(defparameter *gpdt-objective-cities* '("Abyss" "Barnacle" "Calypso"))

(defparameter *gpdt-objective-actions*
  '("evacuate injured"
    "evacuate population"
    "evacuate with medical team"
    "send medical supplies"
    "send emergency food"
    "send medical team"
    "repair gas leak"
    "defuse terrorist bomb"
    "build emergency housing"
    "repair power station turbine"
    "provide immediate assistance"
    "shut down power station"))

(defun write-coa-definition-table (coa)
  (html-tag-line "h2" "Objectives")
  (html-block "table"
    (loop for i from 1 to *max-objectives* do
      (html-item "tr"
        (html-item "td"
	  (cond ((and (coa-parameters coa)
		      (properly-defined-objective-p coa i))
		 ;; Default to the current defintion, if there is one.
		 (html-gpdt-objective-inputs i
		   (coa-arg coa i :action)
		   (string-capitalize (coa-arg coa i :city))))
		((= i 1)
		 ;; Else, a nonempty default for 1st objective,
		 (html-gpdt-objective-inputs 1
		   (first *gpdt-objective-actions*)
		   (first *gpdt-objective-cities*)))
		(t	
		 ;; and an empty default for the others.
		 (html-gpdt-objective-inputs i))))))))

(defun html-gpdt-objective-inputs (objective-number &optional action city)
  ;; Number
  (html-tag "b" "~D" objective-number)
  ;; Action
  (html-select-from-list
    :name (coa-arg-name objective-number "action")
    :size 1
    :options
      (mark-selected action
	`("" ,@*gpdt-objective-actions*)))
  ;; City
  (html-select-from-list
    :name (coa-arg-name objective-number "city")
    :size 1
    :options
      (mark-selected city
	`("" ,@*gpdt-objective-cities*))))

(defun mark-selected (item list)
  (mapcar #'(lambda (e) (if (equal e item) `(:selected ,e) e))
	  list))


;;; Situation definition table

;;; /\/: Ought to be able to get the values in a query-arg-name-type.

(defun write-situation-definition-table (coa)
  ;; The coa can be nil.
  (html-tag-line "h2" "Situation")
  (html-aiai-table ()
    ;; A row containing the query arg names as headings
    (html-item "tr"
      (dolist (argd *situation-definition-parameters*)
	(html-item "th align=center"
          (html-format "~A" (keyword->text (q-arg-name argd))))))
    ;; A row of input items
    (html-item "tr"
      (dolist (argd *situation-definition-parameters*)
        (html-item "td align=center"
	  (html-select-from-list
	    :name (q-arg-name argd)
	    :size 1
	    :options
	      (mark-selected
	        (if (and coa (coa-parameters coa))
		    (situation-arg coa (q-arg-name argd))
		  (task-arg (q-arg-name argd)))
		(query-arg-name-type-values (car (q-arg-type argd))))))))))


;;;; TF output

(defun output-coa-task (coa)
  (with-open-file (*tf-out* (coa-filename coa "task.tf")
			    :direction :output
			    :if-exists :supersede)
    (output (:stream *tf-out*)
      // ";;; COA task"
      //
      // "initially" (:include (output-coa-initial-effects coa)) ";"
      //
      // "task Pacifica_COA_" (coa-number coa) ";"
      // "  nodes"
      // "    sequential"
      // "      1 start,"
                (:include (output-coa-task-actions coa))
      // "      2 finish"
      // "    end_sequential;"
      // "end_task;"
      //
      // "include \"" (web-demo-filename "gpdt") "\";"
      //)))

;;; "Initially" effects

(defun output-coa-initial-effects (coa)
  (output-comma-separated "     "
    (append
      (generate-coa-initial-weather-effects coa)
      (generate-coa-initial-road-status-effects coa)
      (generate-coa-initial-location-and-status-effects coa))))

(defun generate-coa-initial-weather-effects (coa)
  (list (format nil "{weather} = ~A" (situation-arg coa :weather))))

(defun generate-coa-initial-road-status-effects (coa)
  (loop for p in *situation-definition-parameters*
	for name = (symbol-name (q-arg-name p))
	when (sequence-begins "ROAD-" name)
	collect
	  (let ((parts (break-string-at #\- name)))
	    (format nil "{road_status ~:(~A_~A~)} = ~A"
		    (second parts) (third parts)
		    (situation-arg coa (q-arg-name p))))))

(defun generate-coa-initial-location-and-status-effects (coa)
  (declare (ignore coa))
  (loop for property in '("location" "available" "empty_vehicle")
	as  value    in '("Delta"    "true"      "true")
	collect
	  (format nil "{~A ??} = ~A" property value)))

;;; Task actions

(defun output-coa-task-actions (coa)
  (let ((actions (generate-coa-task-actions coa)))
    (when actions
      (output // "      parallel")
      (output-comma-separated "        "
	(loop for action in actions
	      as i from 3
	      collect (format nil "~D action ~A" i action)))
      (output // "      end_parallel,"))))

(defun generate-coa-task-actions (coa)
  (loop for ttd from 1 to *max-objectives*
	when (properly-defined-objective-p coa ttd)
	collect
	  (format nil "{~A ~A}"
		  (replace-subseq "_" " " (coa-arg coa ttd :action))
		  (coa-arg coa ttd :city))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

