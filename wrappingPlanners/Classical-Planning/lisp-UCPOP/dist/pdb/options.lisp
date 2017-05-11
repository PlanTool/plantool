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

(defvar *shell-options-table* nil)
(defvar *shell-options-sets* nil)

(defstruct option
  name value range)

(defstruct range-pair
  value documentation)

(defun range-pair (v)
  (cond ((consp v)
	 (make-range-pair :value (car v)
			  :documentation (cadr v)))
	(T
	 (make-range-pair :value v
			  :documentation (format nil "~A" v)))))

(defun get-range (range value)
  (find value range :key #'range-pair-value))

(defun get-option-documentation (option value)
  (let ( (range (get-range (option-range option) value)) )
    (range-pair-documentation range)))

(defun option-in-range (option value)
  (let ( (range (get-range (option-range option) value)) )
    (not (null range))))



(defun define-option (name value range)
  (let ( (old-option (find name *shell-options-table* :key #'option-name)) )
    (cond ((not (null old-option))
	   (setf (option-range old-option) (mapcar #'range-pair range)
		 (option-value old-option) value))
	  (t
	   (push (make-option 
		  :name name
		  :value value
		  :range (mapcar #'range-pair range))
		 *shell-options-table*)))))

(defun get-option (name &optional (table *shell-options-table*))
  (let ( (option (find name table :key #'option-name)) )
    option))

(defun get-option-value (name &optional (table *shell-options-table*))
  (let ( (option (find name table :key #'option-name)) )
    (cond ((not (null option))
	   (option-value option))
	  (t nil))))

(defun set-option-value (name value &optional (table *shell-options-table*))
  (let ( (old-option (find name table :key #'option-name)) )
    (cond ((not (null old-option))
	   (if (option-in-range old-option value) 
	       (setf (option-value old-option) value)
	     (error "Bad option value"))))))



;;; This allows you to associate a tag with a list of options.
;;; You can use get-option-set to get a copy of those options
;;; from the database and set-option-set to save the options
;;; straight back to the database. Individual options can
;;; be retrieved using get-option-value and set-option-value
;;; with the list of options as the extra argument.

(defun define-option-set (name options)
  (let ( 
	(set (assoc name *shell-options-sets*))
	)
    (cond ((null set)
	   (push (list name options)
		 *shell-options-sets*))
	  (T
	   (setf (cadr set) options)))))

(defun get-option-set (name)
  (let ( (set (assoc name *shell-options-sets*)) )
    (mapcar #'(lambda (n)
		(copy-option
		 (get-option n *shell-options-table*)))
	    (cadr set))))

(defun set-option-set (options)
  (mapc #'(lambda (option)
	    (set-option-value (option-name option)
			      (option-value option)))
	options))


;;; Read and write options from a file.

(defun read-option-file ()
  (let* (
	 (directory (namestring (user-homedir-pathname)))
	 (filename  ".plandebugrc")
	 (pathname (make-pathname :directory directory
				  :name filename))
	 )
    (let ( (stream (open pathname :direction :input
			 :if-does-not-exist nil)) )
      (when (not (null stream))
	(read-options stream)
	(close stream)))))

(defun write-option-file ()
  (let* (
	 (directory (namestring (user-homedir-pathname)))
	 (filename  ".plandebugrc")
	 (pathname (make-pathname :directory directory
				  :name filename))
	 )
    (let ( (stream (open pathname :direction :output
			          :if-exists :overwrite
				  :if-does-not-exist :create)) )
      (write-options stream)
      (close stream))))

(defun read-options (stream)
  (let ( (next (read stream)) )
    (set-option-value (car next) (cadr next))))

(defun write-options (stream)
  (dolist (option *shell-options-table*)
    (format stream "(~S ~S)~%"
	    (option-name option)
	    (option-value option))))








