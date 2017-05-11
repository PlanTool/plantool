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

(in-package "VCR")

(defclass graphic ()
	  ((object :initarg :object :accessor graphic-object)
	   
	   (stream  :initarg :stream  :accessor graphic-stream)
	   (frame   :initarg :frame   :accessor graphic-frame)
	   (x       :initarg :x       :accessor graphic-x)
	   (y       :initarg :y       :accessor graphic-y)
	   (shown   :initform nil     :accessor graphic-shown)))

(defclass graphic-flaw (graphic) ())
(defclass graphic-step (graphic) ())
(defclass graphic-cond (graphic) ())
(defclass graphic-link (graphic) ())
(defclass graphic-effect (graphic) ())

(defclass graphic-node (graphic)
	  ((nodes :initarg :nodes :accessor graphic-node-nodes)))

(defclass graphic-browser-item (graphic)
	  ((name    :initarg :name :accessor graphic-browser-item-name)
	   (stack   :initarg :stack :accessor graphic-browser-item-stack)
	   (submenu :initform nil :initarg :submenu :accessor graphic-browser-item-submenu)
	   (printer :initform nil :initarg :printer :accessor graphic-browser-item-printer)))

;;; --- Session structure


(defclass session ()
	  ((domain   :initarg :domain  :initform nil :accessor session-domain)
	   (problem  :initarg :problem :initform nil :accessor session-name)
   
	   (limit    :initarg :limit :initform 0 :accessor session-limit)
	   (search-f :initarg :search-f :initform nil :accessor session-search-f)
	   (rank-f   :initarg :rank-f :initform nil :accessor session-rank-f)
	   (sc       :initarg :sc :initform nil :accessor session-sc)
	   (result   :initarg :result :initform nil :accessor session-result)
	   (solution :initarg :solution :initform nil :accessor session-solution)
           
	   (tree     :initarg :tree :initform nil :accessor session-tree)
	   (size     :initarg :size :initform 0 :accessor session-size)
	   
	   (nodes    :initarg :nodes :initform nil :accessor session-nodes)
	   (tape     :initarg :tape :initform nil :accessor session-tape)
	   (ranks    :initarg :ranks :initform nil :accessor session-ranks)
	   (firings  :initarg :firings :initform nil :accessor session-firings)
	   (max-rank :initform 0 :accessor session-max-rank)
	   (min-rank :initform 0 :accessor session-min-rank)
	   ))


;;; --- Plan tree node and graph structure

(defclass plan-tree ()
	  ((entry   :initarg :entry :accessor plan-tree-entry)
	   (rank    :initform nil :initarg :rank :accessor plan-tree-rank)
	   (flaw    :initform nil :initarg :flaw :accessor plan-tree-flaw)
	   (firings :initform nil :initarg :firings :accessor plan-tree-firings)
	   (diff    :initform nil :initarg :diff :accessor plan-tree-diff)
	   
	   (number   :initform 0 :initarg :number :accessor plan-tree-number)
	   (parent   :initform nil :initarg :parent :accessor plan-tree-parent)
	   (children :initform nil :initarg :children :accessor
		     plan-tree-children)
	   ;; this is cached for plan windows
	   (color :initform nil :initarg :color :accessor plan-tree-color)
	   (next :initarg :prev :initform nil :accessor plan-tree-next)
	   (prev :initarg :next :initform nil :accessor plan-tree-prev)))
