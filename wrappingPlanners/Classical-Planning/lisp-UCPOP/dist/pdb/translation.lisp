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

;;; plan-translation.lisp
(in-package "VCR")

;;; --- struct/graph correspondence table

;;; retrieve presentation for struct
(defun get-translation (table object 
			&aux (tcons (assoc object (gethash object table))))
  (when tcons (cdr tcons)))

;;; store struct/presentation pair
(defun put-translation (table object graph)
  (setf (gethash object table)
    (cons (cons object graph)
	  (gethash object table))))



;;; --- effects

(defun translate-effect (table effect &key frame stream)
  (let* ( (ge (make-instance 'graphic-effect
		:object effect
		:frame frame
		:stream stream)) )
    (put-translation table effect ge)
    (mapc #'(lambda (a)
	      (translate-condition table a
				   :frame frame :stream stream))
	  (get-effect-add effect))
    ge))


;;; --- conditions

(defun translate-condition (table cond &key frame stream)
  (let ( (co (make-instance 'graphic-cond
	       :object cond
	       :frame frame
	       :stream stream)) )
    (put-translation table cond co)
    co))



;;; --- steps

(defun translate-step (table step &key frame stream)
  (let ( (gs (make-instance 'graphic-step
	       :object step
	       :frame frame
	       :stream stream)) )
    (put-translation table step gs)
    (mapc #'(lambda (e) (translate-effect table e
					  :frame frame :stream stream))
	  (get-step-effects step))
    gs))


;;; --- links

(defun translate-link (table link &key frame stream)
  (let ( (gl (make-instance 'graphic-link
	       :object link
	       :frame frame
	       :stream stream)) )
    (put-translation table link gl)
    (translate-condition table (get-link-openc link)
			 :frame frame :stream stream)
    gl))

;;; --- threats(unsafe)

(defun translate-threat (table threat &key frame stream)
  (let ( (gt (make-instance 'graphic-flaw
	       :object threat
	       :frame frame
	       :stream stream)) )
    (put-translation table threat gt)
    gt))

;;; --- plan

(defun translate-plan (table plan &key frame stream)
  (let (
	(plan-table (make-hash-table))
	(steps (get-plan-steps plan))
	(links (get-plan-links plan))
	(openc (get-plan-open plan))
	(threats (get-plan-unsafe plan))
	)
    (when is-xii
      (when (not (null (get-plan-step plan 0)))
	(translate-step plan-table (get-plan-step plan 0) :frame frame :stream stream)))
    
    (mapc #'(lambda (s) (translate-step plan-table s :frame frame :stream stream))
	  steps)
    (mapc #'(lambda (l) (translate-link plan-table l :frame frame :stream stream))
	  links)
    (mapc #'(lambda (o) (translate-condition plan-table o :frame frame :stream stream))
	  openc)
    (mapc #'(lambda (o) (translate-threat plan-table o :frame frame :stream stream))
	  threats)
    (put-translation table plan plan-table)
    plan-table))
    
;;; --- plan tree

(defun translate-plan-tree (table node &key frame stream)
  (let ( (pt (make-instance 'graphic-node
	       :object node
	       :nodes  nil
	       :frame frame
	       :stream stream)) )
    (put-translation table node pt)
    pt))















