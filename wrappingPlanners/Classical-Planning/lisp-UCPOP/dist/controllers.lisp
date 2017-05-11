" (c) 1993,1994 copyright (c) university of washington
  written by tony barrett.

  all rights reserved. use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  all copies must
  include this copyright message.  this software is made available as is, and
  neither the authors nor the university of washington make any warranty about
  the software or its performance.

  when you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "UCPOP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. special purpose search controllers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mimicing the old best first search
;;;
;;; (sc-control 'sussman-anomaly #'bf-mimic)

(defun bf-mimic (prob)
  (declare (ignore prob))
  (reset-controller)
  (define (scr select-ranked)
      :when '((:node $p $r) (rank-plan $p $n))
      :effect '(:rank :node $n $p))
  (define (scr select-threats)
      :when '((:current :node $n)
	      (:flaw $g1)
	      (threat $n $g1 $l $t))
      :effect '(:rank :flaw -1 $g1))
  (define (clause (rank-plan p n))
      (bound! 'rank-plan p)
    (when (plan-p p)
      (matchb n (list (rank3 p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a search controller for the prodigy blocks world domain
;;;
;;; (sc-control 'prodigy-sussman #'speed-prodigy-bw)

(defun speed-prodigy-bw (prob)
  (bf-mimic prob)
  (define (scr rank-goals)
      :when '((:flaw $f) (rank-goals $f $n))
      :effect '(:rank :flaw $n $f))
  (define (scr only-puton-for-goals)
      :when '((:current :flaw $g)
	      (:node $p (:step $s $add))
	      (operator $s (stack $b $c) $p)
	      (goal $p $g $t $sn) (neq $sn :goal))
      :effect '(:reject :node $p))
  (define (scr prefer-ons)
      :when '((:candidate :flaw 3 $g1)
	      (:candidate :flaw 3 $g2)
	      (:current :node $p)
	      (goal $p $g1 (on $a $b) :goal)
	      (goal $p $g2 (on $b $c) :goal))
      :effect '(:prefer :flaw $g2 $g1))
  (define (clause (rank-goals g n))
      (bound! 'rank-goals g)
    (when (and (openc-p g) 
	       (member (car (openc-condition g)) 
		       '(on on-table holding clear arm-empty)))
      (matchb n (list (cdr (assoc (car (openc-condition g)) 
				  '((on . 3) (on-table . 2)
				    (holding . 1) (clear . 4)
				    (arm-empty . 5)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a search controller for the prodigy processp domain (included in other-domains dir)
;;;
;;; (sc-control (load-metal-prob "p101") #'speed-machine)

(defun speed-machine (prob)
  (bf-mimic prob)
;  (define (scr plan-cost)
;      :when   '((:node $p $r) (plan-cost $p $n))
;      :effect '(:rank :node $n $p))
  (define (scr rank-goals)
      :when '((:flaw $t) (rank-goals $t $n))
      :effect '(:rank :flaw $n $t))
  (define (clause (plan-cost p n))
      (bound! 'plan-cost p)
    (when (plan-p p)
      (let ((cost 0))
	(dolist (s (plan-steps p))
	  (incf cost (case (car (p-step-action s))
		       ((drill-with-spot-drill
			 drill-with-twist-drill drill-with-high-helix-drill
			 drill-with-straight-fluted-drill
			 drill-with-oil-hole-drill
			 drill-with-gun-drill drill-with-center-drill) 10)
		       
		       ((tap countersink counterbore ream) 10)
		       
		       ((side-mill
			 face-mill drill-with-spot-drill-in-milling-machine
			 drill-with-twist-drill-in-milling-machine) 10)
		       
		       ((rough-turn-rectangular-part
			 rough-turn-cylindrical-part
			 finish-turn make-thread-with-lathe
			 make-knurl-with-lathe file-with-lathe
			 polish-with-lathe) 10)
		       
		       ((rough-shape finish-shape) 10)
		       
		       ((rough-shape-with-planer finish-shape-with-planer) 10)
		       
		       ((rough-grind-with-hard-wheel
			 rough-grind-with-soft-wheel
			 finish-grind-with-hard-wheel
			 finish-grind-with-soft-wheel) 10)
		       
		       ((cut-with-circular-cold-saw
			 cut-with-circular-friction-saw) 10)
		       
		       ((cut-with-band-saw polish-with-band-saw) 10)
		       
		       ((weld-cylinders-metal-arc weld-cylinders-gas) 100)
		       
		       ((metal-spray-coating metal-spray-prepare) 10)
		       
		       ((clean remove-burrs) 5)
		       
		       ((put-tool-on-milling-machine
			 put-in-drill-spindle put-toolbit-in-lathe
			 put-cutting-tool-in-shaper-or-planer
			 put-wheel-in-grinder
			 put-circular-saw-attachment-in-circular-saw
			 put-band-saw-attachment-in-band-saw
			 put-electrode-in-welder
			 remove-tool-from-machine) 2)
		       
		       ((put-holding-device-in-milling-machine
			 put-holding-device-in-drill
			 put-holding-device-in-lathe
			 put-holding-device-in-shaper
			 put-holding-device-in-planer
			 put-holding-device-in-grinder
			 put-holding-device-in-circular-saw
			 put-holding-device-in-welder
			 remove-holding-device-from-machine) 2)
		       
		       ((add-soluble-oil
			 add-mineral-oil add-any-cutting-fluid) 1)
		       
		       ((put-on-machine-table
			 put-on-shaper-table hold-with-v-block hold-with-vise
			 hold-with-toe-clamp
			 secure-with-toe-clamp hold-with-centers
			 hold-with-4-jaw-chuck
			 hold-with-collet-chuck hold-with-magnetic-chuck
			 release-from-holding-device
			 release-from-holding-device-weak) 1)
		       (t 0))))
	(matchb n (list cost)))))
  (define (clause (rank-goals g n))
      (bound! 'rank-goals g)
    (cond
     ((openc-p g)
      (matchb n 
	      (list (case (car (openc-condition g))
		      (is-a -7)
		      ((is-a is-of-type			  
			angle-of-drill-bit 
			size-of-drill-bit diameter-of-drill-bit
			material-of-abrasive-cloth hardness-of-wheel
			grit-of-wheel size-of-machine
			hardness-of side-up-for-machining 
			has-high-melting-point
			is-available-tool-holder
			is-available-tool
			is-available-table
			is-available-holding-device
			is-available-machine
			has-center-holes
			shape-of
			is-empty-holding-device
			is-available-part
			is-of-type
			surface-finish-quality-side
			surface-finish
			surface-coating
			alloy-of
			size-of
			)  -5)
		      (:or  -4)
		      ((half-of smaller smaller-than-2in 
			finishing-size new-size new-part new-material) -3)
		      ((HAS-CENTER-HOLE HAS-SPOT HAS-HOLE) -2)
		      (t 0)))))
     ((univ-threat-p g)
      (matchb n (list -4)))
     (t nil))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a search controller for the refridgerator domain
;;;
;;; (sc-control 'fixa #'speed-fridge)
;;; (sc-control 'fixb #'speed-fridge)
  
(defun speed-fridge (prob)
  (bf-mimic prob)
  (define (scr reject-symmetric-holds)
      :when '((:node $p1 (:link 0 (holds $a1 $f)))
	      (:node $p2 (:link 0 (holds $a2 $f)))
	      (prefered-screw $a1 $a2))
      :effect '(:reject :node $p2))
  (define (scr no-screw-abstraction)
      :when '((:flaw $s) 
	      (:current :node $p)
	      (goal $p $s $g $sn) (no-screw-abstraction $g))
      :effect '(:rank :flaw -1 $s))
  (define (clause (prefered-screw a1 a2))
      (bound! 'prefered-screw a1 a2)
    (when (and (symbolp a1) (symbolp a2)
	       (string< (symbol-name a1) (symbol-name a2)))
      '(nil)))
  (define (clause (no-screw-abstraction g))
      (bound! 'no-screw-abstraction g)
    (when (not (member (if (eq (car g) :not) (caadr g) (car g))
		       '(screwed screw)))
      '(nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a search controller for the ferry domain
;;;
;;; (sc-control 'test-ferry #'speed-ferry)

(defun speed-ferry (prob)
  (declare (ignore prob))
  (reset-controller)
  (define (scr select-threats)
      :when '((:current :node $n)
	      (:flaw $f)
	      (threat $n $f $l $t))
      :effect '(:rank :flaw -2 $f))
  (define (scr road-abstraction)
      :when '((:current :node $p) (:flaw $s)
	      (goal $p $s $g $sn) (in-road-abstraction $g))
      :effect '(:rank :flaw -1 $s))
  (define (scr price-plan)
      :when   '((:node $p $r) (sail-cost $p $n))
      :effect '(:rank :node $n $p))
  (define (scr reject-partial-sails)
      :when '((:current :flaw $g)
	      (:node $p (:step $s $a))
	      (operator $s (debark $x $y) $p)
	      (goal $p $g $t $sn) (neq $sn :goal))
      :effect '(:reject :node $p))
  (define (clause (in-road-abstraction g))
      (bound! 'in-road-abstraction g)
    (when (member (car g) '(at empty-ferry on auto place))
       '(nil)))
  (define (clause (sail-cost p n))
      (bound! 'sail-cost p)
    (when (plan-p p)
      (let ((cost 0))
	(dolist (s (plan-steps p))
	  (incf cost (case (car (p-step-action s))
		       ('sail 40)
		       ('board 2)
		       ('debark 2)
		       (t 0))))
	(matchb n (list cost))))))

(defun speed-ferry2 (prob)
  (speed-ferry prob)
  (define (scr flaw-cost)
      :when   '((:node $p $r) (flaw-cost $p $n))
      :effect '(:rank :node $n $p))
  (define (clause (flaw-cost p n))
      (bound! 'flaw-cost p)
    (when (plan-p p)
      (let ((cost 0))
	(dolist (s (plan-flaws p))
	  (when (and (openc-p s) (eq :goal (openc-id s)))
	    (incf cost (case (car (openc-condition s))
			 (at 45)
			 (t 0)))))
	(matchb n (list cost))))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a search controller for tire world

(defun speed-tire (prob)
  (declare (ignore prob))
  (reset-controller)
  (define (scr select-threats)
      :when '((:current :node $n)
	      (:flaw $f)
	      (threat $n $f $l $t))
      :effect '(:rank :flaw -2 $f))
  (define (scr price-plan)
      :when   '((:node $p $r) (plan-cost $p $n))
      :effect '(:rank :node $n $p))
  (define (scr delay-in)
      :when '((:flaw $f) 
	      (:current :node $p)
	      (goal $p $f (in $a $b) :goal))
      :effect '(:rank :flaw 1 $f))
  (define (clause (plan-cost p n))
      (bound! 'plan-cost p)
    (when (plan-p p)
      (let ((cost (length (plan-steps p))))
	(dolist (s (plan-flaws p))
	  (when (openc-p s) (incf cost))
	  (when (and (openc-p s) (eq :goal (openc-id s)))
	    (incf cost (case (car (openc-condition s))
			 (open 1)
			 ((in have) 2)
			 ((tight inflated on-ground) 20)
			 (on 11)
			 (t 0)))))
	(matchb n (list cost))))))

(defun speed-tire2 (prob)
  (speed-tire prob)
  
  (fail-link '(put-away $a $b) '(in $a $b) '(fetch $a $b))
  (fail-link '(close $a) '(:not (open $a)) '(open $a))
  (fail-link '(jack-down $a) '(:not (jacked $a)) '(jack-up $a))
  (fail-link '(jack-down $a) '(have jack) '(jack-up $b)))
  
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handy utility

(defun fail-link (source condition destination)
  (define (scr reject-link)
      :when `((:current :node $n-111)
	      (:current :flaw $f-111)
	      (:node $p-111 (:step $s2-111 ,condition))
	      (operator $s2-111 ,source $p-111)
	      (goal $n-111 $f-111 $g-111 $s1-111)
	      (operator $s1-111 ,destination $p-111))
      :effect '(:reject :node $p-111)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A search controller for the STRIPS world
;;;
;;; (sc-control 'move-boxes #'speed-strips)

(defun speed-strips (prob)
  (bf-mimic prob)
  (define (scr rank-goals)
      :when '((:flaw $f) (rank-goals $f $n))
      :effect '(:rank :flaw $n $f))
  (define (scr ignore-locations-1)
      :when '((:node $p (:step $s $add))
	      (operator $s (goto-loc $b $c $d) $p))
      :effect '(:reject :node $p))
  (define (scr ignore-locations-2)
      :when '((:node $p (:step $s $add))
	      (operator $s (push-to-loc $a $b $c $d) $p))
      :effect '(:reject :node $p))
  (define (clause (rank-goals g n))
      (bound! 'rank-goals g)
    (when (openc-p g)
      (let ((x (assoc (car (openc-condition g))
		      '((in-room . 1) (is-type . 0) (connects . -1)
			(loc-in-room . 8) (pushable . 0) (next-to . 5)
			(statis . 6)))))
	(when x 
	  (setf x (if (member 'robot (openc-condition g))
		      (1+ (cdr x)) (cdr x))))
	(when x (matchb n (list x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;u

(defun test-sussman ()
  (sc-control 'sussman-anomaly #'bf-mimic))

(defun test-prodigy-bw ()
  (sc-control 'prodigy-sussman #'speed-prodigy-bw))

(defun test-fixa ()
  (sc-control 'fixa #'speed-fridge))

(defun test-fixb ()
  (sc-control 'fixb #'speed-fridge))

(defun test-ferry ()
  (sc-control 'test-ferry #'speed-ferry))

(defun demo-ferry (host)
  (trace-scr 'select-threats)
  (trace-scr 'road-abstraction)
  (trace-scr 'price-plan)
  (trace-scr 'reject-partial-sails)
  (sc-show 'test-ferry #'speed-ferry host)
  (untrace-scr))

(defun demo-tire (host)
  (sc-show 'fixit2 #'speed-tire host)
  (trace-scr 'reject-link)
  (sc-show 'fixit2 #'speed-tire2 host)
  (untrace-scr))

(defun demo-fridge (host)
  (trace-scr 'reject-symmetric-holds)
  (trace-scr 'select-ranked)
  (trace-scr 'select-threats)
  (trace-scr 'no-screw-abstraction)
  (trace-scr 'prefered-screw)
  (sc-show 'fixb #'speed-fridge host)
  (untrace-scr))

(defun e1 (&optional (host "hobbes:0"))
  (sc-show 'fixit2 #'speed-tire host))

(defun e2 (&optional (host "hobbes:0"))
  (sc-show 'fixit2 #'speed-tire2 host))