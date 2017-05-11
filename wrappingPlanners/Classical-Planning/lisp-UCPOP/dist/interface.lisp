" (c) 1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "UCPOP")

(export '(*version*))
(defconstant *version* "4.0.8")

#+:clim-2 (import '(vcr:VCR-RECORD-RANK
		    vcr:PDB-NEW
		    vcr:PDB-LOAD
		    vcr:PDB-END
		    vcr:PDB-RECORD
		    vcr:PDB-STOP
		    vcr:PDB-SETUP
		    vcr:PDB-DISPLAY
		    vcr:PDB-SAVE
		    vcr:PDB-RESTART
		    ))

(export '(define reset-domain trace-scr untrace-scr profile 
	  show-profile operator axiom fact problem clause scr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Variables

(defparameter *tests* nil)

(defparameter *domains* nil)

(defparameter *controller* nil)

(defparameter *cache-flaws* t)

(defparameter *safety-constraints* nil)

(defstruct problem
  name
  domain
  inits
  goal
  rank-fun
  flaw-fun
  )

(defstruct domain
  name
  operators
  axioms
  facts
  safety)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Interface functions

(defmacro DEFINE ((dtype name) &body body)
  (cond ((eq dtype 'operator)
         `(apply #'defoper ',(cons name body)))
        ((eq dtype 'axiom)
         `(apply #'defaxiom ',(cons name body)))
        ((eq dtype 'fact)
	 `(apply #'deffact ',(cons name body)))
        ((eq dtype 'problem)
         `(apply #'def-problem '(,name ,@body)))
	((eq dtype 'domain)
	 `(apply #'def-domain ',(cons name (list body))))
	((eq dtype 'clause)
	 (macroexpand `(sc:def-clause *controller* ,name ,@body)))
	((eq dtype 'scr)
	 `(sc:rule *controller* ',name ,@body))
	((eq dtype 'search)
	 (macroexpand `(def-search ,name ,body)))
	((eq dtype 'regression-test)
	 (macroexpand `(def-regression-test ,name ,body)))
	((eq dtype 'safety)
	 `(setf *safety-constraints* ,@body))
	))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Purge a previously defined domain theory.
;;; 

(defun RESET-DOMAIN ()
  (setf *facts* nil)
  (setf *axioms* nil)
  (setf *templates* nil)
  (setf *safety-constraints* nil))

(defun trace-scr (name)
  (rule-net:trace-rule name))

(defun untrace-scr ()
  (rule-net:untrace-rule))

(defun profile ()
  (rule-net:profile))

(defun show-profile ()
  (rule-net:show-profile))

(defun bf-show (prob)
#+:clim-2 (vcr:enable-vcr)
  (bf-control prob))

(defmacro def-search (name body)
  (let ((prob (caar body))
	(aux-param (cdar body)))
    `(defun ,name (,prob ,@aux-param)
       (setf ,prob  (set-up-problem ,prob))
#+:clim-2 (when vcr:*enable-vcr*
	    (pdb-record)
	    (pdb-new ,prob)
	    (pdb-load ,prob)
	    )
       (multiple-value-bind (plan stat)
	   ,@(cdr body)
#+:clim-2 (when vcr:*enable-vcr* (pdb-end ,prob stat plan) (pdb-display))
          (when (>= *plans-created* *search-limit*)
	     (format t "~%Warning: Search limit exceeded"))
	  (when plan (display-plan plan))
	   (display-stat stat)
	   (values plan stat)))))

(define (search bf-control) (prob)
  (plan (problem-inits prob) (problem-goal prob) 
	:rank-fun (problem-rank-fun prob)
	:flaw-fun (problem-flaw-fun prob)))

(define (search ie-control) (prob)
  (plan (problem-inits prob) (problem-goal prob)
	:search-fun #'call-ie :rank-fun (problem-rank-fun prob)
	:flaw-fun (problem-flaw-fun prob)))

(define (search ibf-control) (prob)
      (plan (problem-inits prob) (problem-goal prob) 
            :search-fun #'id-bf-search
            :rank-fun (problem-rank-fun prob)
	    :flaw-fun (problem-flaw-fun prob)))

;; Regression tests are just searches with minimal output.

(defmacro def-regression-test (name body)
  (let ((prob (caar body))
	(aux-param (cdar body)))
    `(defun ,name (,prob ,@aux-param)
       (setf ,prob  (set-up-problem ,prob))
#+:clim-2 (when vcr:*enable-vcr*
	    (pdb-record)
	    (pdb-new ,prob)
	    (pdb-load ,prob)
	    )
       (multiple-value-bind (plan stat)
	   ,@(cdr body)
#+:clim-2 (when vcr:*enable-vcr* (pdb-end ,prob stat plan) (pdb-display))
         (format t "~%~20@a  ~5@a  ~5@a  ~8@f s ~8@f bf. ~3@a steps. ~a"
		 (problem-name prob)
		 (stat-created stat)
		 (stat-visited stat)
		 (/ (stat-time stat) internal-time-units-per-second)
		 (stat-ave-branch stat)
		 (stat-plan-len stat)
		 (if (stat-complete? stat)
		     "Complete!"
		   (if (stat-reached-max? stat)
		       "Hit limit."
		     "Failure!"))
		 )
	 (values plan stat)))))

(define (regression-test bf-test) (prob)
  (plan (problem-inits prob) (problem-goal prob) 
	:rank-fun (problem-rank-fun prob)
	:flaw-fun (problem-flaw-fun prob)))

(defun ISC-CONTROL (prob scs)
  "Use search controller to perform an iterative deepening 
   best first search"
  (setf prob (set-up-problem prob))
  (sc-control* prob scs #'sc:isrch))

(defun SC-CONTROL (prob scs)
  "Use search controller to perform a best first search"
  (setf prob (set-up-problem prob))
  (sc-control* prob scs #'sc:srch))

(defun sc-show (prob scs &optional (display nil))
  "Use search controller to perform a best first search,
   followed by a graphical exploration of search space"
  (pdb-record)
  (multiple-value-bind (plan stat) 
      (sc-control prob scs)
    (progn 
      (pdb-stop)
      (pdb-display)
      (values plan stat))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

(defun def-problem (name &key domain inits goal rank-fun flaw-fun)
  (let ((problem 
         (make-problem 
          :name name
          :domain (eval domain)
          :inits inits
          :goal goal
          :rank-fun rank-fun
	  :flaw-fun flaw-fun)))
    (setf *tests* (remove name *tests* :key #'problem-name))
    (push problem *tests*)
    problem))

(defun def-domain (name clauses)
  (reset-domain)
  (dolist (clause clauses)
    (case (car clause)
      (:operator
       (apply #'defoper (cdr clause)))
      (:axiom
       (apply #'defaxiom (cdr clause)))
      (:fact
       (apply #'deffact (cdr clause)))
      (:safety 
       (setf *safety-constraints* (caddr clause)))
      ))
  (push (make-domain
	 :name name
	 :operators *templates*
	 :axioms *axioms*
	 :safety *safety-constraints*
	 :facts *facts*)
	*domains*)
  (reset-domain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Search routines for UCPOP

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A simple best first search strategy.  Returns 3 values: the found state,
;;;  the average branching factor, and the number of generated but unexplored 
;;;  states.  The search will only generate up to LIMIT states.  
(defun BESTF-SEARCH (initial-state daughters-fn goal-p rank-fn limit)
  (let ((branches nil))                         ; compute average branch factor
    (do* ((current-state initial-state (cdar search-queue))
          (search-queue nil (cdr search-queue)))
         ((or (null current-state)
              (funcall goal-p current-state)
              (> 0 limit))
          (values current-state
                  (if (null branches) 0
                      (div* (reduce #'+ branches) (length branches)))
                  (length search-queue)))
      (let ((children (mapcar #'(lambda (c)
				  (cons (funcall rank-fn c) c))
			      (funcall daughters-fn current-state))))
	(let ((num-children (length children)))
	  (push num-children branches)
	  (decf limit num-children))
#+:clim-2(dolist (c children) (vcr-record-rank (car c) (cdr c)))
        (setf search-queue
          (my-merge 
           search-queue
           (sort children #'< :key #'car)
           #'(lambda (x y) (< (car x) (car y)))))
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;  IE search function written by Stuart Russell 
;;;  (See "Efficient Memory-Bounded Search Methods" in ECAI-92)

(defun CALL-IE (node successors goalp rank-fn limit)
  (setf *ie-limit* limit)
  (setf *ie-branches* (cons 0 0))
  (let ((solution (ie (cons (funcall rank-fn node) node)
                      goalp successors rank-fn most-positive-single-float)))
    (values solution (if (zerop (car *ie-branches*)) 0 
                         (div* (cdr *ie-branches*) (car *ie-branches*))) 0)))

(defun IE (vnode goalp successors rank-fn bound &aux children)
  (cond ((or (funcall goalp (cdr vnode)) (> 0 *ie-limit*))
         (cdr vnode))
        ((null (setf children
                 (mapcar #'(lambda (child)
                             (cons (funcall rank-fn child) child))
                         (funcall successors (cdr vnode)))))
         (setf (car vnode) most-positive-single-float)
         nil)
        (t 
           (decf *ie-limit* (length children))
           (incf (car *ie-branches*)) 
           (incf (cdr *ie-branches*) (length children))
           (dolist (vn children)    ;;; pathmax
             (setf (car vn) (max (car vn) (car vnode))))
           (do ()
               ((> (car vnode) bound))
             (setf children
                   (sort children #'< :key #'car))
             (let* ((best (car children))
                    (rest (cdr children))
                    ;; best sibling value
                    (new-bound (reduce #'min (cons bound (mapcar #'car rest)))))
               (let ((v (ie best goalp successors rank-fn new-bound)))
                 (when v (return v)))
               (if (and rest (< (caar rest) (car best)))
                 (setf (car vnode) (caar rest))
                 (setf (car vnode) (car best))))))))

(defun SET-UP-PROBLEM (prob)
  (when (symbolp prob) 
    (setf prob (find prob *tests* :key #'problem-name)))
  (unless prob (error "Could not find problem"))
  (assert-domain (problem-domain prob))
  prob)

(defun ASSERT-DOMAIN (name)
  (let ( (domain (find name *domains* :key #'domain-name)) )
    (cond ((null domain)
	   (funcall name))
	  (T
	   (setf *templates* (domain-operators domain))
	   (setf *axioms* (domain-axioms domain))
	   (setf *facts* (domain-facts domain))
	   (setf *safety-constraints* (domain-safety domain))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; A linear-space best-first search routine by Richard Korf
;;; See "Linear-Space Best-First Search: Summary of Results" in AAAI92

(defun ID-BF-SEARCH (initial-state daughters-fn goal-p rank-fn limit)
  (labels 
      ((RBFS (n v b) ;; n = node, v = rank, b = bound
         (when (funcall goal-p n) 
           (return-from ID-BF-SEARCH (values n 0 0)))
         (let ((rank (funcall rank-fn n))
               (kids (funcall daughters-fn n)))
           (cond ((null kids) (return-from RBFS most-positive-fixnum))
                 ((> 0 (decf limit (length kids))) 
                  (return-from ID-BF-SEARCH (values n 0 0))))
           (setf kids 
             (mapcar #'(lambda (k) (cons (funcall rank-fn k) k)) kids))
	   (if (= 1 (length kids))
               (setf (cdr kids) `((,most-positive-fixnum . nil)))
             (setf kids (sort kids #'< :key #'car)))
           (dolist (k kids)
             (when (and (< rank v) (< (car k) v)) (setf (car k) v)))
           (do () ((> (car (nth 0 kids)) b) (car (nth 0 kids)))
             (setf (car (nth 0 kids)) 
               (rbfs (cdr (nth 0 kids)) (car (nth 0 kids)) 
                     (min (car (nth 1 kids)) b)))
             (setf kids
               (let ((temp kids))
                 (pop kids)
                 (setf (cdr temp) nil)
                 (my-merge temp kids
                           #'(lambda (x y) (< (car x) (car y))))))))))
    (rbfs initial-state (funcall rank-fn initial-state) most-positive-fixnum)
    (values initial-state 0 0)))


;;;;;;;;;;;;;;;;;;;;
;;; non recursive best first search for getting Composer statistics.
(defun ID-BESTF-SEARCH (initial-state successors goalp rank-fn limit)
  (let ((stack `(((,(funcall rank-fn initial-state) . ,initial-state))))
        (bounds (list most-positive-single-float)))
    (do () ((null stack))
      (let ((best (car stack))
            (rest (cdar stack)))
        ;; Test the last node in the search path
        (when (or (funcall goalp (cdar best)) (> 0 limit))
          (return-from ID-BESTF-SEARCH (values (cdar best) 0 0)))
        ;; Incrementally add the next point in the search path
        (let ((kids (mapcar #'(lambda (k)
                                (cons (funcall rank-fn k) k))
                            (funcall successors (cdar best)))))
          (cond ((null kids)
                 (push `((,most-positive-single-float)) stack)
                 (push (car bounds) bounds))
                (t (setf kids (sort kids #'< :key #'car))
		   (dolist (k kids)
                     (setf (car k) (max (car k) (caar best))))
                   (decf limit (length kids))
                   (push (if rest (min (car bounds) (caar rest)) (car bounds))
                         bounds)
                   (push kids stack))))
        ;; Retract the search path until the bound is met
        (do ((best (car stack) (car stack))
             (rest (cdar stack) (cdar stack)))
            ((<= (caar best) (car bounds)))
          (pop stack)
          (pop bounds)
          (setf (caaar stack) (caar best))
          (let ((b (car stack))
                (r (cdar stack)))
            (setf (cdr b) nil)
            (setf (car stack)
              (my-merge b r #'(lambda (x y) (< (car x) (car y)))))))))))

(defun SC-CONTROL* (prob scs srch)
  (init-ucpop)
  (funcall scs prob) ; Setting up the controller
  
#+:clim-2  (when vcr:*enable-vcr*
	      (pdb-record)
	      (pdb-new prob :sc *controller*)
	      (pdb-load prob))
  (let* ((init-time (get-internal-run-time))
         (start (init-plan (problem-inits prob) (problem-goal prob)))
         (plan (funcall srch start *controller* *search-limit*))
         (stat (make-stat :algo        "UCPOP"             
                          :date        (today)
                          :prob-num    1
                          :num-init    (length (problem-inits prob))       
                          :num-goal    (length (problem-goal prob))
                          :plan-len    (if plan (plan-high-step plan) 0)
                          :reached-max? (>= *plans-created* *search-limit*)
                          :complete?    (and plan 
                                             (null (plan-flaws plan)))
                          :time         (- (get-internal-run-time) init-time)
                          :visited      *nodes-visited*     
                          :created      *plans-created*
                          :q-len        0
                          :ave-branch   0
                          :unify-count  *unify-count*
                          :rank-unifies *compute-rank-unifies*
                          :add-bindings *add-bind-count*)))
#+:clim-2   (when vcr:*enable-vcr* (pdb-end prob stat plan) (pdb-display))
    (when (>= *plans-created* *search-limit*)
      (format t "~%Warning: Search limit exceeded"))
    (when plan (display-plan plan))
    (display-stat stat)
    (values plan stat)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Making a search controller for UCPOP

(defun flaw-fn (sc p)
  (when (plan-flaws p)
    (do ()
        ((or (null (plan-flaws p))
             (and *cache-flaws* (numberp (flaw-rank (car (plan-flaws p))))))
         (setf (plan-flaws p) 
           (nconc (mapcar #'(lambda (x)
                              (setf (flaw-rank (cdr x)) (car x))
                              (cdr x))
                          (sc:sort-entries :flaw sc))
                  (plan-flaws p))))
      (sc:assertion sc `(:flaw ,(pop (plan-flaws p)))))
    (let* ((f nil)
	   (facts nil))
      (do () (f f)
	(dolist (a (plan-flaws p))
	  (when (and (not (member a facts))
		     (or (not (openc-p a))
			 (> (plan-high-step p) (openc-src-limit a)))
		     (or (null f)
			 (< (flaw-rank a) (flaw-rank f)))) 
	    (setf f a)))
	(when (null f) (return :bad-flaws))
	(when (fact-p f) 
	  (setf (fact-bindings f)
	    (apply (fact-function f) 
		   (mapcar 
		    #'(lambda (x) (bind-variable x (plan-bindings p)))
		    (cdr (fact-condition f)))))
	  (when (eq (fact-bindings f) :no-match-attempted)
	    (push f facts)
	    (setf f nil)))))))

(defun repair-fn (sc n f)
  (incf *nodes-visited*)
  (unless (eq f :bad-flaws)
    (when (fact-p f)
      (setf (fact-bindings f)
	(apply (fact-function f) 
	       (mapcar 
		#'(lambda (x) (bind-variable x (plan-bindings n)))
		(cdr (fact-condition f)))))
      (when (eq :no-match-attempted (fact-bindings f))
	(error "Chose an under specified fact ~a for the current flaw" 
	       (fact-condition f))))
    (dolist (p (new-plans n f))
      (let ((r1 (bind-variable (cdr (assoc :reason (plan-other p))) 
			       (plan-bindings p))))
	(sc:assertion sc `(:node ,p ,r1))))
    (let ((trace (rule-net:dump-firings)))
      (when trace
	(push (cons :trace trace) (plan-other n))))))

(defun reset-controller ()
  (setf *controller* (sc:make-sc :name 'ucpop
		      :flaw-fn #'flaw-fn
		      :repair-fn #'repair-fn))
  (define (clause (operator s op p))
      (bound! 'operator s p)
    (when (and (plan-p p) (numberp s))
      (let ((act (bind-variable (get-operator p s) (plan-bindings p))))
	(sc::match* op (list act)))))
  (define (clause (goal p g term step))
      (bound! 'goal p g)
    (when (openc-p g)
      (sc::match* (list term step) 
		  (list (list (if (plan-p p)
				  (bind-variable (openc-condition g)
						 (plan-bindings p))
				(openc-condition g))
			      (openc-id g))))))
  (define (clause (threat p g l s))
      (bound! 'threat p g)
    (when (unsafe-p g)
      (let* ((link (unsafe-link g))
	     (s1 (link-id1 link))
	     (c (if (plan-p p) 
		    (bind-variable (link-condition link) (plan-bindings p))
		  (link-condition link)))
	     (s2 (link-id2 link))
	     (step (effect-id (unsafe-clobber-effect g))))
	(matchb (list l s) `(((,s1 ,c ,s2) ,step))))))
  (define (clause (univ-threat p g term))
      (bound! 'univ-threat p g)
    (when (univ-threat-p g)
      (sc::match* term
		  (list (bind-variable (univ-threat-term g)
				       (plan-bindings p))))))
  (define (clause (neq p x))
      (bound! 'neq p x)
    (when (null (matchb p (list x))) '(nil)))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Search control utilities

(defun MATCHB (pattern lst &aux (alst nil) (ret nil))
  "Compute the most general unifier between PATTERN and each value in LST.
   Only PATTERN contains $variables.  Returns sets of binding sets."
  (labels 
      ((match* (p l)
         (when (not (eql p l))
           (cond
            ((rule-net:variable$ p)
             (dolist (a alst (push (cons p l) alst))
               (when (eq (car a) p)
                 (if (eql (cdr a) l) (return alst)
                   (throw 'done :fail)))))
            ((or (atom p) (atom l)) (throw 'done :fail))
            (t (match* (car p) (car l))
               (match* (cdr p) (cdr l)))))))
    (dolist (val lst ret)
      (catch 'done
        (setf alst nil)
        (match* pattern val)
        (push alst ret)))))

;;;;;;;;;;;;;;;;
;;; Getting the action name for an ID (for computing reasons)
(defun GET-OPERATOR (plan id)
  (cond ((not (numberp id)) id)
        ((= id 0) ':initial)
        (t (dolist (s (plan-steps plan))
             (when (= (p-step-id s) id) (return (p-step-action s)))))))

(defun bound! (clause &rest args)
  (dolist (arg args)
    (when (rule-net:variable$ arg)
      (error "~%Clause [~a] expects variable [~a] to be bound" clause arg))))
