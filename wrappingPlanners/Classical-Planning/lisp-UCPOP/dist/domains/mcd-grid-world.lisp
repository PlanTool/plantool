;;; -*- Mode: LISP; Base: 10; Package: UCPOP; Syntax: ANSI-Common-Lisp -*-

;;; Drew McDermott's grid world with locked doors and keys
;;; from his submission to IJCAI95
;;; as encoded / translated by Dave SMith then Dan Weld

(in-package "UCPOP")

(define (domain mcd-grid-world)
    (:operator Go
               :parameters (?x ?y ?nx ?ny)
               :precondition (:and (at-robot ?x ?y)
                                    (path ?x ?y ?nx ?ny)
                                    (:not (:exists (key ?k) 
                                                  (lock ?nx ?ny ?k)))
                                    )
               :effect (:and (:not (at-robot ?x ?y))
                             (at-robot ?nx ?ny)))
  (:operator Open
             :parameters (?x ?y ?nx ?ny)
             :precondition (:and (at-robot ?x ?y)
                                 (path ?x ?y ?nx ?ny)
                                 (:exists (key ?k)
                                     (:and (lock ?nx ?ny ?k)
                                           (has ?k))))
             :effect (:and (:not (at-robot ?x ?y)) 
                           (at-robot ?nx ?ny)))
 
  (:operator Pickup
             :parameters (?k ?x ?y)
             :precondition (:and (At ?k ?x ?y)
                                 (at-robot ?x ?y))
             :effect (:and (:not (at ?k ?x ?y)) 
                           (has ?k)))
  )

(defun generate-paths (xmin ymin xmax ymax) 
  (loop for x from xmin upto xmax nconc
	(loop for y from ymin upto ymax nconc
	      (let ((nx (1+ x))
		    (ny (1+ y)))
		(nconc (when (<= nx xmax)
			 `((path ,x ,y ,nx ,y)
			   (path ,nx ,y ,x ,y)))
		       (when (<= ny ymax)
			 `((path ,x ,y ,x ,ny)
			   (path ,x ,ny ,x ,y))))))))

(defun generate-nots (xmin ymin xmax ymax locks)
  (let ((result nil))
    (dotimes (x (- xmax xmin) result)
      (dotimes (y (- ymax ymin))
        (unless (find-if #'(lambda (s)
                             (and (eq (first s) 'lock)
                                  (eql (second s) x)
                                  (eql (third s) y)))
                         locks)
		  (push `(not (lock ,x ,y ?k)) result))))))

(defparameter *start* '((At-Robot 0 0)))
(defvar *paths42* (generate-paths -4 -2 4 2))
(defvar *paths44* (generate-paths -4 -4 4 4))
(defvar *paths55* (generate-paths -5 -5 5 5))
(defvar *inits55* (append *start* *paths55*
                          (generate-nots -5 -5 5 5 nil)))

(defparameter *locks* '((lock  -4 0 diamond)		; messy left clump
                        (lock  -3 -1 diamond) 
                        (lock  -3 1 diamond)
                        (lock  -2 1 triangle)
                        (lock  -2 0 triangle)
                        (lock  -2 -1 triangle)
                        (at circle  -3 0)          ; circle key
 
                        (lock  -2 -2 triangle)     ; impossible triangle clump
                        (lock  -3 -3 triangle) 
                        (lock  -1 -3 triangle)
                        (lock  -2 -4 triangle)
                        (at triangle  -2 -3)       ; triangle key
                        
                        (lock  3 1 circle)         ; right solution clump
                        (lock  2 0 triangle)
                        (lock  4 0 triangle)
                        (lock  3 -1 triangle)
                        (at diamond  2 1)          ; diamond key
                        (key circle)
                        (key diamond)
                        (key triangle)
                        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These problems hold the solution length steady at 3, but vary the
;;; size of the world map

(def-problem 'Simple42
    :domain ''mcd-grid-world
    :goal '(at-robot 3 0)
    :inits (append *start* *paths42*
                   (generate-nots -4 -2 4 2 nil)))

(def-problem 'Simple44
    :domain  ''mcd-grid-world
    :goal '(at-robot 3 0)
    :inits (append *start* *paths44*
                   (generate-nots -4 -4 4 4 nil)))

(def-problem 'Simple55
    :domain  ''mcd-grid-world
    :goal '(at-robot 3 0)
    :inits *inits55*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These problems hold the world size steady and 
;;; vary the length of path to be run. 

(def-problem 'Runto11
    :domain  ''mcd-grid-world
    :goal '(at-robot 1 1)
    :inits *inits55*)

(def-problem 'Runto12
    :domain  ''mcd-grid-world
    :goal '(at-robot 1 2)
    :inits *inits55*)

(def-problem 'Runto22
    :domain  ''mcd-grid-world
    :goal '(at-robot 2 2)
    :inits *inits55*)

(def-problem 'Runto23
    :domain  ''mcd-grid-world
    :goal '(at-robot 2 3)
    :inits *inits55*)

(def-problem 'Runto33
    :domain  ''mcd-grid-world
    :goal '(at-robot 3 3)
    :inits *inits55*)

(def-problem 'Runto44
    :domain  ''mcd-grid-world
    :goal '(at-robot 4 4)
    :inits *inits55*)

(def-problem 'Runto55
    :domain  ''mcd-grid-world
    :goal '(at-robot 5 5)
    :inits *inits55*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-problem 'Lock42
    :domain  ''mcd-grid-world
    :goal '(at-robot 3 0)
    :inits (append *start* *locks* *paths42*
                   (generate-nots -4 -2 4 2 *locks*)))

(def-problem 'Lock44
    :domain  ''mcd-grid-world
    :goal '(at-robot 3 0)
    :inits (append *start* *locks* *paths44*
                   (generate-nots -4 -4 4 4 *locks*)))

(def-problem 'Lock55
    :domain  ''mcd-grid-world
    :goal '(at-robot 3 0)
    :inits (append *start* *locks* *paths55*
                   (generate-nots -5 -5 5 5 *locks*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Better Encoding 
;;;;
;;;;  Go and Open have been combined into a single operator

(define (domain mcd-grid-world2)
    (:operator Go
               :parameters (?x ?y ?nx ?ny)
               :precondition (:and  (path ?x ?y ?nx ?ny)
                                    (:forall (lock ?nx ?ny ?k)
                                             (has ?k))
                                    (at-robot ?x ?y)
                                    )
               :effect (:and (:not (at-robot ?x ?y)) 
                             (at-robot ?nx ?ny)))
    (:operator Pickup
             :parameters (?k ?x ?y)
             :precondition (:and (At ?k ?x ?y)
                                 (at-robot ?x ?y))
             :effect (:and (:not (at ?k ?x ?y)) 
                           (has ?k)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These problems hold the world size steady and 
;;; vary the length of path to be run. 

(def-problem 'Runto11
    :domain  ''mcd-grid-world2
    :goal '(at-robot 1 1)
    :inits *inits55*)

(def-problem 'Runto12
    :domain  ''mcd-grid-world2
    :goal '(at-robot 1 2)
    :inits *inits55*)

(def-problem 'Runto22a
    :domain  ''mcd-grid-world2
    :goal '(at-robot 2 2)
    :inits *inits55*)

(def-problem 'Runto23
    :domain  ''mcd-grid-world2
    :goal '(at-robot 2 3)
    :inits *inits55*)

(def-problem 'Runto33
    :domain  ''mcd-grid-world2
    :goal '(at-robot 3 3)
    :inits *inits55*)

(def-problem 'Runto44
    :domain  ''mcd-grid-world2
    :goal '(at-robot 4 4)
    :inits *inits55*)

(def-problem 'Runto55
    :domain  ''mcd-grid-world2
    :goal '(at-robot 5 5)
    :inits *inits55*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun fast-grid (prob)
;  (bf-control prob)
;  (fail-link '(put-away $a $b) '(in $a $b) '(fetch $a $b))
;  (fail-link '(close $a) '(:not (open $a)) '(open $a))
;  (fail-link '(jack-down $a) '(:not (jacked $a)) '(jack-up $a))
;  (fail-link '(jack-down $a) '(have jack) '(jack-up $b)))
