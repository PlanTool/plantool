(in-package "UCPOP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The eight-puzzle is a game in which you attempt to move the pieces into a
;;; certain arrangement from some random arrangement.  The board is two
;;; dimensional, and you can only move pieces left, right, up, and down into
;;; an adjacent empty square.
;;;
;;; These problems are all completely impossible for UCPOP.
;;; 
;;;                                 --==>> Marc Friedman, October 1995
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The board:
;; S1 S2 S3
;; S4 S5 S6
;; S7 S8 S9

;; The goal:
;; P1 P2 P3
;; P4 P5 P6
;; P7 P8 P9


(define (domain eight-puzzle)
    (:axiom is-empty
     :context (or 
	       (and (eq ?x S1)
		    (eq ?y S2))
	       (and (eq ?x S1)
		    (eq ?y S4))
	       (and (eq ?x S2)
		    (eq ?y S1))
	       (and (eq ?x S2)
		    (eq ?y S3))
	       (and (eq ?x S2)
		    (eq ?y S5))
	       (and (eq ?x S3)
		    (eq ?y S2))
	       (and (eq ?x S3)
		    (eq ?y S6))
	       (and (eq ?x S4)
		    (eq ?y S1))
	       (and (eq ?x S4)
		    (eq ?y S5))
	       (and (eq ?x S4)
		    (eq ?y S7))
	       (and (eq ?x S5)
		    (eq ?y S4))
	       (and (eq ?x S5)
		    (eq ?y S2))
	       (and (eq ?x S5)
		    (eq ?y S6))
	       (and (eq ?x S5)
		    (eq ?y S8))
	       (and (eq ?x S6)
		    (eq ?y S5))
	       (and (eq ?x S6)
		    (eq ?y S3))
	       (and (eq ?x S6)
		    (eq ?y S9))
	       (and (eq ?x S7)
		    (eq ?y S4))
	       (and (eq ?x S7)
		    (eq ?y S8))
	       (and (eq ?x S8)
		    (eq ?y S7))
	       (and (eq ?x S8)
		    (eq ?y S5))
	       (and (eq ?x S8)
		    (eq ?y S9))
	       (and (eq ?x S9)
		    (eq ?y S8))
	       (and (eq ?x S9)
		    (eq ?y S6)))
     :implies (adjacent ?x ?y))
  (:axiom is-done
	 :context (and 
		   (at P1 S1)
		   (at P2 S2)
		   (at P3 S3)
		   (at P4 S4)
		   (at P5 S5)
		   (at P6 S6)
		   (at P7 S7)
		   (at P8 S8)
		   )
	 :implies (solved))
  (:operator slide
	     :parameters (?piece ?from ?to)
	     :precondition (and 
			    (adjacent ?from ?to)
			    (at ?piece ?from)
			    (empty ?to)
			    )
	     :effect (and (empty ?from)
			  (not (empty ?to))
			  (at ?piece ?to)
			  (not (at ?piece ?from)))))

;; 1 2 3         1 2 3
;; 4 5 6  --==>> 4 5 6
;; 7 - 9         7 8 -

(define (problem 8-1)
    :domain 'eight-puzzle
    :inits (
	    (at P1 S1)
	    (at P2 S2)
	    (at P3 S3)
	    (at P4 S4)
	    (at P5 S5)
	    (at P6 S6)
	    (at P7 S7)
	    (at P8 S9)
	    (empty S8))
    :goal (solved))

;; - 8 7         1 2 3
;; 6 5 4  --==>> 4 5 6
;; 3 2 1         7 8 -

(define (problem 8-2)
    :domain 'eight-puzzle
    :inits (
	    (at P1 S9)
	    (at P2 S8)
	    (at P3 S7)
	    (at P4 S6)
	    (at P5 S5)
	    (at P6 S4)
	    (at P7 S3)
	    (at P8 S2)
	    (empty S1))
    :goal (solved))

