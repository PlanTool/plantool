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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  3. This file contains a set of test problems.  Some of them work,
;;;  and others are intractable for ucpop.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain blocks-world-domain)
    ;; Define step for placing one block on another.
    (:operator puton
	      :parameters (?X ?Y ?Z)
	      :precondition (and (on ?X ?Z) (clear ?X) (clear ?Y)
				 (neq ?Y ?Z) (neq ?X ?Z)
				 (neq ?X ?Y) (neq ?X Table))
	      :effect 
	      (and (on ?X ?Y) (not (on ?X ?Z))
		   (when (neq ?Z Table) (clear ?Z))
		   (when (neq ?Y Table) (not (clear ?Y))))))

;;;UCPOP(22): (bf-control 'sussman-anomaly)
;;;
;;;Initial  : ((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK TABLE) (ON C A) (ON A TABLE)
;;;            (ON B TABLE) (CLEAR C) (CLEAR B) (CLEAR TABLE))
;;;
;;;Step 1  : (PUTON C TABLE A)      Created 2 
;;;           0  -> (ON C A)            
;;;           0  -> (CLEAR C)           
;;;           0  -> (CLEAR TABLE)       
;;;Step 2  : (PUTON B C TABLE)      Created 3 
;;;           0  -> (ON B TABLE)        
;;;           0  -> (CLEAR B)           
;;;           0  -> (CLEAR C)           
;;;Step 3  : (PUTON A B TABLE)      Created 1 
;;;           0  -> (ON A TABLE)        
;;;           2  -> (CLEAR A)           
;;;           0  -> (CLEAR B)           
;;;
;;;Goal    : (AND (ON B C) (ON A B))
;;;           3  -> (ON B C)            
;;;           1  -> (ON A B)            
;;;Complete!
;;;
;;;UCPOP (Init = 10 ; Goals = 3 ) => Win  (3 steps)     CPU 283      
;;;     Nodes (V = 51  ; Q = 25  ; C = 82  )             Branch 1.4901961 
;;;     Working Unifies: 481                             Bindings added: 202  
;;;NIL

(define (problem sussman-anomaly)
  :domain 'blocks-world-domain
  :inits ((block A) (block B) (block C) (block Table)
	  (on C A) (on A Table) (on B Table) 
	  (clear C) (clear B) (clear Table))
  :goal (and (on B C) (on A B)))

(define (problem tower-invert3)
    :domain 'blocks-world-domain
    :inits ((block A) (block B) (block C) (block Table)
	    (on a b) (on b c) (on c table)
	    (clear a) (clear table))
    :goal (:and (on b c) (on c a)))

(define (problem tower-invert4)
    :domain 'blocks-world-domain
    :inits ((block A) (block B) (block C) (block D) (block Table)
	    (on a b) (on b c) (on c d) (on d table)
	    (clear a) (clear table))
    :goal (:and (on b c) (on c d) (on d a)))

(defun random-bw-problem (n)
  (make-problem
   :name (format nil "Random ~a block blocksworld problem" n)
   :domain 'blocks-world-domain
   :inits (random-bw-state n)
   :goal (cons :and (random-bw-state n))))

(defun random-bw-state (n &aux (state nil) (ret '((clear table))))
  (labels ((PERMUTE (list)
	     (let ((l (copy-list list))
		   (ret nil))
	       (do ()
		   ((null l) ret)
		 (let ((i (random (length l))))
		   (push (nth i l) ret)
		   (setf l (delete (nth i l) l)))))))
    (dolist (b (permute (subseq '(a b c d e f) 0 n)))
      (let ((r (random (1+ (length state)))))
	(if (zerop r) (push (list b) state)
	  (push b (nth (1- r) state)))))
    (dolist (tower state (permute ret))
      (push `(clear ,(car tower)) ret)
      (do ((b tower (cdr b)))
	  ((null b))
	(push `(on ,(car b) ,(if (cdr b) (cadr b) 'table)) ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; traveling domain

(define (domain road-operators)
  (:operator drive
	     :parameters (?vehicle ?location1 ?location2)
	     :precondition (:and (at ?vehicle ?location1) (road ?location1 ?location2))
	     :effect
	     (:and (at ?vehicle ?location2)
		   (:not (at ?vehicle ?location1))))
  (:operator cross
	     :parameters (?vehicle ?location1 ?location2)
	     :precondition (:and (at ?vehicle ?location1) (bridge ?location1 ?location2))
	     :effect
	     (:and (at ?vehicle ?location2)
		   (:not (at ?vehicle ?location1)))))

(defun add-road (a b)
  `((road ,a ,b) (road ,b ,a)))

(defun add-bridge (a b)
  `((bridge ,a ,b) (bridge ,b ,a)))

;;;UCPOP(23): (bf-control (road-test))
;;;
;;;Initial  : ((VEHICLE JACK) (VEHICLE MARK) (PLACE A) (PLACE D) (PLACE G)
;;;            (AT JACK A) (AT MARK A) (BRIDGE A D) (BRIDGE D A) (ROAD D G)
;;;            (ROAD G D))
;;;
;;;Step 1  : (CROSS JACK A D)       Created 4 
;;;           0  -> (AT JACK A)         
;;;           0  -> (BRIDGE A D)        
;;;Step 2  : (CROSS MARK A D)       Created 2 
;;;           0  -> (AT MARK A)         
;;;           0  -> (BRIDGE A D)        
;;;Step 3  : (DRIVE JACK D G)       Created 3 
;;;           4  -> (AT JACK D)         
;;;           0  -> (ROAD D G)          
;;;Step 4  : (DRIVE MARK D G)       Created 1 
;;;           2  -> (AT MARK D)         
;;;           0  -> (ROAD D G)          
;;;
;;;Goal    : (AND (AT JACK G) (AT MARK G))
;;;           3  -> (AT JACK G)         
;;;           1  -> (AT MARK G)         
;;;Complete!
;;;
;;;UCPOP (Init = 11 ; Goals = 3 ) => Win  (4 steps)     CPU 133      
;;;     Nodes (V = 20  ; Q = 7   ; C = 28  )             Branch 1.35      
;;;     Working Unifies: 177                             Bindings added: 43   
;;;NIL

(defun road-test ()
  (make-problem
   :name 'road-test
   :domain 'road-operators
   :inits `((vehicle jack)(vehicle mark) 
			  (place a)(place d)(place g)
			  (at jack a) (at mark a)
			  ,@(add-bridge 'a 'd) ,@(add-road 'd 'g))
   :goal '(:and (at jack g) (at mark g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tower of hanoi

(define (domain hanoi-domain)
  (:operator move-disk
	     :parameters ((disk ?disk) ?below-disk ?new-below-disk)
	     :precondition (:and (smaller ?disk ?new-below-disk) ;handles pegs!
				 (:neq ?new-below-disk ?below-disk)
				 (:neq ?new-below-disk ?disk)
				 (:neq ?below-disk ?disk)
				 (on ?disk ?below-disk)
				 (clear ?disk)
				 (clear ?new-below-disk))
	     :effect
	     (:and (clear ?below-disk)
		   (on ?disk ?new-below-disk)
		   (:not (on ?disk ?below-disk))
		   (:not (clear ?new-below-disk)))))

;;;UCPOP(24): (bf-control (hanoi 2))
;;;
;;;Initial  : ((SMALLER D1 P1) (SMALLER D2 P1) (SMALLER D1 P2) (SMALLER D2 P2)
;;;            (SMALLER D1 P3) (SMALLER D2 P3) (SMALLER D1 D2) (CLEAR P1)
;;;            (CLEAR P2) (CLEAR D1) (DISK D1) (DISK D2) (ON D1 D2) (ON D2 P3))
;;;
;;;Step 1  : (MOVE-DISK D1 D2 P2)   Created 2 
;;;           0  -> (SMALLER D1 P2)     
;;;           0  -> (ON D1 D2)          
;;;           0  -> (CLEAR D1)          
;;;           0  -> (CLEAR P2)          
;;;           0  -> (DISK D1)           
;;;Step 2  : (MOVE-DISK D2 P3 P1)   Created 1 
;;;           0  -> (SMALLER D2 P1)     
;;;           0  -> (ON D2 P3)          
;;;           2  -> (CLEAR D2)          
;;;           0  -> (CLEAR P1)          
;;;           0  -> (DISK D2)           
;;;Step 3  : (MOVE-DISK D1 P2 D2)   Created 3 
;;;           0  -> (SMALLER D1 D2)     
;;;           2  -> (ON D1 P2)          
;;;           0  -> (CLEAR D1)          
;;;           2  -> (CLEAR D2)          
;;;           0  -> (DISK D1)           
;;;
;;;Goal    : (AND (ON D1 D2) (ON D2 P1))
;;;           3  -> (ON D1 D2)          
;;;           1  -> (ON D2 P1)          
;;;Complete!
;;;
;;;UCPOP (Init = 14 ; Goals = 3 ) => Win  (3 steps)     CPU 184      
;;;     Nodes (V = 32  ; Q = 14  ; C = 50  )             Branch 1.4375    
;;;     Working Unifies: 279                             Bindings added: 84   
;;;NIL

(defun hanoi (n)
  (make-problem 
   :name 'hanoi
   :domain 'hanoi-domain
   :inits
   (let* ((disks (subseq '(d1 d2 d3 d4 d5 d6 d7 d8 d9) 0 n))
	  (sizes (nconc (mapcar #'(lambda (d) `(smaller ,d p1)) disks)
			(mapcar #'(lambda (d) `(smaller ,d p2)) disks)
			(mapcar #'(lambda (d) `(smaller ,d p3)) disks)
			(mapcon
			 #'(lambda (d)
			     (mapcar #'(lambda (d2)
					 `(smaller ,(car d) ,d2))
				     (cdr d)))
			 disks)))
	  (initial (append '((clear p1)(clear p2)(clear d1))
			   (mapcar #'(lambda (d)
				       `(disk ,d)) disks)
			   (maplist
			    #'(lambda (d)
				(if (cdr d)
				    `(on ,(car d) ,(cadr d))
				  `(on ,(car d) p3)))
			    disks))))
     (nconc sizes initial))
   :goal 
   (let* ((disks (subseq '(d1 d2 d3 d4 d5 d6 d7 d8 d9) 0 n)))
     (cons :and (maplist #'(lambda (d)
			     (if (cdr d)
				 `(on ,(car d) ,(cadr d))
			       `(on ,(car d) p1)))
			 disks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ferry domain

(define (domain ferry-domain)
  (:operator board
	     :parameters ((auto ?x)(place ?y))
	     :precondition (:and (at ?x ?y)(at-ferry ?y)(empty-ferry))
	     :effect 
	     (:and (on ?x ferry)
		   (:not (at ?x ?y))
		   (:not (empty-ferry))))
  (:operator sail
	     :parameters ((place ?x)(place ?y))
	     :precondition (:and (at-ferry ?x) (:neq ?x ?y))
	     :effect
	     (:and (at-ferry ?y)
		   (:not (at-ferry ?x))))
  (:operator debark
	     :parameters ((auto ?x)(place ?y))
	     :precondition (:and (on ?x ferry)(at-ferry ?y))
	     :effect
	     (:and (:not (on ?x ferry))
		   (at ?x ?y)
		   (empty-ferry))))

;;;UCPOP(25): (bf-control 'test-ferry)
;;;
;;;Initial  : ((PLACE A) (PLACE B) (AUTO C1) (AUTO C2) (AT C1 A) (AT C2 A)
;;;            (AT-FERRY A) (EMPTY-FERRY))
;;;
;;;Step 1  : (BOARD C2 A)           Created 3 
;;;           0  -> (AT C2 A)           
;;;           0  -> (AT-FERRY A)        
;;;           0  -> (EMPTY-FERRY)       
;;;           0  -> (AUTO C2)           
;;;           0  -> (PLACE A)           
;;;Step 2  : (SAIL A B)             Created 2 
;;;           0  -> (AT-FERRY A)        
;;;           0  -> (PLACE A)           
;;;           0  -> (PLACE B)           
;;;Step 3  : (DEBARK C2 B)          Created 1 
;;;           3  -> (ON C2 FERRY)       
;;;           2  -> (AT-FERRY B)        
;;;           0  -> (AUTO C2)           
;;;           0  -> (PLACE B)           
;;;Step 4  : (SAIL B A)             Created 6 
;;;           2  -> (AT-FERRY B)        
;;;           0  -> (PLACE B)           
;;;           0  -> (PLACE A)           
;;;Step 5  : (BOARD C1 A)           Created 7 
;;;           0  -> (AT C1 A)           
;;;           6  -> (AT-FERRY A)        
;;;           1  -> (EMPTY-FERRY)       
;;;           0  -> (AUTO C1)           
;;;           0  -> (PLACE A)           
;;;Step 6  : (SAIL A B)             Created 5 
;;;           6  -> (AT-FERRY A)        
;;;           0  -> (PLACE A)           
;;;           0  -> (PLACE B)           
;;;Step 7  : (DEBARK C1 B)          Created 4 
;;;           7  -> (ON C1 FERRY)       
;;;           5  -> (AT-FERRY B)        
;;;           0  -> (AUTO C1)           
;;;           0  -> (PLACE B)           
;;;
;;;Goal    : (AND (AT C1 B) (AT C2 B))
;;;           4  -> (AT C1 B)           
;;;           1  -> (AT C2 B)           
;;;Complete!
;;;
;;;UCPOP (Init = 8  ; Goals = 3 ) => Win  (7 steps)     CPU 2633     
;;;     Nodes (V = 488 ; Q = 153 ; C = 786 )             Branch 1.3135246 
;;;     Working Unifies: 2194                            Bindings added: 362  
;;;NIL

(define (problem test-ferry)
    :domain 'ferry-domain
    :inits ((place a) (place b) (auto c1) (auto c2)
	    (at c1 a)(at c2 a)(at-ferry a)
	    (empty-ferry))
    :goal (:and (at c1 b)(at c2 b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; molgen domain

(define (domain molgen-domain)
  ;; steps for building DNA molecules from mRNA
  
  (:operator reverse-transcribe
	     :parameters (?x)
	     :precondition (mRNA ?x)
	     :effect
	     (connected-cDNA-mRNA ?x))
  (:operator separate
	     :parameters (?x)
	     :precondition (connected-cDNA-mRNA ?x)
	     :effect 
	     (:and (single-strand ?x)
		   (:not (connected-cDNA-mRNA ?x))))
  (:operator polymerize
	     :parameters (?x)
	     :precondition (single-strand ?x)
	     :effect
	     (:and (hair-pin ?x)
		   (:not (single-strand ?x))))
  (:operator digest
	     :parameters (?x)
	     :precondition (hair-pin ?x)
	     :effect
	     (:and (double-strand ?x)
		   (:not (hair-pin ?x))))

  ;; steps for splicing DNA molecules
  (:operator ligate
	     :parameters (?x ?y)
	     :precondition (:neq ?x ?y)
	     :effect
	     (:and (:when (:and (double-strand ?y) (:eq ?x LINKER))
		     (cleavable ?y))
		   (:when (:and (cleaved ?x) (cleaved ?y) (:neq ?x LINKER))
		     (:and (contains ?x ?y) (cleavable ?y)
			   (:not (cleaved ?x)) (:not (cleaved ?y))))))
       
  (:operator cleave
	     :parameters (?x)
	     :precondition (cleavable ?x)
	     :effect
	     (:and (cleaved ?x)
		   (:not (cleavable ?x))))
  
  ;; Step for inserting a molecule into an organism
  (:operator transform
	     :parameters (?x (bacterium ?y))
	     :precondition (:and (:neq ?x ?y)
				 (cleavable ?x) ; molecule must be whole
				 (accepts ?x ?y)) ; Is molecule accepted?
	     :effect
	     (:and (contains ?x ?y)
		   (:not (cleavable ?x))))
  
  ;; purify a culture with an antibiotic
  (:operator screen
	     :parameters ((bacterium ?x) ?y (antibiotic ?z))
	     :precondition (:and (:neq ?x ?y) (:neq ?y ?z) (:neq ?x ?z)
				 (resists ?z ?y)(contains ?y ?x))
	     :effect
	     (pure ?x)))

;;;UCPOP(30): (bf-control 'rat-insulin)
;;;
;;;Initial  : ((MOLECULE INSULIN-GENE) (MOLECULE E-COLI-EXOSOME)
;;;            (MOLECULE JUNK-EXOSOME) (MOLECULE LINKER) (BACTERIUM E-COLI)
;;;            (BACTERIUM JUNK) (ANTIBIOTIC ANTIBIOTIC-1) (MRNA INSULIN-GENE)
;;;            (CLEAVABLE E-COLI-EXOSOME) (CLEAVABLE JUNK-EXOSOME)
;;;            (ACCEPTS JUNK-EXOSOME JUNK) (ACCEPTS E-COLI-EXOSOME E-COLI)
;;;            (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME))
;;;
;;;Step 1  : (REVERSE-TRANSCRIBE INSULIN-GENE)   Created 10
;;;           0  -> (MRNA INSULIN-GENE) 
;;;Step 2  : (SEPARATE INSULIN-GENE)   Created 9 
;;;           10 -> (CONNECTED-CDNA-MRNA INSULIN-GENE)
;;;Step 3  : (POLYMERIZE INSULIN-GENE)   Created 8 
;;;           9  -> (SINGLE-STRAND INSULIN-GENE)
;;;Step 4  : (DIGEST INSULIN-GENE)   Created 7 
;;;           8  -> (HAIR-PIN INSULIN-GENE)
;;;Step 5  : (LIGATE LINKER INSULIN-GENE)   Created 6 
;;;           7  -> (DOUBLE-STRAND INSULIN-GENE)
;;;Step 6  : (CLEAVE INSULIN-GENE)   Created 5 
;;;           6  -> (CLEAVABLE INSULIN-GENE)
;;;Step 7  : (CLEAVE E-COLI-EXOSOME)   Created 4 
;;;           0  -> (CLEAVABLE E-COLI-EXOSOME)
;;;Step 8  : (LIGATE INSULIN-GENE E-COLI-EXOSOME)   Created 3 
;;;           5  -> (CLEAVED INSULIN-GENE)
;;;           4  -> (CLEAVED E-COLI-EXOSOME)
;;;Step 9  : (TRANSFORM E-COLI-EXOSOME E-COLI)   Created 2 
;;;           3  -> (CLEAVABLE E-COLI-EXOSOME)
;;;           0  -> (ACCEPTS E-COLI-EXOSOME E-COLI)
;;;           0  -> (BACTERIUM E-COLI)  
;;;Step 10 : (SCREEN E-COLI E-COLI-EXOSOME ANTIBIOTIC-1)   Created 1 
;;;           0  -> (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME)
;;;           2  -> (CONTAINS E-COLI-EXOSOME E-COLI)
;;;           0  -> (BACTERIUM E-COLI)  
;;;           0  -> (ANTIBIOTIC ANTIBIOTIC-1)
;;;
;;;Goal    : (EXISTS ((BACTERIUM ?YGOAL) (MOLECULE ?XGOAL))
;;;           (AND (CONTAINS INSULIN-GENE ?XGOAL) (CONTAINS ?XGOAL ?YGOAL)
;;;            (PURE ?YGOAL)))
;;;           3  -> (CONTAINS INSULIN-GENE E-COLI-EXOSOME)
;;;           2  -> (CONTAINS E-COLI-EXOSOME E-COLI)
;;;           1  -> (PURE E-COLI)       
;;;           0  -> (BACTERIUM E-COLI)  
;;;           0  -> (MOLECULE E-COLI-EXOSOME)
;;;Complete!
;;;
;;;UCPOP (Init = 13 ; Goals = 3 ) => Win  (10 steps)     CPU 6850     
;;;     Nodes (V = 896 ; Q = 203 ; C = 1255)             Branch 1.2265625 
;;;     Working Unifies: 3176                            Bindings added: 1600 
;;;NIL

(define (problem rat-insulin)
    :domain 'molgen-domain
    :inits ((molecule insulin-gene) 
	    (molecule e-coli-exosome) 
	    (molecule junk-exosome) (molecule linker)
	    (bacterium e-coli) (bacterium junk)
	    (antibiotic antibiotic-1)
	    (mRNA insulin-gene)
	    (cleavable e-coli-exosome) 
	    (cleavable junk-exosome) 
	    (accepts junk-exosome junk) 
	    (accepts e-coli-exosome e-coli) 
	    (resists antibiotic-1 e-coli-exosome))
    :goal (:exists (bacterium ?y) 
		   (:exists (molecule ?x)
			    (:and (contains insulin-gene ?x)
				  (contains ?x ?y)
				  (pure ?y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; robot domain (from Hank Wan [hsw@cs.cmu.edu])
(define (domain robot-domain)

  (:operator pickup
	     :parameters ((object ?x) (location ?loc))
	     :precondition (:and (:neq ?x robot) (empty-handed) (at ?x ?loc) (at robot ?loc))
	     :effect
	     (:and (grasping ?x)
		   (:not (empty-handed))))

  (:operator drop
	     :parameters ((object ?x))
	     :precondition (:and (:neq ?x robot) (grasping ?x))
	     :effect
	     (:and (empty-handed)
		   (:not (grasping ?x))))

  (:operator move
	     :parameters ((location ?from) (location ?to))
	     :precondition (:and (:neq ?from ?to) (at robot ?from) (connected ?from ?to))
	     :effect
	     (:and (at robot ?to)
		   (:not (at robot ?from))
		   (:forall (?x)
			    (:when (:and (grasping ?x) (object ?x))
			      (:and (at ?x ?to) (:not (at ?x ?from))))))))

;;;UCPOP(31): (bf-control 'r-test1)
;;;
;;;Initial  : ((LOCATION RM1) (LOCATION RM2) (OBJECT BOX1) (OBJECT BOX2)
;;;            (OBJECT ROBOT) (CONNECTED RM1 RM2) (CONNECTED RM2 RM1)
;;;            (AT BOX1 RM2) (AT BOX2 RM2) (EMPTY-HANDED) (AT ROBOT RM1))
;;;
;;;Step 1  : (MOVE RM1 RM2)         Created 2 
;;;           0  -> (NOT (GRASPING BOX1))
;;;           0  -> (AT ROBOT RM1)      
;;;           0  -> (CONNECTED RM1 RM2) 
;;;           0  -> (LOCATION RM1)      
;;;           0  -> (LOCATION RM2)      
;;;Step 2  : (PICKUP BOX1 RM2)      Created 3 
;;;           0  -> (EMPTY-HANDED)      
;;;           0  -> (AT BOX1 RM2)       
;;;           2  -> (AT ROBOT RM2)      
;;;           0  -> (OBJECT BOX1)       
;;;           0  -> (LOCATION RM2)      
;;;Step 3  : (MOVE RM2 RM1)         Created 1 
;;;           3  -> (GRASPING BOX1)     
;;;           0  -> (OBJECT BOX1)       
;;;           2  -> (AT ROBOT RM2)      
;;;           0  -> (CONNECTED RM2 RM1) 
;;;           0  -> (LOCATION RM2)      
;;;           0  -> (LOCATION RM1)      
;;;
;;;Goal    : (AT BOX1 RM1)
;;;           1  -> (AT BOX1 RM1)       
;;;Complete!
;;;
;;;UCPOP (Init = 11 ; Goals = 3 ) => Win  (3 steps)     CPU 117      
;;;     Nodes (V = 20  ; Q = 11  ; C = 32  )             Branch 1.55      
;;;     Working Unifies: 157                             Bindings added: 25   
;;;NIL

(define (problem r-test1)
    :domain 'robot-domain
    :inits ((location rm1) 
	    (location rm2) 
	    (object box1) (object box2) (object robot)
	    (connected rm1 rm2)
	    (connected rm2 rm1)
	    (at box1 rm2) (at box2 rm2)
	    (empty-handed)
	    (at robot rm1))
    :goal (at box1 rm1))

(define (problem r-test2)
    :domain 'robot-domain
    :inits ((location rm1)
	    (location rm2) 
	    (object box1) (object box2) (object robot)
	    (connected rm1 rm2)
	    (connected rm2 rm1)
	    (at box1 rm2) (at box2 rm2)
	    (empty-handed)
	    (at robot rm1))
    :goal (:and (at box1 rm1) (at box2 rm1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monkey domain (from prodigy)

(define (domain monkey-domain)			; Comment: adding location caused fail
  
  ;; movement and clinbing
  (:operator GO-TO
	     :parameters (?x ?y)
	     :precondition (:and (:neq ?y ?x) (on-floor) (at monkey ?y))
	     :effect (:and (at monkey ?x) (:not (at monkey ?y))))
  
  (:operator CLIMB
	     :parameters (?x)
	     :precondition (:and (at box ?x) (at monkey ?x))
	     :effect (:and (onbox ?x) (:not (on-floor))))
  
  (:operator PUSH-BOX
	     :parameters (?x ?y)
	     :precondition (:and (:neq ?y ?x) (at box ?y) (at monkey ?y) (on-floor))
	     :effect (:and (at monkey ?x) (:not (at monkey ?y))
			   (at box ?x)    (:not (at box ?y))))

  ;; getting bananas
  (:operator GET-KNIFE
	     :parameters (?y)
	     :precondition (:and (at knife ?y) (at monkey ?y))
	     :effect (:and (hasknife) (:not (at knife ?y))))
  
  (:operator GRAB-BANANAS
	     :parameters (?y)
	     :precondition (:and (hasknife) (at bananas ?y) (onbox ?y))
	     :effect (hasbananas))
  
  ;; getting water
  (:operator PICKGLASS
	     :parameters (?y)
	     :precondition (:and (at glass ?y) (at monkey ?y))
	     :effect (:and (hasglass) (:not (at glass ?y))))
  
  (:operator GETWATER
	     :parameters (?y)
	     :precondition (:and (hasglass)
				 (at waterfountain ?y)
				 (at monkey ?y)
				 (onbox ?y))
	     :effect (haswater)))
      
;;;UCPOP(32): (bf-control 'monkey-test1)
;;;
;;;Initial  : ((LOCATION P1) (LOCATION P2) (LOCATION P3) (LOCATION P4)
;;;            (AT MONKEY P1) (ON-FLOOR) (AT BOX P2) (AT BANANAS P3)
;;;            (AT KNIFE P4))
;;;
;;;Step 1  : (GO-TO P4 P1)          Created 5 
;;;           0  -> (ON-FLOOR)          
;;;           0  -> (AT MONKEY P1)      
;;;Step 2  : (GET-KNIFE P4)         Created 6 
;;;           0  -> (AT KNIFE P4)       
;;;           5  -> (AT MONKEY P4)      
;;;Step 3  : (GO-TO P2 P4)          Created 4 
;;;           0  -> (ON-FLOOR)          
;;;           5  -> (AT MONKEY P4)      
;;;Step 4  : (PUSH-BOX P3 P2)       Created 3 
;;;           0  -> (AT BOX P2)         
;;;           4  -> (AT MONKEY P2)      
;;;           0  -> (ON-FLOOR)          
;;;Step 5  : (CLIMB P3)             Created 2 
;;;           3  -> (AT BOX P3)         
;;;           3  -> (AT MONKEY P3)      
;;;Step 6  : (GRAB-BANANAS P3)      Created 1 
;;;           6  -> (HASKNIFE)          
;;;           0  -> (AT BANANAS P3)     
;;;           2  -> (ONBOX P3)          
;;;
;;;Goal    : (HASBANANAS)
;;;           1  -> (HASBANANAS)        
;;;Complete!
;;;
;;;UCPOP (Init = 9  ; Goals = 1 ) => Win  (6 steps)     CPU 850      
;;;     Nodes (V = 66  ; Q = 26  ; C = 103 )             Branch 1.3939394 
;;;     Working Unifies: 875                             Bindings added: 101  
;;;NIL

(define (problem monkey-test1)
    :domain 'monkey-domain
    :inits ((location p1)
	    (location p2)(location p3)(location p4)
	    (at monkey p1)(on-floor)(at box p2)(at bananas p3)
	    (at knife p4))
    :goal (hasbananas))

(define (problem monkey-test2)
    :domain 'monkey-domain
    :inits ((location p1)
	    (location p2)(location p3)
	    (location p4)(location p6)
	    (at monkey p1)(on-floor)
	    (at box p2)
	    (at bananas p3)
	    (at knife p4)
	    (at waterfountain p3)(at glass p6))
    :goal (:and (hasbananas) (haswater)))

(define (problem monkey-test3)
    :domain 'monkey-domain
    :inits ((location p1)
	    (location p2)(location p3)
	    (location p4)(location p6)
	    (at monkey p1)(on-floor)
	    (at box p2)
	    (at bananas p3)
	    (at knife p4)
	    (at waterfountain p5)(at glass p6))
    :goal (:and (hasbananas) (haswater)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jscott's briefcase world

(define (domain briefcase-world)
  ;; purge old domain prior to defining a new domain

  (:operator mov-b
      :parameters (?m ?l)
      :precondition (:and (:neq ?m ?l) (at B ?m))
      :effect (:and (at b ?l) (:not (at B ?m))
		    (:when (in P)
		      (:and (at P ?l) (:not (at P ?m))))
		    (:when (in D)
		      (:and (at D ?l) (:not (at D ?m))))))

  (:operator take-out
      :parameters (?x)
      :precondition (:neq ?x B)
      :effect (:not (in ?x)))
      
  (:operator put-in
      :parameters (?x ?l)
      :precondition (:neq ?x B)
      :effect (:when (:and (at ?x ?l) (at B ?l))
		(in ?x))))

;;;UCPOP(33): (bf-control 'get-paid)
;;;
;;;Initial  : ((PLACE HOME) (PLACE OFFICE) (OBJECT P) (OBJECT D) (OBJECT B)
;;;            (AT B HOME) (AT P HOME) (AT D HOME) (IN P))
;;;
;;;Step 1  : (PUT-IN D HOME)        Created 3 
;;;           0  -> (AT D HOME)         
;;;           0  -> (AT B HOME)         
;;;Step 2  : (TAKE-OUT P)           Created 2 
;;;Step 3  : (MOV-B HOME OFFICE)    Created 1 
;;;           3  -> (IN D)              
;;;           0  -> (AT B HOME)         
;;;           2  -> (NOT (IN P))        
;;;
;;;Goal    : (AND (AT B OFFICE) (AT D OFFICE) (AT P HOME))
;;;           1  -> (AT B OFFICE)       
;;;           1  -> (AT D OFFICE)       
;;;           0  -> (AT P HOME)         
;;;Complete!
;;;
;;;UCPOP (Init = 9  ; Goals = 4 ) => Win  (3 steps)     CPU 134      
;;;     Nodes (V = 20  ; Q = 10  ; C = 31  )             Branch 1.5       
;;;     Working Unifies: 278                             Bindings added: 37   
;;;NIL

(define (problem get-paid)
    :domain 'briefcase-world
    :inits ((place home)
	    (place office) 
	    (object p) (object d)(object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at B office) (at D office) (at P home)))

(define (problem get-paid2)
    :domain 'briefcase-world
    :inits ((place home)
	    (place office) 
	    (object p) (object d) (object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at P home) (at D office) (at B home)))

(define (problem get-paid3)
    :domain 'briefcase-world
    :inits ((place home)
	    (place office) (place bank)
	    (object p) (object d) (object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at P bank) (at D office) (at B home)))

(define (problem get-paid4)
    :domain 'briefcase-world
    :inits ((place home)
	    (place office) (place bank)
	    (object p) (object d) (object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at B home) (at D office) (at P bank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flat-tire domain (from Stuart Russell)


(define (domain init-flat-tire)
  
  (:operator cuss
      :effect (:not (annoyed)))
  
  (:operator open
      :parameters ((container ?x))
      :precondition (:and (:not (locked ?x)) (:not (open ?x)))
      :effect (open ?x))
  
  (:operator close
      :parameters ((container ?x))
      :precondition (open ?x)
      :effect (:not (open ?x)))
  
  (:operator fetch
      :parameters (?x (container ?y))
      :precondition (:and (:neq ?x ?y) (in ?x ?y) (open ?y))
      :effect (:and (have ?x) 
		    (:not (in ?x ?y))))
  
  (:operator put-away
      :parameters (?x (container ?y))
      :precondition (:and (:neq ?x ?y) (have ?x) (open ?y))
      :effect (:and (in ?x ?y)
		    (:not (have ?x))))
  
  (:operator loosen
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (have wrench) (tight ?x ?y) 
			  (on-ground ?y))
      :effect (:and (loose ?x ?y)
		    (:not (tight ?x ?y))))
  
  (:operator tighten
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (have wrench) (loose ?x ?y) 
			  (on-ground ?y))
      :effect (:and (tight ?x ?y)
		    (:not (loose ?x ?y))))

  (:operator jack-up
      :parameters ((hub ?y))
      :precondition (:and (on-ground ?y) (have jack))
      :effect (:and (:not (on-ground ?y))
		    (:not (have jack))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:operator jack-down
      :parameters ((hub ?x))
      :precondition (:not (on-ground ?x))
      :effect (:and (on-ground ?x)
		    (have jack)))
  
  (:operator undo
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) 
			  (:not (on-ground ?y)) (:not (unfastened ?y))
			  (have wrench) (loose ?x ?y))
      :effect (:and (have ?x) (unfastened ?y)
		    (:not (on ?x ?y)) (:not (loose ?x ?y))))
  
  (:operator do-up
      :parameters ((nut ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y)
			  (have wrench) (unfastened ?y)
			  (:not (on-ground ?y)) (have ?x))
      :effect
      (:and (loose ?x ?y) (:not (unfastened ?y)) (:not (have ?x))))

  (:operator remove-wheel
      :parameters ((wheel ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (:not (on-ground ?y))
			  (on ?x ?y) (unfastened ?y))
      :effect (:and (have ?x) (free ?y) (:not (on ?x ?y))))
  
  (:operator put-on-wheel
      :parameters ((wheel ?x) (hub ?y))
      :precondition (:and (:neq ?x ?y) (have ?x) (free ?y) (unfastened ?y)
			  (:not (on-ground ?y)))
      :effect
      (:and (on ?x ?y) (:not (have ?x)) (:not (free ?y))))
  
  (:operator inflate
      :parameters ((wheel ?x))
      :precondition (:and (have pump) (:not (inflated ?x)) (intact ?x))
      :effect (inflated ?x)))

(define (problem fixit)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (in jack boot) (in pump boot)
	    (in wheel2 boot) (in wrench boot) 
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (:and 
	   (:not (open boot)) (in jack boot) (in pump boot)
	   (in wheel1 boot) (in wrench boot)
	   (tight nuts hub) (inflated wheel2)(on wheel2 hub)))

(define (problem fix1)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (in jack boot) (in pump boot)
	    (in wheel2 boot) (in wrench boot) 
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (:and (have jack) (have pump) (have wheel2)
		(have wrench)))

(define (problem fix2)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (open boot)
	    (have jack) (have pump) (have wheel2) (have wrench)
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (:and
	   (inflated wheel2) (:not (on-ground hub))
	   (loose nuts hub)))

(define (problem fix3)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts)
	    (container boot) (intact wheel2)
	    (have pump) (have wheel2)
	    (have wrench) (on wheel1 hub) (inflated wheel2)
	    (loose nuts hub))
    :goal (:and (tight nuts hub) (on-ground hub)
		(on wheel2 hub) 
		))

(define (problem fix4)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts)
	    (container boot) (intact wheel2)
	    (have jack) (have pump) (have wheel1)
	    (have wrench) (open boot)
	    (inflated wheel2) 
	    (on wheel2 hub) 
	    (tight nuts hub) (on-ground hub)
	    )
    :goal (:and
	   (in jack boot) (in pump boot) (in wheel1 boot)
	   (in wrench boot) (inflated wheel2) (on wheel2 hub) 
	   (tight nuts hub)))

(define (problem fix5)
    :domain 'init-flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot)
	    (open boot) (in jack boot) (in pump boot)
	    (in wheel1 boot)
	    (in wrench boot) (inflated wheel2) (on wheel2 hub) 
	    (tight nuts hub))
    :goal (:and
	   (:not (open boot)) (in jack boot) (in pump boot)
	   (in wheel1 boot)
	   (in wrench boot) (inflated wheel2) (on wheel2 hub) 
	   (tight nuts hub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here lies an encoding of Pednault's IJCAI-91 example
;;
;;  You bought a house and discover that after turning on water,
;;  it pours out of holes in the wall.  Using the three actions from
;;  below, the homeowner's problem is to find a way to have the water
;;  on without having all those holes in the wall.
;;
;;  1) A "Fixing the wall" action is only effective when the
;;     plumbing is good.
;;  2) A "Fixing the plumbing" action is only good when the water
;;     is off.
;;  2) A "Turning the faucet" action will bash the wall only
;;     when the plumbing is bad and when you turn it to "ON".
;;
;;  The first two actions are encoded as (FIX ?it).
;;  The second is (TURN-FAUCET ?how).
;;
;; J. Scott Penberthy 3/92
;;

(define (domain ho-world)
  ;; purge old domain prior to defining a new domain

  ;; a FIX operator -- a handyman can do anything, within limits

  (:operator fix
      :parameters ((object ?it))
      :effect
      (:and (:when (:and (:eq ?it wall) (good-plumbing))
	      (:not (holey-walls)))
	    (:when (:and (:eq ?it wall) (:not (good-plumbing)) (water off))
	      (:not (holey-walls)))
	    (:when (:and (:eq ?it plumbing) (water off))
	      (good-plumbing))))

  ;; another operator for turning the water on/off

  (:operator turn-faucet
      :parameters (?how)
      :effect
      (:and (water ?how)
	    (:forall (?s)
		     (:when (:and (:neq ?s ?how) (water ?s))
		       (:not (water ?s))))
	    (:when (:and (:eq ?how ON) (:not (good-plumbing)))
	      (holey-walls)))))

;;;UCPOP(40): (bf-control 'ho-demo)
;;;
;;;Initial  : ((OBJECT WALL) (OBJECT PLUMBING) (HOLEY-WALLS) (WATER ON))
;;;
;;;Step 1  : (TURN-FAUCET OFF)      Created 3 
;;;Step 2  : (FIX PLUMBING)         Created 2 
;;;           3  -> (WATER OFF)         
;;;           0  -> (OBJECT PLUMBING)   
;;;Step 3  : (TURN-FAUCET ON)       Created 4 
;;;Step 4  : (FIX WALL)             Created 1 
;;;           2  -> (GOOD-PLUMBING)     
;;;           0  -> (OBJECT WALL)       
;;;
;;;Goal    : (AND (WATER ON) (NOT (HOLEY-WALLS)))
;;;           4  -> (WATER ON)          
;;;           1  -> (NOT (HOLEY-WALLS)) 
;;;Facts:
;;;Complete!
;;;
;;;UCPOP Stats: Initial terms = 4 ;   Goals = 3 ;  Success (4 steps)
;;;      Created 64 plans, but explored only 42
;;;      CPU time:    0.1340 sec
;;;      Branching factor:  1.143
;;;      Working Unifies: 72  
;;;      Bindings Added: 25  
;;;#plan<S=5; O=0; U=0>
;;;#Stats:<cpu time = 0.1340>

(define (problem ho-demo)
    :domain 'ho-world
    :inits ((object wall) (object plumbing) (holey-walls) (water on))
    :goal (:and (water on) (:not (holey-walls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dan's fridge domain
(define (domain fridge-domain)

  (:operator unscrew
	     :parameters ((screw ?x) (backplane ?y))
	     :precondition (:and (screwed ?X) (holds ?x ?y) )
	     :effect (:not (screwed ?X)))
  (:operator screw
	     :parameters ((screw ?x) (backplane ?y))
	     :precondition (:and (:not (screwed ?X)) (holds ?x ?y))
	     :effect (screwed ?X))
  (:operator remove-backplane
	     :parameters ((backplane ?x) ?f ?a ?b ?c ?d)
	     :precondition (:and (:neq ?a ?b) (:neq ?a ?c) (:neq ?a ?d)
				 (:neq ?b ?c) (:neq ?b ?d) (:neq ?c ?d)
				 (in-place ?x) (part-of ?x ?f) (:not (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x)  
				 (holds ?c ?x)  (holds ?d ?x)
				 (:not (screwed ?a)) (:not (screwed ?b)) 
				 (:not (screwed ?c)) (:not (screwed ?d)))
	     :effect (:not (in-place ?X)))
  (:operator attach-backplane
	     :parameters ((backplane ?x) ?f ?a ?b ?c ?d)
	     :precondition (:and (:neq ?a ?b) (:neq ?a ?c) (:neq ?a ?d)
				 (:neq ?b ?c) (:neq ?b ?d) (:neq ?c ?d)
				 (:not (in-place ?x))
				 (part-of ?x ?f) (:not (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x) 
				 (holds ?c ?x)  (holds ?d ?x)
				 (:not (screwed ?a)) (:not (screwed ?b))
				 (:not (screwed ?c)) (:not (screwed ?d)))
	     :effect (in-place ?X))
  (:operator start-fridge
	     :parameters (?f ?a ?b ?c ?d ?x)
	     :precondition (:and (:neq ?a ?b) (:neq ?a ?c) (:neq ?a ?d)
				 (:neq ?b ?c) (:neq ?b ?d) (:neq ?c ?d)
				 (backplane ?x) (in-place ?x) (part-of ?x ?f)
				 (holds ?a ?x)(holds ?b ?x)(holds ?c ?x)(holds ?d ?x)
				 (screwed ?a) (screwed ?b) (screwed ?c) (screwed ?d)
				 (:not (fridge-on ?f)))
	     :effect (fridge-on ?f))
  (:operator stop-fridge
	     :parameters (?f)
	     :precondition (fridge-on ?f)
	     :effect
	     (:not (fridge-on ?f)))
  (:operator change-compressor
	     :parameters (?x ?y ?a)
	     :precondition (:and (:neq ?x ?y) (backplane ?a) (:not (in-place ?a))
				 (covers ?a ?x)
				 (compressor ?x) (compressor ?y) 
				 (attached ?x) (:not (attached ?y)))
	     :effect (:and (:not (attached ?X)) (attached ?y)
			   (:not (covers ?a ?x)) (covers ?a ?y))))

(define (problem fixa)
    :domain 'fridge-domain
    :inits ((screw s1)
	    (screw s2) (screw s3) (screw s4) 
	    (backplane b1)
	    (compressor c1) (compressor c2) (fridge f1)
	    (covers b1 c1) (part-of b1 f1)
	    (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	    (holds s4 b1)
	    (ok c1) (ok c2) (fridge-on f1)
	    (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	    (in-place b1) (attached c1))
    :goal (:and (attached c2) (ok c2)))

(define (problem fixb)
    :domain 'fridge-domain
    :inits ((screw s1)
	    (screw s2) (screw s3) (screw s4) 
	    (backplane b1)
	    (compressor c1) (compressor c2) (fridge f1)
	    (covers b1 c1) (part-of b1 f1)
	    (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	    (holds s4 b1)
	    (ok c1) (ok c2) (fridge-on f1)
	    (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	    (in-place b1) (attached c1))
    :goal (:and (attached c2) (ok c2) (fridge-on f1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  McDermott blocks world

(define (domain mcd-blocksworld)
  (:operator puton
	     :parameters (?x ?y ?d)
	     :precondition (and (neq ?x ?y) (neq ?x table) (neq ?d ?y) 
				(on ?x ?d) 
				(or (eq ?x Table)
				    (forall (block ?b) (not (on ?b ?x))))
				(or (eq ?y Table)
				    (forall (block ?b) (not (on ?b ?y)))))
	     :effect
	     (and (on ?x ?y) (not (on ?x ?d))
		  (forall (?c) 
			  (when (or (eq ?y ?c) (above ?y ?c)) 
			    (above ?x ?c)))
		  (forall (?e) 
			  (when (and (above ?x ?e) (neq ?y ?e) (not (above ?y ?e)))
			    (not (above ?x ?e)))))))

;;;UCPOP(41): (bf-control 'mcd-sussman-anomaly)
;;;
;;;Initial  : ((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK TABLE) (ON C A) (ON B TABLE)
;;;            (ON A TABLE))
;;;
;;;Step 1  : (PUTON C TABLE A)      Created 2 
;;;           0  -> (ON C A)            
;;;           0  -> (NOT (ON TABLE C))  
;;;           0  -> (NOT (ON C C))      
;;;           0  -> (NOT (ON B C))      
;;;           0  -> (NOT (ON A C))      
;;;Step 2  : (PUTON B C TABLE)      Created 3 
;;;           0  -> (ON B TABLE)        
;;;           0  -> (NOT (ON TABLE B))  
;;;           0  -> (NOT (ON C B))      
;;;           0  -> (NOT (ON B B))      
;;;           0  -> (NOT (ON A B))      
;;;           0  -> (NOT (ON TABLE C))  
;;;           0  -> (NOT (ON C C))      
;;;           0  -> (NOT (ON B C))      
;;;           0  -> (NOT (ON A C))      
;;;Step 3  : (PUTON A B TABLE)      Created 1 
;;;           0  -> (ON A TABLE)        
;;;           0  -> (NOT (ON TABLE A))  
;;;           2  -> (NOT (ON C A))      
;;;           0  -> (NOT (ON B A))      
;;;           0  -> (NOT (ON A A))      
;;;           0  -> (NOT (ON TABLE B))  
;;;           0  -> (NOT (ON C B))      
;;;           0  -> (NOT (ON B B))      
;;;           0  -> (NOT (ON A B))      
;;;
;;;Goal    : (AND (ON B C) (ON A B))
;;;           3  -> (ON B C)            
;;;           1  -> (ON A B)            
;;;Complete!
;;;
;;;UCPOP (Init = 7  ; Goals = 3 ) => Win  (3 steps)     CPU 400      
;;;     Nodes (V = 54  ; Q = 25  ; C = 101 )             Branch 1.462963  
;;;     Working Unifies: 976                             Bindings added: 163  
;;;NIL

(define (problem mcd-sussman-anomaly)
    :domain 'mcd-blocksworld
    :inits ((block a) (block b) (block c) (block Table)
		      (on c a) (on b table) (on a table))
    :goal (and (on b c) (on a b)))


(define (problem mcd-tower-invert)
    :domain 'mcd-blocksworld
    :inits ((block A) (block B) (block C) (block D) (block E) (block Table)
		      (clear a) (on a b) (on b c) (on c d) (on d e)(on e table)
		      (clear table))
    :goal (:and (on b c) (on c d) (on d e) (on e a)))

(define (domain  mcd-blocksworld-axiom)
  (:axiom is-clear
	  :context (or (eq ?x Table)
		       (not (exists (on ?b ?x))))
	  :implies (clear ?x))
  (:operator puton
	     :parameters (?x ?y ?d)
	     :precondition (and (neq ?x ?y) (neq ?x table) (neq ?d ?y) 
				(on ?x ?d) (clear ?x) (clear ?y))
	     :effect
	     (and (on ?x ?y) (not (on ?x ?d))
		  (forall (?c)
			  (when (or (eq ?y ?c) (above ?y ?c))
			    (above ?x ?c)))
		  (forall (?e)
			  (when (and (above ?x ?e) (neq ?y ?e) (not (above ?y ?e)))
			    (not (above ?x ?e)))))))

(define (problem mcd-sussman)
    :domain 'mcd-blocksworld-axiom
    :inits ((block a) (block b) (block c) (block Table)
	    (on c a) (on b table) (on a table))
    :goal (and (on b c) (on a b)))

(define (problem mcd-tower)
    :domain 'mcd-blocksworld-axiom
    :inits ((block A) (block B) (block C) (block Table)
	    (clear a) (on a b) (on b c) (on c table)
	    (clear table))
    :goal (:and (on b c) (on c a)))

(define (domain uni-bw)

  (:operator mov-b
	     :parameters (?m ?l)
	     :precondition (:and (at B ?m) (:neq ?m ?l))
	     :effect
	     (:and (at b ?l) (:not (at B ?m))
		   (:forall (?z)
			    (:when (:and (in ?z) (:neq ?z B))
			      (:and (at ?z ?l)  (:not (at ?z ?m)))))))
    
  (:operator take-out
	     :parameters (?x)
	     :precondition (:neq ?x B)
	     :effect
	     (:not (in ?x)))
      
  (:operator put-in
	     :parameters (?x ?l)
	     :precondition (:neq ?x B)
	     :effect
	     (:when (:and (at ?x ?l) (at B ?l))
	       (in ?x))))

;;;UCPOP(42): (bf-control 'uget-paid)
;;;
;;;Initial  : ((AT B HOME) (AT P HOME) (AT D HOME) (IN P))
;;;
;;;Step 1  : (PUT-IN D HOME)        Created 3 
;;;           0  -> (AT D HOME)         
;;;           0  -> (AT B HOME)         
;;;Step 2  : (TAKE-OUT P)           Created 2 
;;;Step 3  : (MOV-B HOME OFFICE)    Created 1 
;;;           3  -> (IN D)              
;;;           0  -> (AT B HOME)         
;;;           2  -> (NOT (IN P))        
;;;
;;;Goal    : (AND (AT B OFFICE) (AT D OFFICE) (AT P HOME))
;;;           1  -> (AT B OFFICE)       
;;;           1  -> (AT D OFFICE)       
;;;           0  -> (AT P HOME)         
;;;Complete!
;;;
;;;UCPOP (Init = 5  ; Goals = 4 ) => Win  (3 steps)     CPU 150      
;;;     Nodes (V = 23  ; Q = 11  ; C = 44  )             Branch 1.4782609 
;;;     Working Unifies: 254                             Bindings added: 65   
;;;NIL

(define (problem uget-paid)
    :domain 'uni-bw
    :inits ((at B home) (at P home) (at D home) (in P) )
    :goal (:and (at B office) (at D office) (at P home)))
      
(define (problem uget-paid2)
    :domain 'uni-bw
    :inits ((place home)(place office) (object p)(object d)(object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at P home) (at D office) (at B home)))

(define (problem uget-paid3)
    :domain 'uni-bw
    :inits ((place home)(place office)(place bank)
	    (object p)(object d)(object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at P bank) (at D office) (at B home)))

(define (problem uget-paid4)
    :domain 'uni-bw
    :inits ((place home)(place office)(place bank)
	    (object p)(object d)(object b)
	    (at B home) (at P home) (at D home) (in P))
    :goal (:and (at B home) (at D office) (at P bank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sched world

(define (domain sched-world-domain2)
  
  (:operator POLISH
	     :parameters (?x)
	     :precondition (temperature ?x cold) 
	     :effect
	     (:and (surface-condition ?x polished)
		   (:forall (?oldsurf)
			    (:when (:neq ?oldsurf polished)
			      (:not (surface-condition ?x ?oldsurf))))))
  
  (:operator ROLL
	     :parameters (?x)
	     :effect
	     (:and (temperature ?x hot)
		   (shape ?x cylindrical)
		   (painted ?x nil)
		   (surface-condition ?x smooth)
		   (:forall (?old)
		     (:and 
		      (:when (:neq ?old nil)
			(:not (painted ?x ?old)))
		      (:when (:neq ?old cylindrical)
			(:not (shape ?x ?old)))
		      (:when (:neq ?old hot)
			(:not (temperature ?x ?old)))
		      (:when (:neq ?old smooth)
			(:not (surface-condition ?x ?old)))))
		   (:forall (?oldwidth ?old-orient)
			    (:not (has-hole ?x ?oldwidth ?old-orient)))))
		      
  (:operator LATHE
	     :parameters (?x)
	     :effect
	     (:and (surface-condition ?x rough)
		   (shape ?x cylindrical)
		   (painted ?x nil)
		   (:forall (?old)
			    (:and 
			     (:when (:neq ?old nil)
			       (:not (painted ?x ?old)))
			     (:when (:neq ?old cylindrical)
			       (:not (shape ?x ?old)))
			     (:when (:neq ?old rough)
			       (:not (surface-condition ?x ?old)))))))

  (:operator GRIND
	     :parameters (?x)
	     :effect
	     (:and (surface-condition ?x smooth)
		   (painted ?x nil)
		   (:forall (?old)
			    (:and 
			     (:when (:neq ?old nil)
			(:not (painted ?x ?old)))
			     (:when (:neq ?old smooth)
			       (:not (surface-condition ?x ?old)))))))

  (:operator PUNCH
	     :parameters (?x ?width ?orient)
	     :precondition (temperature ?x cold)
	     :effect
	     (:and (has-hole ?x ?width ?orient)
		   (surface-condition ?x rough)
		   (:forall (?oldsurf)
			    (:when (:neq ?oldsurf rough)
			      (:not (surface-condition ?x ?oldsurf))))))

  (:operator DRILL-PRESS
	     :parameters (?x ?width ?orient)
	     :precondition (:and (temperature ?x cold)
				 (have-bit ?width))
	     :effect
	     (has-hole ?x ?width ?orient))

  (:operator SPRAY-PAINT
	     :parameters (?x ?paint)
	     :precondition (:and (temperature ?x cold)
				 (sprayable ?paint))
	     :effect 
	     (painted ?x ?paint))

  (:operator IMMERSION-PAINT
	     :parameters (?x ?paint)
	     :precondition (have-paint-for-immersion ?paint)
	     :effect 
	     (painted ?x ?paint)))

(defun sched-world-prob (time i-state goal)
  (dotimes (i time)
    (dolist (machine '(polisher roller lathe grinder punch drill-press
                       spray-painter immersion-painter))
      (push `(idle ,machine ,i) i-state)))
  (dolist (size '(.1 .2 .3))
    (push `(have-bit ,size) i-state))
  (dolist (color '(red black))
    (push `(have-paint-for-immersion ,color) i-state)
    (push `(sprayable ,color) i-state))
  (dotimes (i time)
    (dotimes (j i)
      (push `(later ,i ,j) i-state)))
  (make-problem :domain 'sched-world-domain2
		:inits i-state
		:goal goal))

(defun sched-test1a ()
  (sched-world-prob 
   5 '((shape obj-A oblong)
       (temperature obj-A cold)
       (surface-condition obj-A rough)
       (painted obj-A none)
       (has-hole obj-A 0 nil)
       (last-scheduled obj-A 0)
       (shape obj-B cylindrical)
       (temperature obj-B cold)
       (surface-condition obj-B smooth)
       (painted obj-B red)
       (has-hole obj-B 0 nil)
       (last-scheduled obj-B 0))
   '(:and (shape Obj-A cylindrical)
     (surface-condition Obj-B polished))))

(defun sched-test2a ()
  (sched-world-prob 
   5 '((shape obj-A rectangular)
       (temperature obj-A cold)
       (surface-condition obj-A rough)
       (painted obj-A blue)
       (has-hole obj-A .2 front)
       (last-scheduled obj-A 0)
       
       (shape obj-B flat)
       (temperature obj-B cold)
       (surface-condition obj-B polished)
       (painted obj-B nil)
       (has-hole obj-B 0 nil)
       (last-scheduled obj-B 0)
       
       (shape obj-C oblong)
       (temperature obj-C cold)
       (surface-condition obj-C rough)
       (painted obj-C nil)
       (has-hole obj-C 0 nil)
       (last-scheduled obj-C 0))
   '(:and 
     (surface-condition Obj-B polished)
     (surface-condition Obj-A smooth)
     (shape Obj-A cylindrical))))

(define (domain prodigy-bw)
  (:operator pick-up
	     :parameters ((object ?ob1))
	     :precondition (:and (clear ?ob1) (on-table ?ob1) (arm-empty))
	     :effect
	     (:and (:not (on-table ?ob1))
		   (:not (clear ?ob1))
		   (:not (arm-empty))
		   (holding ?ob1)))
  (:operator put-down
	     :parameters ((object ?ob))
	     :precondition (holding ?ob)
	     :effect
	     (:and (:not (holding ?ob))
		   (clear ?ob)
		   (arm-empty)
		   (on-table ?ob)))
  (:operator stack
	     :parameters ((object ?sob)(object ?sunderob))
	     :precondition (:and (holding ?sob) (clear ?sunderob))
	     :effect
	     (:and (:not (holding ?sob))
		   (:not (clear ?sunderob))
		   (clear ?sob)
		   (arm-empty)
		   (on ?sob ?sunderob)))
  (:operator unstack
	     :parameters ((object ?sob)(object ?sunderob))
	     :precondition (:and (on ?sob ?sunderob) (clear ?sob) (arm-empty))
	     :effect
	     (:and (holding ?sob)
		   (clear ?sunderob)
		   (:not (clear ?sob))
		   (:not (arm-empty))
		   (:not (on ?sob ?sunderob)))))

(define (problem prodigy-sussman)
    :domain 'prodigy-bw
    :inits ((object a)
	    (object b) (object c) 
	    (on-table a) (on-table b) (on c a)
	    (clear b) (clear c) (arm-empty))
    :goal (:and (on a b) (on b c)))

(define (problem prodigy-p22)
    :domain 'prodigy-bw
    :inits ((object B)
	    (clear A)
	    (clear B) (on-table B)
	    (object G) (on-table G)
	    (object F) (on F G)
	    (object E) (on E F)
	    (object D) (on D E)
	    (object C) (on C D) (clear C)
	    (object A) (on-table A) 
	    (arm-empty))
    :goal (:and (on B C) (on-table A) (on F A) (on C D)))

(define (domain strips-world)
    
    (:fact (loc-in-room ?x ?y ?room)
	   (labels 
	       ((convert-loc-to-room (x y)
		  (cond ((and (<= y 5) (>= y 1))
			 (cond ((and (<= x 4) (>= x 3))   'rpdp)
			       ((and (<= x 9) (>= x 5))   'rclk)
			       ((and (<= x 12) (>= x 10)) 'rril)
			  (t (error "Invalid x room coordinate"))))
			((and (<= y 10) (>= y 6))
			 (cond ((and (<= x 2) (>= x 1))   'runi)
			       ((and (<= x 6) (>= x 3))   'rmys)
			       ((and (<= x 9) (>= x 7))   'rram)
			       ((and (<= x 12) (>= x 10)) 'rhal)
			       (t (error "Invalid x room coordinate"))))
			(t (error "Invalid y room coordinate")))))
	     (cond ((or (variable? ?x) (variable? ?y))
		    :no-match-attempted)
		   ((variable? ?room) 
		    `(((,?room ,(convert-loc-to-room ?x ?y)))))
		   ((atom ?room)
		    (when (eq ?room (convert-loc-to-room ?x ?y))
		      '(nil)))
		   (t (error "Room is not a variable or an atom")))))
  
  (:operator GOTO-BOX
	     :parameters (?box ?room)
	     :precondition 
	     (:and (is-type ?box object) ;[6]
		   (in-room ?box ?room)	;[5]
		   (in-room robot ?room)) ;[5]
	     :effect (:and (:forall (?1 ?2) 
				    (:when (at robot ?1 ?2) 
				      (:not (at robot ?1 ?2))))
			   (:forall (?1)
				    (:when (:and (:neq ?1 ?box) (next-to robot ?1))
				      (:not (next-to robot ?1))))
			   (next-to robot ?box)))
  
  (:operator GOTO-DOOR
	     :parameters (?door ?roomx ?roomy)
	     :precondition 
	     (:and (is-type ?door door)	;[6]
		   (connects ?door ?roomx ?roomy) ;[6]
		   (in-room robot ?roomx)) ;[5]
	     :effect (:and (:forall (?1 ?2) 
				    (:when (at robot ?1 ?2)
				      (:not (at robot ?1 ?2))))
			   (:forall (?1)
				    (:when (:and (:neq ?1 ?door) (next-to robot ?1))
				      (:not (next-to robot ?1))))
			   (next-to robot ?door)))
  
  (:operator GOTO-LOC
	     :parameters (?x ?y ?roomx)
	     :precondition 
	     (:and (loc-in-room ?x ?y ?roomx) ;[6]
		   (in-room robot ?roomx)) ;[5]
	     :effect (:and (:forall (?1 ?2) 
				    (:when (at robot ?1 ?2)
				      (:not (at robot ?1 ?2))))
			   (:forall (?1)
				    (:when (next-to robot ?1)
				      (:not (next-to robot ?1))))
			   (at robot ?x ?y)))
  
  (:operator PUSH-BOX
	     :parameters (?boxx ?boxy ?roomx)
	     :precondition 
	     (:and (is-type ?boxy object) ;[6]
		   (pushable ?boxx)	;[6]
		   (in-room ?boxx ?roomx) ;[5]
		   (in-room ?boxy ?roomx) ;[5]
		   (in-room robot ?roomx) ;[5]
		   (next-to robot ?boxx)) ;[1]
	     :effect
	     (:and (:forall (?1 ?2) 
			    (:and (:when (at robot ?1 ?2)
				    (:not (at robot ?1 ?2)))
				  (:when (at ?boxx ?1 ?2)
				    (:not (at ?boxx ?1 ?2)))))
		   (:forall (?1)
			    (:and (:when (:and (:neq ?1 ?boxx) (next-to robot ?1)) 
				    (:not (next-to robot ?1)))
				  (:when (:and (:neq ?1 ?boxy)  (next-to ?boxx ?1))
				    (:not (next-to ?boxx ?1)))
				  (:when (:and (:neq ?1 robot) (:neq ?1 ?boxy)
					       (next-to ?1 ?boxx))
				    (:not (next-to ?1 ?boxx)))))
		   (next-to ?boxy ?boxx)
		   (next-to ?boxx ?boxy)
		   (next-to robot ?boxx)))
  
  (:operator PUSH-TO-DOOR
	     :parameters (?box ?door ?roomx ?roomy)
	     :precondition
	     (:and (connects ?door ?roomx ?roomy) ;[6]
		   (pushable ?box)	;[6]
		   (is-type ?door door)	;[6]
		   (in-room robot ?roomx) ;[5]
		   (in-room ?box ?roomx) ;[5]
		   (next-to robot ?box)) ;[1]
	     :effect     
	     (:and (:forall (?1 ?2) 
			    (:and (:when (at robot ?1 ?2)
				    (:not (at robot ?1 ?2)))
				  (:when (at ?box ?1 ?2)
				    (:not (at ?box ?1 ?2)))))
		   (:forall (?1)
			    (:and (:when (:and (:neq ?1 ?box) (next-to robot ?1))
				    (:not (next-to robot ?1)))
				  (:when (:and (:neq ?1 ?door) (next-to ?box ?1))
				    (:not (next-to ?box ?1)))
				  (:when (:and (:neq ?1 robot) (next-to ?1 ?box))
				    (:not (next-to ?1 ?box)))))
		   (next-to ?box ?door)
		   (next-to robot ?box)))
  
  
  (:operator PUSH-TO-LOC
	     :parameters (?box ?x ?y ?roomx)
	     :precondition
	     (:and (pushable ?box)	;[6]
		   (loc-in-room ?x ?y ?roomx) ;[6]
		   (in-room robot ?roomx) ;[5]
		   (in-room ?box ?roomx) ;[5]
		   (next-to robot ?box)) ;[1]
	     :effect
	     (:and (:forall (?1 ?2) 
			    (:and (:when (at robot ?1 ?2)
				    (:not (at robot ?1 ?2)))
				  (:when (:and (:neq ?x ?1) (:neq ?y ?2)
					       (at ?box ?1 ?2))
				    (:not (at ?box ?1 ?2)))))
		   (:forall (?1)
			    (:and (:when (:and (:neq ?1 ?box) (next-to robot ?1))
				    (:not (next-to robot ?1)))
				  (:when (next-to ?box ?1) 
				    (:not (next-to ?box ?1)))
				  (:when (:and (:neq ?1 robot) (next-to ?1 ?box))
				    (:not (next-to ?1 ?box)))))
		   (at ?box ?x ?y)
		   (next-to robot ?box)))
  
  (:operator GO-THRU-DOOR
	     :parameters (?door ?roomy ?roomx)
	     :precondition 
	     (:and (connects ?door ?roomx ?roomy) ;[6]
		   (is-type ?door door)	;[6]
		   (is-type ?roomx room) ;[6]
		   (in-room robot ?roomy) ;[5]
		   (statis ?door open)	;[2]
		   (next-to robot ?door)) ;[1]
	     :effect   
	     (:and (:forall (?1 ?2) 
			    (:when (at robot ?1 ?2)
			      (:not (at robot ?1 ?2))))
		   (:forall (?1)
			    (:when (next-to robot ?1)
			      (:not (next-to robot ?1))))
		   (:when (in-room robot ?roomy)
		     (:not (in-room robot ?roomy)))
		   (in-room robot ?roomx)))
  
  (:operator PUSH-THRU-DOOR
	     :parameters (?box ?door ?roomy ?roomx)
	     :precondition
	     (:and (connects ?door ?roomy ?roomx) ;[6]
		   (pushable ?box)	;[6]
		   (is-type ?door door)	;[6]
		   (is-type ?roomx room) ;[6]
		   (in-room robot ?roomy) ;[5]
		   (in-room ?box ?roomy) ;[5]
		   (statis ?door open)	;[2]
		   (next-to ?box ?door)	;[1]
		   (next-to robot ?box)) ;[1]
	     :effect
	     (:and (:forall (?1 ?2) 
		     (:and (:when (at robot ?1 ?2)
			     (:not (at robot ?1 ?2)))
			   (:when (at ?box ?1 ?2)
			     (:not (at ?box ?1 ?2)))))
		   (:forall (?1)
			    (:and (:when (:and (:neq ?1 ?box) (next-to robot ?1))
				    (:not (next-to robot ?1)))
				  (:when (next-to ?box ?1)
				    (:not (next-to ?box ?1)))
				  (:when (:and (:neq ?1 robot) (next-to ?1 ?box))
				    (:not (next-to ?1 ?box)))))
		   (:when (in-room robot ?roomy) (:not (in-room robot ?roomy)))
		   (:when (in-room ?box ?roomy) (:not (in-room ?box ?roomy)))
		   (in-room robot ?roomx)
		   (in-room ?box ?roomx)
		   (next-to robot ?box)))
       
  (:operator OPEN-DOOR
	     :parameters (?door)
	     :precondition 
	     (:and (is-type ?door door)	;[6]
		   (next-to robot ?door) ;[5]
		   (statis ?door closed)) ;[5]
	     :effect
	     (:and (:when (statis ?door closed) (:not (statis ?door closed)))
		   (statis ?door open)))
  
  (:operator CLOSE-DOOR
	     :parameters (?door)
	     :precondition 
	     (:and (is-type ?door door)	;[6]
		   (next-to robot ?door) ;[5]
		   (statis ?door open))	;[5]
	     :effect
	     (:and (:when (statis ?door open) (:not (statis ?door open)))
		   (statis ?door closed))))

(define (problem move-boxes)
    :domain 'strips-world
    :inits 
    ((connects dunimys runi rmys) (connects dmysram rmys rram)
     (connects dramhal rram rhal) (connects dmyspdp rmys rpdp)
     (connects dpdpclk rpdp rclk) (connects dmysclk rmys rclk)
     (connects dramclk rram rclk) (connects dclkril rclk rril)
     (connects dunimys rmys runi) (connects dmysram rram rmys)
     (connects dramhal rhal rram) (connects dmyspdp rpdp rmys)
     (connects dpdpclk rclk rpdp) (connects dmysclk rclk rmys)
     (connects dramclk rclk rram) (connects dclkril rril rclk)
     (statis dunimys open) (statis dmysram open) (statis dramhal open)
     (statis dmyspdp open) (statis dpdpclk open) (statis dmysclk open)
     (statis dramclk open) (statis dclkril closed)
     (is-type dunimys door) (is-type dmysram door) (is-type dramhal door)
     (is-type dmyspdp door) (is-type dpdpclk door) (is-type dmysclk door)
     (is-type dramclk door) (is-type dclkril door)
     (is-type runi room) (is-type rmys room) (is-type rram room)
     (is-type rhal room) (is-type rpdp room) (is-type rclk room)
     (is-type rril room)
     (is-type box1 object) (is-type box2 object) (is-type box3 object)
     (in-room robot rril) (in-room box1 rpdp) (in-room box2 rpdp)
     (in-room box3 rclk)
     (pushable box1) (pushable box2) (pushable box3))
    :goal (:and  (next-to box1 box2)(next-to box2 box3)))

(define (problem move-boxes-1)
    :domain 'strips-world
    :inits
    ((connects dunimys runi rmys) (connects dmysram rmys rram)
     (connects dramhal rram rhal) (connects dmyspdp rmys rpdp)
     (connects dpdpclk rpdp rclk) (connects dmysclk rmys rclk)
     (connects dramclk rram rclk) (connects dclkril rclk rril)
     (connects dunimys rmys runi) (connects dmysram rram rmys)
     (connects dramhal rhal rram) (connects dmyspdp rpdp rmys)
     (connects dpdpclk rclk rpdp) (connects dmysclk rclk rmys)
     (connects dramclk rclk rram) (connects dclkril rril rclk)
     (statis dunimys open) (statis dmysram open) (statis dramhal open)
     (statis dmyspdp open) (statis dpdpclk open) (statis dmysclk open)
     (statis dramclk open) (statis dclkril closed)
     (is-type dunimys door) (is-type dmysram door) (is-type dramhal door)
     (is-type dmyspdp door) (is-type dpdpclk door) (is-type dmysclk door)
     (is-type dramclk door) (is-type dclkril door)
     (is-type runi room) (is-type rmys room) (is-type rram room)
     (is-type rhal room) (is-type rpdp room) (is-type rclk room)
     (is-type rril room)
     (is-type box1 object) (is-type box2 object) (is-type box3 object)
     (in-room robot rril) (in-room box1 rpdp) (in-room box2 rpdp)
     (in-room box3 rclk)
     (pushable box1) (pushable box2) (pushable box3))
    :goal (:and  (next-to box1 box2)(in-room box2 rclk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced briefcase world
;;;   This domain is rather interesting - should be extended more -DSW 12/92

(define (domain office-world)

  (:operator move
	     :parameters (?b ?l ?m)
	     :precondition (:and (briefcase ?b) (:neq ?m ?l) (at ?b ?l))
	     :effect (:and (at ?b ?m)
			   (:not (at ?B ?l))
			   (:forall (?x)
				    (:when (:and (object ?x) (in ?x ?b))
				      (:and (at ?x ?m) (:not (at ?x ?l)))))))
       
  (:operator take-out
	     :parameters (?x ?b)
	     :precondition (in ?x ?b)
	     :effect (:not (in ?x ?b)))
  
  (:operator put-in
	     :parameters (?x ?b ?l)
	     :precondition (:and (:neq ?x ?B) (at ?x ?l) (at ?B ?l) (briefcase ?b))
	     :effect (in ?x ?b))

  (:operator print-check-for
	    :parameters (?p ?c)
	    :precondition (:and (person ?p) (new-object ?c))
	    :effect (:and (object ?c) (check ?c) (at ?c office) (written-for ?p ?c)))

  (:fact (new-object ?x)
	(when (variable:variable? ?x)
	  (list (setb ?x (gensym "obj-")))))
  )

(define (problem all-home)
    :domain 'office-world
    :inits ((object d) (object b) (briefcase b)
	    (at B home) (at d office))
    :goal (:forall (object ?o)
		   (at ?o home)))

(define (problem all3-home)
    :domain 'office-world
    :inits ((object d) (object b) (object p) (briefcase b)
	    (at B home) (at d office) (at p home))
    :goal (:forall (object ?o)
		   (at ?o home)))

(define (problem office1)
    :domain 'office-world
    :inits ((place home) (place office) (person sam) (person sue)
	    (object dict) (object b) (briefcase b)
	    (at B home) (at Dict home))
    :goal (:forall (person ?p)
		   (:exists (object ?c)
			    (:and (written-for ?p ?c)))))

(define (problem office2)
    :domain 'office-world
    :inits ((place home) (place office) (person sam) (person sue)
	    (object dict) (object b) (briefcase b)
	    (at B home) (at Dict home))
    :goal (:forall (object ?x) (at ?x office)))

(define (problem office3)
    :domain 'office-world
    :inits ((place home) (place office) (person sam) (person sue)
	    (object dict) (object b) (briefcase b)
	    (at B home) (at Dict home))
    :goal (:forall (person ?p)
		   (:exists (object ?c)
			    (:and (check ?c) (written-for ?p ?c)))))

(define (problem office4)
    :domain 'office-world
    :inits ((place home) (place office) (person sam) (person sue)
	    (object dict) (object b) (briefcase b)
	    (at B home) (at Dict home))
    :goal (:and (:forall (object ?x) (at ?x office))
		(:forall (person ?p)
			 (:exists (object ?c)
				  (:and (check ?c) (written-for ?p ?c))))))

(define (problem office5)
    :domain 'office-world
    :inits ((place home) (place office) (person sam) (person sue)
	    (object dict) (object b) (briefcase b)
	    (at B home) (at Dict home))
    :goal (:and (:forall (object ?x) (at ?x home))
		(:forall (person ?p)
			 (:exists (object ?c)
				  (:and (check ?c) (written-for ?p ?c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain init-flat-tire2)

  (:operator cuss
      :effect (:not (annoyed)))
  
  (:operator open
	     :parameters ((container ?x))
	     :precondition (:and (:not (locked ?x)) (:not (open ?x)))
	     :effect (open ?x))
  
  (:operator close
	     :parameters ((container ?x))
	     :precondition (open ?x)
	     :effect (:not (open ?x)))
  
  (:operator fetch
	     :parameters (?x (container ?y))
	     :precondition (:and (:neq ?x ?y) (in ?x ?y) (open ?y))
	     :effect (:and (have ?x) 
			   (:not (in ?x ?y))))
  
  (:operator put-away
	     :parameters (?x (container ?y))
	     :precondition (:and (:neq ?x ?y) (have ?x) (open ?y))
	     :effect (:and (in ?x ?y)
			   (:not (have ?x))))
  
  (:operator loosen
	     :parameters ((nut ?x) (hub ?y))
	     :precondition (:and (:neq ?x ?y) (have wrench) (tight ?x ?y) 
				 (:not (jacked ?y)))
	     :effect (:and (loose ?x ?y)
			   (:not (tight ?x ?y))))
  
  (:operator tighten
	     :parameters ((nut ?x) (hub ?y))
	     :precondition (:and (:neq ?x ?y) (have wrench) (loose ?x ?y) 
				 (:not (jacked ?y)))
	     :effect (:and (tight ?x ?y)
			   (:not (loose ?x ?y))))

  (:operator jack-up
	     :parameters ((hub ?y))
	     :precondition (:and (:not (jacked ?y)) (have jack))
	     :effect (:and (jacked ?y)
			   (forall (?x) (when (on ?x ?y) (:not (on ?x ground))))
			   (:not (have jack))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:operator jack-down
	     :parameters ((hub ?x))
	     :precondition (jacked ?x)
	     :effect (:and (:not (jacked ?x))
			   (forall (?y) (when (on ?y ?x) (on ?y ground)))
			   (have jack)))
  
  (:operator undo
	     :parameters ((nut ?x) (hub ?y))
	     :precondition (:and (:neq ?x ?y) 
				 (jacked ?y) (:not (unfastened ?y))
				 (have wrench) (loose ?x ?y))
	     :effect (:and (have ?x) (unfastened ?y)
			   (:not (on ?x ?y)) (:not (loose ?x ?y))))
  
  (:operator do-up
	     :parameters ((nut ?x) (hub ?y))
	     :precondition (:and (:neq ?x ?y)
				 (have wrench) (unfastened ?y)
				 (jacked ?y) (have ?x))
	     :effect
	     (:and (loose ?x ?y) (:not (unfastened ?y)) (:not (have ?x))))

  (:operator remove-wheel
	     :parameters ((wheel ?x) (hub ?y))
	     :precondition (:and (:neq ?x ?y) (jacked ?y)
				 (on ?x ?y) (unfastened ?y))
	     :effect (:and (have ?x) (free ?y) (:not (on ?x ?y))))
  
  (:operator put-on-wheel
	     :parameters ((wheel ?x) (hub ?y))
	     :precondition (:and (:neq ?x ?y) (have ?x) (free ?y) (unfastened ?y)
				 (jacked ?y))
	     :effect
	     (:and (on ?x ?y) (:not (have ?x)) (:not (free ?y))))
  
  (:operator inflate
	     :parameters ((wheel ?x))
	     :precondition (:and (have pump) (:not (inflated ?x)) (intact ?x))
	     :effect (inflated ?x)))


(define (problem fixit2)
    :domain 'init-flat-tire2
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (tool jack) (tool pump) (tool wrench)
	    (container boot) (intact wheel2)
	    (in jack boot) (in pump boot)
	    (in wheel2 boot) (in wrench boot)
	    (on wheel1 hub) (on wheel1 ground) (tight nuts hub))
    :goal (:and 
	   (:not (open boot)) 
	   (forall (tool ?x) (in ?x boot))
	   (in wheel1 boot)
	   (tight nuts hub) (inflated wheel2)(on wheel2 hub)))

