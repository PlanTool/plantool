
(define (problem eight4) ;; (e f g d - b h a c)
    (:domain longs-puzzle)
      (:objects
    	sq0-0
    	sq0-1	
    	sq0-2
    	sq1-0
    	sq1-1	
    	sq1-2
    	sq2-0
    	sq2-1	
    	sq2-2
    	A
    	B
    	C
    	D
    	E
    	F
    	G
    	H)
    (:init 
    	   (square sq0-0)
	   (square sq0-1)
	   (square sq0-2)
	   (square sq1-0)
	   (square sq1-1)
	   (square sq1-2)
	   (square sq2-0)
	   (square sq2-1)
	   (square sq2-2)
	   (conn sq0-0 sq1-0)
	   (conn sq0-0 sq0-1)
	   (conn sq0-1 sq1-1)
	   (conn sq0-1 sq0-2)
	   (conn sq0-1 sq0-0)
	   (conn sq0-2 sq1-2)
	   (conn sq0-2 sq0-1)
	   (conn sq1-0 sq2-0)
	   (conn sq1-0 sq0-0)
	   (conn sq1-0 sq1-1)
	   (conn sq1-1 sq2-1)
	   (conn sq1-1 sq0-1)
	   (conn sq1-1 sq1-2)
	   (conn sq1-1 sq1-0)
	   (conn sq1-2 sq2-2)
	   (conn sq1-2 sq0-2)
	   (conn sq1-2 sq1-1)
	   (conn sq2-0 sq1-0)
	   (conn sq2-0 sq2-1)
	   (conn sq2-1 sq1-1)
	   (conn sq2-1 sq2-2)
	   (conn sq2-1 sq2-0)
	   (conn sq2-2 sq1-2)
	   (conn sq2-2 sq2-1)
	   (tile A)
	   (tile B)
	   (tile C)
	   (tile D)
	   (tile E)
	   (tile F)
	   (tile G)
	   (tile H)
	   (at e sq0-0)
	   (at f sq0-1)
	   (at g sq0-2)
	   (at d sq1-0)
	   (at-blank sq1-1)
	   (at b sq1-2)
	   (at h sq2-0)
	   (at a sq2-1)
	   (at c sq2-2))
    (:goal (and
	   (at A sq0-0)
	   (at B sq0-1)
	   (at C sq0-2)
	   (at D sq1-0)
	   (at E sq1-1)
	   (at F sq1-2)
	   (at G sq2-0)
	   (at H sq2-1)
	   (at-blank sq2-2))))
