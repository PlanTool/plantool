(define (problem hard-eight3)  ;;    eight1 in aaai paper
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
	   (at H sq0-0)
	   (at F sq0-1)
	   (at G sq0-2)
	   (at B sq1-0)
	   (at E sq1-1)
	   (at D sq1-2)
	   (at C sq2-0)
	   (at-blank sq2-1)
	   (at A sq2-2))  ;; from HFGBEDC-A
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


