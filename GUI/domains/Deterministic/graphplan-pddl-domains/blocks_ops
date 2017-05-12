#old-style.  The hash mark indicates this is old-style, meaning that
any precondition not explicitly mentioned in the effects is deleted.
See also "LOAD-TRUCK" in the logistics domain.  This is convenient for
domains having the property that you never delete a non-precondition
--- which seems to include most natural domains.

By the way, everything up to the first parenthesis is a comment...

(operator
  PICK-UP
  (params (<ob1> OBJECT))
  (preconds 
	(clear <ob1>) (on-table <ob1>) (arm-empty))
  (effects 
	(holding <ob1>)))

(operator
  PUT-DOWN
  (params (<ob> OBJECT))
  (preconds
	(holding <ob>))
  (effects 
	 (clear <ob>) (arm-empty) (on-table <ob>)))

(operator
 STACK
 (params (<ob> OBJECT) (<underob> OBJECT))
 (preconds 
	(clear <underob>) (holding <ob>))
 (effects 
       (arm-empty) (clear <ob>) (on <ob> <underob>)))

(operator
 UNSTACK
 (params (<ob> OBJECT) (<underob> OBJECT))
 (preconds
	(on <ob> <underob>) (clear <ob>) (arm-empty))
 (effects 
        (holding <ob>) (clear <underob>)))

