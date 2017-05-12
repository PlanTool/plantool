(OPERATOR 
  PICK-UP
  (params <ob1>)
  (preconds 
   ((<ob1> OBJECT))
   (and (clear <ob1>)
	(on-table <ob1>)
	(arm-empty)))
  (effects 
      () ; no vars need genenerated in effects list
      ((del (on-table <ob1>))
       (del (clear <ob1>))
       (del (arm-empty))
       (add (holding <ob1>)))))

(OPERATOR
  PUT-DOWN
  (params <ob>)
  (preconds 
   ((<ob> OBJECT))
   (holding <ob>))
  (effects 
        ()
	((del (holding <ob>))
	 (add (clear <ob>))
	 (add (arm-empty))
	 (add (on-table <ob>)))))

;(defun diff (x y) (not (eq x y)))

(OPERATOR
 STACK
 (params <ob> <underob>)
 (preconds 
   ((<ob> OBJECT)
    (<underob> OBJECT))
    (and (clear <underob>)
	 (holding <ob>)))
 (effects 
      ()
      ((del (holding <ob>))
       (del (clear <underob>))
       (add (arm-empty))
       (add (clear <ob> ))
       (add (on <ob> <underob>)))))

(OPERATOR
 UNSTACK
 (params <ob> <underob>)
 (preconds
  ((<ob> OBJECT)
   (<underob> OBJECT))
  (and (on <ob> <underob>)
       (clear <ob>)
       (arm-empty)))
 (effects 
        ()
	((del (on <ob> <underob>))
	 (del (clear <ob>))
	 (del (arm-empty))
	 (add (holding <ob>))
	 (add (clear <underob>)))))

