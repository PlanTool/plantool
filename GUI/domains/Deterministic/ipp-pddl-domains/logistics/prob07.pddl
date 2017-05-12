(define (problem att-logistics0) (:domain att-logistics)
  (:objects package1 pgh-truck bos-truck airplane1
	    bos-po pgh-po bos-airport pgh-airport
	    pgh bos)
  (:init (OBJ package1)		; statis predicates
	 (TRUCK pgh-truck)
	 (TRUCK bos-truck)
	 (AIRPLANE airplane1)
	 (LOCATION bos-po)
	 (LOCATION pgh-po)
	 (LOCATION bos-airport)
	 (LOCATION pgh-airport)
	 (AIRPORT bos-airport)
	 (AIRPORT pgh-airport)
	 (CITY pgh)
	 (CITY bos)
	 (IN-CITY pgh-po pgh)
	 (IN-CITY pgh-airport pgh)
	 (IN-CITY bos-po bos)
	 (IN-CITY bos-airport bos)
	 (at package1 pgh-airport)    ;; dynamic predicates
	 (at airplane1 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po))
  (:goal (AND (at package1 bos-airport)))
  (:length (:serial 3) (:parallel 3)))
;;; a three step plan works
