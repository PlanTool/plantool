(define (problem att-log2) (:domain logistics-strips)
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
	 (at package1 pgh-po);; dynamic predicates
	 (at airplane1 pgh-airport)
	 (at bos-truck bos-airport)
	 (at pgh-truck pgh-po))
  (:goal (and (at package1 bos-po))))
;;; this one takes nine steps

