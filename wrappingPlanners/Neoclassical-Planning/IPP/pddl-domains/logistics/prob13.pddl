(define (problem logistics1)
  (:domain logistics-adl)
  (:objects package1 package2 package3 package4 package5 package6 - obj
	    pgh-truck bos-truck la-truck - truck
	    airplane1 airplane2 - airplane
	    bos-po la-po pgh-po - location
	    bos-airport pgh-airport la-airport - airport
	    pgh bos la - city)

  (:init (at package1 pgh-po)
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 pgh-po)
	 (at package5 pgh-po)
	 (at package6 la-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po)
	 (loc-at pgh-po pgh)
	 (loc-at pgh-airport pgh)
	 (loc-at bos-po bos)
	 (loc-at bos-airport bos)
	 (loc-at la-po la)
	 (loc-at la-airport la))
  (:goal (and (at package1 bos-po)
	      (at package2 la-po)
	      (at package3 la-po)
	      (at package4 la-airport)
	      (at package5 bos-po)
	      (at package6 pgh-po)
	      )))