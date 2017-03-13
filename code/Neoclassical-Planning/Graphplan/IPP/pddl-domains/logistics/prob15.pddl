(define (problem logistics3)
  (:domain logistics-adl)
  (:objects package1 package2 package3 package4 
	    package5 package6 package7 - obj
	    pgh-truck bos-truck la-truck ny-truck - truck
	    airplane1 airplane2 - airplane
	    bos-po la-po pgh-po ny-po - location
	    bos-airport pgh-airport la-airport ny-airport - airport
	    pgh bos la ny - city)
  (:init (at package1 pgh-po)
	 (at package2 pgh-po)
	 (at package3 pgh-po)
	 (at package4 ny-po)
	 (at package5 bos-po)
	 (at package6 bos-po)
	 (at package7 ny-po)
	 (at airplane1 pgh-airport)
	 (at airplane2 pgh-airport)
	 (at bos-truck bos-po)
	 (at pgh-truck pgh-po)
	 (at la-truck la-po)
	 (at ny-truck ny-po)
	 (loc-at pgh-po pgh)
	 (loc-at pgh-airport pgh)
	 (loc-at bos-po bos)
	 (loc-at bos-airport bos)
	 (loc-at la-po la)
	 (loc-at la-airport la)
	 (loc-at ny-po ny)
	 (loc-at ny-airport ny))
  (:goal (and
	  (at package1 bos-po)
	  (at package2 ny-po)
	  (at package3 la-po)
	  (at package4 la-airport)
	  (at package5 pgh-po)
	  (at package6 ny-airport)
	  (at package7 pgh-po)))
    )