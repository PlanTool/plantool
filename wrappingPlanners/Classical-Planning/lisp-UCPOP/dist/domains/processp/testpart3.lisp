(load-start-state
 '(

        (is-a milling-machine1 MILLING-MACHINE)
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a twist-drill13 TWIST-DRILL)
	(diameter-of-drill-bit twist-drill13 1/4)

	(is-a twist-drill5 TWIST-DRILL)
	(diameter-of-drill-bit twist-drill5 1/8)

	(is-a tap6 TAP)
	(diameter-of-drill-bit tap6 1/8)

	(is-a counterbore4 COUNTERBORE)
	(size-of-drill-bit counterbore4 3/8)

	(is-a milling-cutter1 PLAIN-MILL)
	(is-a milling-cutter2 END-MILL)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part51 PART)
	(material-of part51 BRASS)
	(size-of part51 LENGTH 5)
	(size-of part51 WIDTH 2)
	(size-of part51 HEIGHT 3)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (and 
	      (material-of <part> BRASS)
	      (size-of <part> LENGTH 3.25)
	      (size-of <part> WIDTH .5)
	      (size-of <part> HEIGHT 2.25)

	      (is-tapped <part> hole1 side1 1 1/8 .5 .25)
	      (is-tapped <part> hole2 side1 1 1/8 2.25 .25)
	      (is-tapped <part> hole3 side6  3 1/8 .25 1.12)

	      (has-hole <part> hole4 side4 1 1/4  2.25 .25)
	      (is-counterbored <part> hole4 side4 1 1/4  2.25 .25 3/8)
	  ))




)
