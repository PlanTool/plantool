(load-start-state
 '(
        (is-a milling-machine1 MILLING-MACHINE)
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a high-helix-drill1 HIGH-HELIX-DRILL)
	(diameter-of-drill-bit high-helix-drill1 1/32)

	(is-a twist-drill13 TWIST-DRILL)
	(diameter-of-drill-bit twist-drill13 1/4)

	(is-a counterbore1 COUNTERBORE)
	(size-of-drill-bit counterbore1 1/4)

	(is-a tap14 TAP)
	(diameter-of-drill-bit tap14 1/4)

	(is-a milling-cutter1 PLAIN-MILL)
	(is-a milling-cutter2 END-MILL)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part6 PART)
	(material-of part6 STEEL)
	(size-of part6 LENGTH 5)
	(size-of part6 WIDTH 3)
	(size-of part6 HEIGHT 3)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (and
	      (material-of <part> STEEL)
	      (size-of <part> LENGTH 4.25)
	      (size-of <part> WIDTH 1.25)
	      (size-of <part> HEIGHT 1)

	      (is-counterbored <part> hole1 side1 1/4  1/32 3/4 5/8 1/4)
	      (is-tapped <part> hole2 side1 1 1/4 57/16 5/8)
))
)
