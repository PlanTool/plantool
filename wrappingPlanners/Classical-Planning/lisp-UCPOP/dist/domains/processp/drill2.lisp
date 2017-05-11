(load-start-state
 '(
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a straight-fluted-drill3 STRAIGHT-FLUTED-DRILL)
	(diameter-of-drill-bit straight-fluted-drill3 3/32)

	(is-a high-helix-drill3 HIGH-HELIX-DRILL)
	(diameter-of-drill-bit high-helix-drill3 3/32)

	(is-a reamer3 REAMER)
	(diameter-of-drill-bit reamer3 3/32)

	(is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part2 PART)
	(material-of part2 COPPER)
	(size-of part2 LENGTH 5)
	(size-of part2 WIDTH 3)
	(size-of part2 HEIGHT 3)

	(is-a part6 PART)
	(material-of part6 STEEL)
	(size-of part6 LENGTH 5)
	(size-of part6 WIDTH 3)
	(size-of part6 HEIGHT 3)
))

(load-goal

	'(exists (<part>) (is-a <part> PART) (and 
	      (is-reamed <part> hole1 side1 1 3/32 1 1)
	      (has-hole <part> hole2 side5 1.5 3/32 2 2)
	  ))


)
