(load-start-state
 '(
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a straight-fluted-drill1 STRAIGHT-FLUTED-DRILL)
	(diameter-of-drill-bit straight-fluted-drill1 1/32)

        (is-a 4-jaw-chuck1 4-JAW-CHUCK)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)
	
	(is-a part10 PART)
	(material-of part10 BRASS)
	(size-of part10 LENGTH 5)
	(size-of part10 DIAMETER 3)
))

(load-goal

	'(exists (<part>) (is-a <part> PART) (and (has-hole <part> hole1 side0 1 1/32 1 1)
	      (is-available-part <part>))
)
)
