(load-start-state
 '(
	(is-a drill1 DRILL)
        (is-a milling-machine1 MILLING-MACHINE)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a straight-fluted-drill1 STRAIGHT-FLUTED-DRILL)
	(diameter-of-drill-bit straight-fluted-drill1 1/32)

	(is-a milling-cutter1 PLAIN-MILL)

	(is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)
	
	(material-of part10 BRASS)
	(size-of part10 LENGTH 5)
	(size-of part10 DIAMETER 3)
))

(load-goal

	'(has-hole part10 hole1 side1 1 1/32 1 1)
)
