(load-start-state
 '(
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a straight-fluted-drill1 STRAIGHT-FLUTED-DRILL)
	(diameter-of-drill-bit straight-fluted-drill1 1/32)

	(is-a vise1 VISE)
	(is-a toe-clamp1 TOE-CLAMP)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)
	
	(is-a part10 PART)
	(material-of part10 BRASS)
	(size-of part10 LENGTH 5)
	(size-of part10 DIAMETER 3)
))

(load-goal

	'(exists (<part>) (is-a <part> PART) (and 
	      (has-hole <part> hole1 side3 1 1/32 1 1)
	      (is-available-part <part>)
	      (is-available-machine drill1)
	      (is-available-tool straight-fluted-drill1)
	      (is-available-tool-holder drill1)
	      (is-available-table drill1 vise1)
	      (is-available-table drill1 toe-clamp)
	      (is-available-holding-device vise1)
	      (is-available-holding-device toe-clamp1)
	      (is-empty-holding-device vise1 drill1))

)
)
