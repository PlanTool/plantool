(load-start-state
 '(
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a twist-drill2 TWIST-DRILL)
	(diameter-of-drill-bit twist-drill2 5/64)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part6 PART)
	(material-of part6 STEEL)
	(size-of part6 LENGTH 5)
	(size-of part6 WIDTH 3)
	(size-of part6 HEIGHT 2)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (has-hole <part> hole1 side1 1 5/64 1 1)

)
)
