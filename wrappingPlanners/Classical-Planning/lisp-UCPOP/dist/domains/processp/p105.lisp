(load-start-state
 '(
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a oil-hole-drill3 OIL-HOLE-DRILL)
	(diameter-of-drill-bit oil-hole-drill3 3/32)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

        (is-a part5 PART)
        (material-of part5 ALUMINUM)
	(size-of part5 LENGTH 5)
	(size-of part5 WIDTH 3)
	(size-of part5 HEIGHT 2)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (has-hole <part> hole1 side3 1 3/32 1 1)
)
)
