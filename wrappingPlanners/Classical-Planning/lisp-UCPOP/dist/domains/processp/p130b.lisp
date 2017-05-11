(load-start-state
 '(
        (is-a lathe1 LATHE)

	(is-a rough-toolbit1 ROUGH-TOOLBIT)

	(is-a vise1 VISE)
        (is-a toe-clamp1 TOE-CLAMP)
        (is-a centers1 CENTERS)

	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a center-drill1 CENTER-DRILL)
	(diameter-of-drill-bit center-drill1 1/16)

	(is-a countersink1 COUNTERSINK)
	(angle-of-drill-bit countersink1 60)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

        (is-a part15 PART)
        (material-of part15 ALUMINUM)
	(size-of part15 LENGTH 5)
	(size-of part15 DIAMETER 3)

))

(load-goal
	'(exists (<part>) (is-a <part> PART) (size-of <part> DIAMETER 2)
)

)
