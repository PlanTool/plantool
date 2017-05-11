(load-start-state
 '(
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a gun-drill1 GUN-DRILL)
	(diameter-of-drill-bit gun-drill1 1/32)

	(is-a gun-drill3 GUN-DRILL)
	(diameter-of-drill-bit gun-drill3 3/32)

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

	'(exists (<part>) (is-a <part> PART) (and (has-hole <part> hole1 side1 1 3/32 1 1)
	      (has-hole <part> hole2 side2 1 1/32 1 1))

)
)
