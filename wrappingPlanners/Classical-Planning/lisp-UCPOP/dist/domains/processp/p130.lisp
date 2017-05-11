(load-start-state
 '(
        (is-a lathe1 LATHE)

	(is-a rough-toolbit1 ROUGH-TOOLBIT)

        (is-a 4-jaw-chuck1 4-JAW-CHUCK)

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
	'(exists (<part>) (is-a <part> PART) (size-of <part> DIAMETER 1.9)
)

)
