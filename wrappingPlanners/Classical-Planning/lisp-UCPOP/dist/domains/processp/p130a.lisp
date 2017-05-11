(load-start-state
 '(
        (is-a lathe1 LATHE)

	(is-a rough-toolbit1 ROUGH-TOOLBIT)

        (is-a 4-jaw-chuck1 4-JAW-CHUCK)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

        (is-a part15 PART)
        (material-of part15 ALUMINUM)
	(size-of part15 LENGTH 5)
	(size-of part15 DIAMETER 3)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (surface-finish-side <part> SIDE0 ROUGH-TURN)
)

)
