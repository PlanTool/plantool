(load-start-state
 '(
        (is-a lathe1 LATHE)

	(is-a rough-toolbit1 ROUGH-TOOLBIT)

        (is-a collet-chuck1 COLLET-CHUCK)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

        (is-a part15 PART)
        (material-of part15 ALUMINUM)
	(size-of part15 LENGTH 5)
	(size-of part15 DIAMETER 3)

))


(load-goal
	'(exists (<part>) (is-a <part> PART) (and (size-of <part> DIAMETER 0.1)
	      (is-available-part <part>))
)
)
