(load-start-state
 '(
        (is-a grinder1 GRINDER)

	(is-a grinding-wheel1 GRINDING-WHEEL)
	(hardness-of-wheel grinding-wheel1 HARD)
	(grit-of-wheel grinding-wheel1 COARSE-GRIT)

        (is-a magnetic-chuck1 MAGNETIC-CHUCK)

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

	'(exists (<part>) (is-a <part> PART) (surface-finish-side <part> SIDE6 ROUGH-GRIND)
)
)
