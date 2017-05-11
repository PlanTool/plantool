(load-start-state
 '(
        (is-a grinder1 GRINDER)

	(is-a grinding-wheel2 GRINDING-WHEEL)
	(hardness-of-wheel grinding-wheel2 SOFT)
	(grit-of-wheel grinding-wheel2 COARSE-GRIT)

        (is-a magnetic-chuck1 MAGNETIC-CHUCK)

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

	'(exists (<part>) (is-a <part> PART) (size-of <part> LENGTH 4)
)
)
