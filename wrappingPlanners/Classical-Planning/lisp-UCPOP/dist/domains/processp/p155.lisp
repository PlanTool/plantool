(load-start-state
 '(
        (is-a band-saw1 BAND-SAW)

	(is-a saw-attachment2 BAND-FILE)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part2 PART)
	(material-of part2 COPPER)
	(size-of part2 LENGTH 5)
	(size-of part2 WIDTH 3)
	(size-of part2 HEIGHT 2)
	
	(surface-finish-side part2 side1 ROUGH-MILL)
))

(load-goal

	'(exists (<part>) (is-a <part> PART) (size-of <part> HEIGHT 1)
)
)
