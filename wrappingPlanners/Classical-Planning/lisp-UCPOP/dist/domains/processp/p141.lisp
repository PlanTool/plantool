(load-start-state
 '(
        (is-a shaper1 SHAPER)
	(size-of-machine shaper1 20)

	(is-a cutting-tool1 ROUGHING-CUTTING-TOOL)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part2 PART)
	(material-of part2 COPPER)
	(size-of part2 LENGTH 5.000001)
	(size-of part2 WIDTH 3)
	(size-of part2 HEIGHT 2)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (surface-finish-side <part> side3 ROUGH-SHAPED)
)

)
