(load-start-state
 '(
        (is-a planer1 PLANER)

	(is-a cutting-tool2 FINISHING-CUTTING-TOOL)

        (is-a toe-clamp1 TOE-CLAMP)

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

	'(exists (<part>) (is-a <part> PART) (size-of <part> LENGTH 5)
)

)
