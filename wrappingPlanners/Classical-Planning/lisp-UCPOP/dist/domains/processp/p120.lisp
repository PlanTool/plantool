(load-start-state
 '(
        (is-a milling-machine1 MILLING-MACHINE)

	(is-a milling-cutter1 PLAIN-MILL)
	(is-a milling-cutter2 END-MILL)

	(is-a v-block1 V-BLOCK)
        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part1 PART)
	(material-of part1 BRASS)
	(size-of part1 LENGTH 5)
	(size-of part1 WIDTH 3)
	(size-of part1 HEIGHT 2)
))


(load-goal
	'(exists (<part>) (is-a <part> PART) (and (size-of <part> LENGTH 4)
	      (size-of <part> WIDTH 2))
)
)
