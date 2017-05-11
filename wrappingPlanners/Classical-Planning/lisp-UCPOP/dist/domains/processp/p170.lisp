

(load-start-state
      '((is-a welder1 METAL-ARC-WELDER)
	(is-a electrode1 ELECTRODE)
	(is-a vise1 VISE)
        (is-a toe-clamp1 TOE-CLAMP)
	
	(is-a part36 PART)
	(material-of part36 STEEL)
	(size-of part36 LENGTH 10)
	(size-of part36 DIAMETER 5)
	(surface-finish-side part36 SIDE0 COLD-ROLLED)
	(surface-finish-side part36 SIDE3 FINISH-MILL)
	(surface-finish-side part36 SIDE6 ROUGH-MILL)

	(is-a part136 PART)
	(material-of part136 STEEL)
	(size-of part136 LENGTH 3)
	(size-of part136 DIAMETER 5)
	(surface-finish-side part136 SIDE0 COLD-ROLLED)
	(surface-finish-side part136 SIDE3 FINISH-GRIND)
	(surface-finish-side part136 SIDE6 ROUGH-GRIND)

       ))

(load-goal
	'(exists (<part>) (is-a <part> PART) (size-of <part> LENGTH 13)
)
)
