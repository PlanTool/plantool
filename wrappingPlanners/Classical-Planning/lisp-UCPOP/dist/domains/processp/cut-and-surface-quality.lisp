(load-start-state
 '(
	(size-of part1 HEIGHT 0.5)
	(size-of part1 WIDTH 1.25)
	(size-of part1 LENGTH 2.75)

	(material-of part1 COPPER)

	(surface-finish-side part1 side1 SAWCUT)
	(surface-finish-side part1 side2 SAWCUT)
	(surface-finish-side part1 side3 SAWCUT)
	(surface-finish-side part1 side4 SAWCUT)
	(surface-finish-side part1 side5 SAWCUT)
	(surface-finish-side part1 side6 SAWCUT)

	(is-circular-saw circular-saw1)
	(is-cold-saw cold-saw1)

	(has-device circular-saw1 vise1)
	(is-vise vise1)

	;add machine to polish <--- (lathe)

))

(load-goal

	'(and (size-of part1 HEIGHT 0.4)
	      (size-of part1 WIDTH 1)
	      (size-of part1 LENGTH 2)
	      (surface-finish-quality-side part1 side1 POLISHED))
)


