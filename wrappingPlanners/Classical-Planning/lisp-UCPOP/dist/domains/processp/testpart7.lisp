(load-start-state
 
      '((is-a milling-machine1 MILLING-MACHINE)
	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a twist-drill6 TWIST-DRILL)
	(diameter-of-drill-bit twist-drill6 9/64)

	(is-a milling-cutter1 PLAIN-MILL)
	(is-a milling-cutter2 END-MILL)

        (is-a vise1 VISE)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

        (is-a part5 PART)
        (material-of part5 ALUMINUM)
	(size-of part5 LENGTH 5)
	(size-of part5 WIDTH 3)
	(size-of part5 HEIGHT 3)
))

(load-goal
       '(exists (<part>) (is-a <part> PART) (and
	      (material-of <part> ALUMINUM)
	      (size-of <part> LENGTH 4)
	      (size-of <part> WIDTH 2)
	      (size-of <part> HEIGHT 0.3)

	      (has-hole <part> hole1 side1 0.3 9/64 1.375 0.25)
	      (has-hole <part> hole2 side1 0.3 9/64 2.42 0.25)
))
)
