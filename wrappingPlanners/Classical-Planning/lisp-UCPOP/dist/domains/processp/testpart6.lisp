
; This problem has a complex goal, it needs a larger time bound.
;
(set-time-bound 800)


(load-start-state
 '(
	(is-a drill1 DRILL)
        (is-a milling-machine1 MILLING-MACHINE)

        (is-a vise1 VISE)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a twist-drill13 TWIST-DRILL)
	(diameter-of-drill-bit twist-drill13 1/4)

	(is-a tap14 TAP)
	(diameter-of-drill-bit tap14 1/4)

	(is-a counterbore2 COUNTERBORE)
	(size-of-drill-bit counterbore2 1/2)

	(is-a high-helix-drill1 HIGH-HELIX-DRILL)
	(diameter-of-drill-bit high-helix-drill1 1/32)

	(is-a tap1 TAP)
	(diameter-of-drill-bit tap1 1/32)

	(is-a milling-cutter1 PLAIN-MILL)
	(is-a milling-cutter2 END-MILL)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

        (is-a part5 PART)
        (material-of part5 ALUMINUM)
	(size-of part5 LENGTH 5)
	(size-of part5 WIDTH 3)
	(size-of part5 HEIGHT 3)

        (is-a part55 PART)
        (material-of part55 ALUMINUM)
	(size-of part55 LENGTH 5)
	(size-of part55 WIDTH 4)
	(size-of part55 HEIGHT 2)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (and
	      (material-of <part> ALUMINUM)
	      (size-of <part> LENGTH 4)
	      (size-of <part> WIDTH 3)
	      (size-of <part> HEIGHT 0.5)

	      (is-tapped <part> hole1 side1 .5 1/4 1/2 1.5)
	      (is-counterbored <part> hole1 side1 .5 1/4 1/2 1.5 1/2)

	      (is-tapped <part> hole2 side1 .5 1/4 1/2 2.5)
	      (is-counterbored <part> hole2 side1 .5 1/4 1/2 2.5 1/2)

	      (is-tapped <part> hole3 side1 .5 1/4 3.5 1.5)
	      (is-counterbored <part> hole3 side1 .5 1/4 3.5 1.5 1/2)

	      (is-tapped <part> hole4 side1 .5 1/4 3.5 2.5)
	      (is-counterbored <part> hole4 side1 .5 1/4 3.5 2.5 1/2)

	      (is-tapped <part> hole5 side1 .5 1/32 1.125 0.242)

	      (is-tapped <part> hole6 side1 .5 1/32 1.125 1.258)

	      (is-tapped <part> hole7 side1 .5 1/32 1.125 1.742)

	      (is-tapped <part> hole8 side1 .5 1/32 1.125 2.753);

	      (is-tapped <part> hole9 side1 .5 1/32 2.875 0.242)

	      (is-tapped <part> hole10 side1 .5 1/32 2.875 1.258)

	      (is-tapped <part> hole11 side1 .5 1/32 2.875 1.742)

	      (is-tapped <part> hole12 side1 .5 1/32 2.875 2.753)

	 ))

)
