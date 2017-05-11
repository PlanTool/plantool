
(load-start-state
 '(

	(is-a electric-spray-gun1 ELECTRIC-ARC-SPRAY-GUN)

	(is-a wire2 SPRAYING-METAL-WIRE)
	(material-of wire2 ZIRCONIUM-OXIDE)

	(is-a wire10 SPRAYING-METAL-WIRE)
	(material-of wire10 TUNGSTEN)


        (is-a lathe1 LATHE)

	(is-a rough-toolbit1 ROUGH-TOOLBIT)

	(is-a vise1 VISE)
        (is-a centers1 CENTERS)

	(is-a drill1 DRILL)

	(is-a spot-drill1 SPOT-DRILL)

	(is-a center-drill1 CENTER-DRILL)
	(diameter-of-drill-bit center-drill1 1/16)

	(is-a countersink1 COUNTERSINK)
	(angle-of-drill-bit countersink1 60)

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

	'(exists (<part>) (is-a <part> PART) (surface-coating <part> HEAT-RESISTANT)
)

)
