(load-start-state
 '(
        (is-a lathe1 LATHE)

	(is-a v-thread1 V-THREAD)
	(is-a knurl1 KNURL)

        (is-a 4-jaw-chuck1 4-JAW-CHUCK)

	(is-a brush1 BRUSH)

	(is-a soluble-oil SOLUBLE-OIL)
	(is-a mineral-oil MINERAL-OIL)

	(is-a part10 PART)
	(material-of part10 BRASS)
	(size-of part10 LENGTH 5)
	(size-of part10 DIAMETER 3.00001)

))

(load-goal

	'(exists (<part>) (is-a <part> PART) (surface-finish-side <part> SIDE0 TAPPED)
)

)
