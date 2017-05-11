(define (problem ZTRAVEL-2-5)
(:domain zeno-travel)
(:objects
	plane1
	plane2
	person1
	person2
	person3
	person4
	person5
	city0
	city1
	city2
	fl0
	fl1
	fl2
	fl3
	fl4
	fl5
	fl6
	)
(:init
	(at plane1 city2)
	(aircraft plane1)
	(fuel-level plane1 fl5)
	(at plane2 city2)
	(aircraft plane2)
	(fuel-level plane2 fl0)
	(at person1 city0)
	(person person1)
	(at person2 city1)
	(person person2)
	(at person3 city0)
	(person person3)
	(at person4 city0)
	(person person4)
	(at person5 city2)
	(person person5)
	(city city0)
	(city city1)
	(city city2)
	(lnext fl0 fl1)
	(lnext fl1 fl2)
	(lnext fl2 fl3)
	(lnext fl3 fl4)
	(lnext fl4 fl5)
	(lnext fl5 fl6)
	(flevel fl0)
	(flevel fl1)
	(flevel fl2)
	(flevel fl3)
	(flevel fl4)
	(flevel fl5)
	(flevel fl6)
)
(:goal (and
	(at plane1 city0)
	(at person2 city2)
	(at person3 city0)
	(at person4 city1)
	(at person5 city2)
	))

)
