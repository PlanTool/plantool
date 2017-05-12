(define (problem p1)
   (:domain tyreworld)
   (:objects jack pump wrench w1 w2 the-hub nuts trunk)

   (:init
	(in jack trunk) 
	(in pump trunk) 
	(in w2 trunk) 

        (in wrench trunk)
	(on w1 the-hub) 
	(on-ground the-hub) 
	(tight nuts the-hub)

	(unlocked trunk) 
	(closed trunk)
	(fastened the-hub)
   )

   (:goal (and 
		(in w1 trunk)
		(in jack trunk) 
		(in pump trunk))))
