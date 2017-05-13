(define (problem tower9)
	(:domain hanoi)
	(:objects teeny tiny small medium large huge massive pega pegb pegc
		xteeny xxteeny)
	(:init 

               (on xxteeny xteeny)
	       (on xteeny teeny)
	       (on teeny tiny)
	       (on tiny small)
	       (on small medium)
	       (on medium large)
   	       (on large huge)
	       (on huge massive)
               (on massive pega)
	       (top xxteeny)
 	       (top pegb)
	       (top pegc)

	       (smaller xxteeny xteeny)
	       (smaller xxteeny teeny)
	       (smaller xxteeny tiny)
	       (smaller xxteeny small)
	       (smaller xxteeny medium)
	       (smaller xxteeny large)
	       (smaller xxteeny huge)
	       (smaller xxteeny massive)
	       (smaller xxteeny pega)  
	       (smaller xxteeny pegb) 
	       (smaller xxteeny pegc)



	       (smaller xteeny teeny)
	       (smaller xteeny tiny)
	       (smaller xteeny small)
	       (smaller xteeny medium)
	       (smaller xteeny large)
	       (smaller xteeny huge)
	       (smaller xteeny massive)
	       (smaller xteeny pega)  
	       (smaller xteeny pegb) 
	       (smaller xteeny pegc)




	       (smaller teeny tiny)
	       (smaller teeny small)
	       (smaller teeny medium)
	       (smaller teeny large)
	       (smaller teeny huge)
	       (smaller teeny massive)
	       (smaller teeny pega)  
	       (smaller teeny pegb) 
	       (smaller teeny pegc)
	       (smaller tiny small)
	       (smaller tiny medium)
	       (smaller tiny large)
	       (smaller tiny huge)
	       (smaller tiny massive)
	       (smaller tiny pega)  
	       (smaller tiny pegb) 
	       (smaller tiny pegc)
	       (smaller small medium)
	       (smaller small large)
	       (smaller small huge)
	       (smaller small massive)
	       (smaller small pega)  
	       (smaller small pegb) 
	       (smaller small pegc)
	       (smaller medium large) 
	       (smaller medium huge)
	       (smaller medium massive)
	       (smaller medium pega) 
	       (smaller medium pegb) 
	       (smaller medium pegc) 
	       (smaller large huge)
	       (smaller large massive)
	       (smaller large pega) 
	       (smaller large pegb) 
	       (smaller large pegc)
	       (smaller huge massive)
	       (smaller huge pega) 
	       (smaller huge pegb) 
	       (smaller huge pegc)
	       (smaller massive pega) 
	       (smaller massive pegb) 
	       (smaller massive pegc)) 
	(:goal (and 
		    (on xxteeny xteeny)
		    (on xteeny teeny)
		    (on teeny tiny)
		    (on tiny small)
		    (on small medium)
 		    (on medium large)
		    (on large huge)
		    (on huge massive)
   		    (on massive pegc))))