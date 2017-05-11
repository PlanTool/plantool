;;; From: Alfonso Gerevini <gerevini@minerva.ing.unibs.it>
;;; To: weld@cs.washington.edu
;;; Subject: Trains and T-Trains
;;; Cc: schubert@cs.rochester.edu, gerevini@bsing.ing.unibs.it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A TRAINS DOMAIN (loosely based on TRAINS-93 world)     
;;
;; From A. Gerevini & L.K. Schubert's AIPS-96 paper.

(defun Trains ()                   
  (reset-domain)
  
  (define (operator mv-engine)
      :parameters (?eng ?cityone ?citytwo ?track ?car)
		  ; ?car is a "hidden" parameter
      :precondition (:and (engine ?eng) (at ?eng ?cityone) 
			  (connects ?track ?cityone ?citytwo))
      :effect (:and (at ?eng ?citytwo) (:not (at ?eng ?cityone))
                    (when (coupled ?eng ?car) 
		      (:and (at ?car ?citytwo)
				(:not (at ?car ?cityone)) ))))
  
  (define (operator ld-oranges)
      :parameters (?ors ?car ?city)
      :precondition (:and (oranges ?ors) (boxcar ?car) 
			  (empty ?car)
                          (at ?ors ?city) (at ?car ?city) )
      :effect (:and (:not (empty ?car)) (in ?ors ?car) 
		    (:not (at ?ors ?city))) )
  
  (define (operator ld-bananas)
      :parameters (?bas ?car ?city)
      :precondition (:and (bananas ?bas) (boxcar ?car) 
			  (empty ?car)
                          (at ?bas ?city) (at ?car ?city) )
      :effect (:and (:not (empty ?car)) (in ?bas ?car) 
		    (:not (at ?bas ?city))) )
  
  (define (operator ld-oj)
      :parameters (?oj ?car ?city)
      :precondition (:and (oj ?oj) 
			  (tanker-car ?car) 
			  (empty ?car)
                          (at ?oj ?city) (at ?car ?city) )
      :effect (:and (:not (empty ?car)) (in ?oj ?car) 
		    (:not (at ?oj ?city))) )
  
  (define (operator make-oj)
      :parameters (?o ?fac ?city)
      :precondition (:and (oranges ?o) (oj-fac ?fac) 
                          (at ?o ?city) (at ?fac ?city) )
      :effect (:and (oj ?o) (:not (oranges ?o))) )
  
  (define (operator unload)
      :parameters (?comm ?car ?city)
      :precondition (:and (in ?comm ?car) (at ?car ?city))
      :effect (:and (:not (in ?comm ?car)) (empty ?car) 
		    (at ?comm ?city)) )
  
  (define (operator couple)
      :parameters (?eng ?car ?city)
      :precondition (:and (engine ?eng) (car ?car) (loose ?car) 
                          (at ?eng ?city) (at ?car ?city) )
      :effect (:and (coupled ?eng ?car) (:not (loose ?car))) )
  
  (define (operator uncouple)
      :parameters (?eng ?car)
      :precondition (coupled ?eng ?car)
      :effect (:and (loose ?car) (:not (coupled ?eng ?car))) ) 
  )  ; end defun


;; Initial states of Trains1/2/3 and of T-Trains1/2/3:
;; the initial state of Trains3 (T-Trains3) is the same as that of
;; Trains1 (T-Trains1) except that oj-fac1 and e3 are at Corning instead
;; of Elmira. The initial state of Trains2 is the same as that of Trains3
;; except that the connections from Corning to Bath and from Dansville to
;; Corning are disabled (say, for maintenance).
;; 
(define (problem Trains1)
  :domain #'Trains
  :inits
  ((city avon) (city bath) (city corning) (city dansville) (city elmira)
   (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
   (connects tr1 avon bath) (connects tr1 bath avon) 
   (connects tr2 bath corning) (connects tr2 corning bath)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   (connects tr4 dansville corning) (connects tr4 corning dansville)
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (engine e1) (engine e2) (engine e3)    
   (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (bananas bas1) (oj-fac oj-fac1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 corning)
   (at e2 elmira) (at e3 elmira) (at bc4 elmira) (at oj-fac1 elmira) )
  :goal (:exists (oranges ?x) (at ?x bath)) )

(define (problem Trains2)
  :domain #'Trains
  :inits
   ((city avon) (city bath) (city corning) (city dansville) (city elmira)
    (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
    (connects tr1 avon bath) (connects tr1 bath avon)
    (connects tr2 bath corning) (connects tr4 corning dansville)
    (connects tr3 avon dansville) (connects tr3 dansville avon)
;;  (connects tr2 corning bath) (connects tr4 dansville corning)     
    (connects tr5 corning elmira) (connects tr5 elmira corning)
    (engine e1) (engine e2) (engine e3)
    (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
    (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
    (oranges ors1) (bananas bas1) (oj-fac oj-fac1)
    (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
    (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
    (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
    (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
    (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning) )   
   :goal (:exists (oj ?x) (at ?x dansville)) )

(define (problem Trains3)
  :domain #'Trains
  :inits
   ((city avon) (city bath) (city corning) (city dansville) (city elmira)
    (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
    (connects tr1 avon bath) (connects tr1 bath avon)
    (connects tr2 bath corning) (connects tr2 corning bath)
    (connects tr3 avon dansville) (connects tr3 dansville avon)
    (connects tr4 dansville corning) (connects tr4 corning dansville)
    (connects tr5 corning elmira) (connects tr5 elmira corning)
    (engine e1) (engine e2) (engine e3)
    (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
    (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
    (oranges ors1) (bananas bas1) (oj-fac oj-fac1)
    (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
    (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
    (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
    (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
    (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning) )   
   :goal (:exists (oj ?x) (at ?x dansville)) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; T-Trains ("Typed" version of the Trains domain)          
;;
(defun T-Trains ()                
  (reset-domain)
  
  (define (operator mv-engine)
      :parameters (?eng ?cityone ?citytwo ?track ?car)
		  ; ?car is a "hidden" parameter
      :precondition (:and (engine ?eng) (at ?eng ?cityone) 
			  (city ?cityone) (city ?citytwo) (track ?track)
			  (connects ?track ?cityone ?citytwo))
      :effect (:and (at ?eng ?citytwo) (:not (at ?eng ?cityone))
		    (when (:and (car ?car) (coupled ?eng ?car))
			(:and (at ?car ?citytwo)
			      (:not (at ?car ?cityone)) ))))
  
  (define (operator ld-oranges)
      :parameters (?ors ?car ?city)
      :precondition (:and (oranges ?ors) (boxcar ?car) 
			  (empty ?car) (city ?city)
			  (at ?ors ?city) (at ?car ?city) )
      :effect (:and (:not (empty ?car)) (in ?ors ?car) 
		    (:not (at ?ors ?city))) )
  
  (define (operator ld-bananas)
      :parameters (?bas ?car ?city)
      :precondition (:and (bananas ?bas) (boxcar ?car) 
			  (empty ?car) (city ?city)
			  (at ?bas ?city) (at ?car ?city) )
      :effect (:and (:not (empty ?car)) (in ?bas ?car) 
		    (:not (at ?bas ?city))) )
  
  (define (operator ld-oj)
      :parameters (?oj ?car ?city)
      :precondition (:and (oj ?oj) 
			  (tanker-car ?car) 
			  (empty ?car) (city ?city)
			  (at ?oj ?city) (at ?car ?city) )
      :effect (:and (:not (empty ?car)) (in ?oj ?car) 
		    (:not (at ?oj ?city))) )
  
  (define (operator make-oj)
      :parameters (?o ?fac ?city)
      :precondition (:and (oranges ?o) (oj-fac ?fac) (city ?city)
			  (at ?o ?city) (at ?fac ?city) )
      :effect (:and (oj ?o) (:not (oranges ?o))) )
  
  (define (operator unload)
      :parameters (?comm ?car ?city)
      :precondition  (:and (comm ?comm) (car ?car) (city ?city)
			   (in ?comm ?car) (at ?car ?city)) 
      :effect (:and (:not (in ?comm ?car)) (empty ?car) 
		    (at ?comm ?city)) )
  
  (define (operator couple)
      :parameters (?eng ?car ?city)
      :precondition (:and (engine ?eng) (car ?car) (loose ?car) 
			  (city ?city) (at ?eng ?city) (at ?car ?city) )
      :effect (:and (coupled ?eng ?car) (:not (loose ?car))) )
  
  (define (operator uncouple)
      :parameters (?eng ?car)
      :precondition (:and (engine ?eng) (car ?car)
			  (coupled ?eng ?car))
      :effect (:and (loose ?car) (:not (coupled ?eng ?car))) ) 
  )  ; end defun


(define (problem T-Trains1)
    :domain #'T-Trains
    :inits
    ((city avon) (city bath) (city corning) (city dansville) (city elmira)
     (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
     (connects tr1 avon bath) (connects tr1 bath avon) 
     (connects tr2 bath corning) (connects tr2 corning bath)
     (connects tr3 avon dansville) (connects tr3 dansville avon)
     (connects tr4 dansville corning) (connects tr4 corning dansville)
     (connects tr5 corning elmira) (connects tr5 elmira corning)
     (engine e1) (engine e2) (engine e3)
     (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
     (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
     (oranges ors1) (bananas bas1) (oj-fac oj-fac1) (comm ors1) (comm bas1)
     (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
     (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
     (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
     (at bc3 dansville) (at tc1 corning) (at ors1 corning)
     (at e2 elmira) (at e3 elmira) (at bc4 elmira) (at oj-fac1 elmira) )
    :goal (:exists (oranges ?x) (at ?x bath)) )


(define (problem T-Trains2)
  :domain #'T-Trains
  :inits
   ((city avon) (city bath) (city corning) (city dansville) (city elmira)
    (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
    (connects tr1 avon bath) (connects tr1 bath avon)
    (connects tr2 bath corning) (connects tr4 corning dansville)
    (connects tr3 avon dansville) (connects tr3 dansville avon)
;;  (connects tr2 corning bath) (connects tr4 dansville corning)     
    (connects tr5 corning elmira) (connects tr5 elmira corning)
    (engine e1) (engine e2) (engine e3)
    (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
    (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
    (oranges ors1) (bananas bas1) (oj-fac oj-fac1) (comm ors1) (comm bas1)
    (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
    (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
    (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
    (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
    (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning) )   
   :goal (:exists (oj ?x) (at ?x dansville)) )


(define (problem T-Trains3)
  :domain #'T-Trains
  :inits
   ((city avon) (city bath) (city corning) (city dansville) (city elmira)
    (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
    (connects tr1 avon bath) (connects tr1 bath avon)
    (connects tr2 bath corning) (connects tr2 corning bath)
    (connects tr3 avon dansville) (connects tr3 dansville avon)
    (connects tr4 dansville corning) (connects tr4 corning dansville)
    (connects tr5 corning elmira) (connects tr5 elmira corning)
    (engine e1) (engine e2) (engine e3)
    (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
    (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
    (oranges ors1) (bananas bas1) (oj-fac oj-fac1) (comm ors1) (comm bas1) 
    (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
    (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
    (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
    (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
    (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning) )   
   :goal (:exists (oj ?x) (at ?x dansville)) )




