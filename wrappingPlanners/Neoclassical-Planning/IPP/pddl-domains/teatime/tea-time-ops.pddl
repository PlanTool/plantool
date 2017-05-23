;;; university of ulm
;;; multi-robot scenario: 2 robots cooperate to serve users in different 
;;; offices with the
;;; each robot is only allowed in some rooms and they meet in the hallway
;;; to exchange cups


(define (domain tea_time)
  (:requirements :strips)
  (:predicates (at ?r ?l)
	       (connected ?l1 ?l2)
               (allowed ?r ?l)
               (cupstack ?l)
               (free ?r)
               (emptycuploaded ?r)
               (teamachine ?l)
               (fullcuploaded ?r)
               (ordered ?l)
               (served ?l)
               (robby ?r)
               (location ?l))

  
(:action go
:parameters (?r ?l1 ?l2)
:precondition (and  (robby ?r) (location ?l1) (location ?l2) 
                    (at ?r ?l1) (connected ?l1 ?l2) (allowed ?r ?l2))
:effect (and (at ?r ?l2) (not (at ?r ?l1))))
   
(:action getcup
:parameters (?r ?l1 )
:precondition (and (robby ?r) (location ?l1)
                  (at ?r ?l1) (cupstack ?l1) (free ?r))
:effect (and (emptycuploaded ?r) (not (free ?r))))
   
(:action fillcup
:parameters (?r ?l1)
:precondition (and (robby ?r) (location ?l1)
                   (at ?r ?l1) (teamachine ?l1) (emptycuploaded ?r))    
:effect (and (fullcuploaded ?r) (not (emptycuploaded ?r))))
   
(:action deliver
:parameters (?r ?l1)
:precondition (and (robby ?r) (location ?l1)
                   (at ?r ?l1) (ordered ?l1) (fullcuploaded ?r))
:effect (and (served ?l1) (free ?r) (not (ordered ?l1)) 
        (not (fullcuploaded ?r))))
   
(:action exchangeemptycup
:parameters (?r1 ?r2 ?l1)
:precondition (and (robby ?r1) (robby ?r2) (location ?l1)
              (at ?r1 ?l1) (at ?r2 ?l1) (emptycuploaded ?r1) (free ?r2))
:effect (and (emptycuploaded ?r2) (free ?r1)
        (not (emptycuploaded ?r1)) (not (free ?r2))))   

(:action exchangefullcup
:parameters (?r1 ?r2 ?l1)
:precondition (and (robby ?r1) (robby ?r2) (location ?l1)
                  (at ?r1 ?l1) (at ?r2 ?l1) (fullcuploaded ?r1) (free ?r2))
:effect (and (fullcuploaded ?r2) (free ?r1)
        (not (fullcuploaded ?r1)) (not (free ?r2))))) 
