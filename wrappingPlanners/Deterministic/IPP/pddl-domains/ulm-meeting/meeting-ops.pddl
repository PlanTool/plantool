;; cr=corridor dr=door rm=room (cr2dr = corridor to door)
;; from university of ulm 
;; inspired from AAAI robot contest (robot calls people for a meeting)

(define (domain meeting)
(:requirements :strips)
(:predicates (at ?r ?l )
             (adjacent ?d ?l )
             (connected ?l1 ?l2 )
             (person ?x)
             (free ?l)
             (informed ?p)
             (meetingroom ?l)
             (robby ?r) 
             (corridor ?l1) 
             (door ?d) 
             (occupied ?r)
             (room ?l))
             
(:action cr2dr
:parameters (?r ?l1 ?d )
:precondition (and (robby ?r) (corridor ?l1) (door ?d) (at ?r ?l1) 
              (adjacent ?d ?l1))
:effect (and  (at ?r ?d) (not (at ?r ?l1))))


(:action dr2cr
:parameters (?r ?d ?l1)
:precondition (and (robby ?r) (corridor ?l1) (door ?d) (at ?r ?d) 
              (adjacent ?d ?l1))
:effect (and  (at ?r ?l1) (not (at ?r ?d))))

(:action rm2dr
:parameters (?r ?l1 ?d)
:precondition (and (robby ?r) (room ?l1) (door ?d) (at ?r ?l1) 
              (adjacent ?d ?l1))
:effect (and  (at ?r ?d) (not (at ?r ?l1))))

(:action dr2rm
:parameters (?r ?d ?l1)
:precondition (and (robby ?r) (room ?l1) (door ?d) (at ?r ?d) 
              (adjacent ?d ?l1))
:effect (and  (at ?r ?l1) (not (at ?r ?d))))

(:action cr2cr
:parameters (?r ?l1 ?l2)
:precondition (and (robby ?r) (corridor ?l1) (corridor ?l2)   (at ?r ?l1) 
              (connected ?l1 ?l2))
:effect (and  (at ?r ?l2) (not (at ?r ?l1))))

(:action approach_person
:parameters (?r ?p ?l1)
:precondition (and (robby ?r) (room ?l1) (person ?p) (at ?r ?l1) (at ?p ?l1))
:effect (and  (at ?r ?p) (not (at ?r ?l1))))

(:action speak
:parameters (?r ?p ?l1)
:precondition (and (robby ?r) (room ?l1) (person ?p) (at ?r ?p) (at ?p ?l1))
:effect (and  (informed ?p) (at ?r ?l1) (not (at ?r ?p))))

(:action occupy1
:parameters (?r ?l1)
:precondition (and (robby ?r) (room ?l1) (at ?r ?l1) (meetingroom ?l1))
:effect (occupied ?l1))

(:action occupy2
:parameters (?r ?l1)
:precondition (and (robby ?r) (room ?l1) (at ?r ?l1) (meetingroom ?l1)
              (free ?l1))
:effect (occupied ?l1)))



