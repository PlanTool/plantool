(define (domain sokoban)
(:requirements :strips)
(:predicates (navigable ?l)
             (direction ?d)
             (ball ?b)
             (at-robot ?l)
             (at ?o ?l)
             (adjacent ?l1 ?l2 ?d) 
             (empty ?l))


(:action move
:parameters (?from ?to ?dir)
:precondition (and (navigable ?from) (navigable ?to) (direction ?dir)
                   (at-robot ?from) (adjacent ?from ?to ?dir) (empty ?to))
:effect (and (empty ?from) (at-robot ?to) (not (empty ?to))
             (not (at-robot ?from))))
             

(:action push
:parameters  (?rloc ?bloc ?floc  ?dir ?b)
:precondition (and (navigable ?rloc) (navigable ?bloc) (navigable ?floc)
                   (direction ?dir) (ball ?b) (at-robot ?rloc)
                   (at ?b ?bloc) (adjacent ?rloc ?bloc ?dir)
                   (adjacent ?bloc ?floc ?dir) (empty ?floc))
:effect (and (at-robot ?bloc) (at ?b ?floc) (empty ?rloc)
             (not (at-robot ?rloc)) (not (at ?b ?bloc)) (not (empty ?floc))))

)
