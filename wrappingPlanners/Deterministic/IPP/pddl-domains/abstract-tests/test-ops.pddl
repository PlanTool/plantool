
(define (domain test)
(:requirements :adl :existential-preconditions :universal-preconditions)
(:types type1 block)
(:predicates (clear ?x)
             (on ?x ?y)
             (smaller ?x ?y)
             (p ?z) (q ?z)(a ?x) (b ?y)(ww ?z ?y))

;;p(?z) & q(?z) => (a(x) & (b(y)) or !q(z))
;;!p(z) or !q(z) or (a(x) & b(y)) or !q(z)
;;!p(z) || !q(z) || a(x) & b(y) als 3 vorbed.

(:action move1
:parameters (?z ?x ?y - type1)
:precondition (imply (and (p ?z) (q ?z)) (or (and (a ?x) (b ?y)) (not (q ?z))))
:effect  (and (clear ?z) 
              (on ?z ?x) 
              (not (on ?z ?x))  
              (not (clear ?y)))
 ) 

(:action move2
:parameters (?z ?w ?y - type1)
:precondition (not (exists (?z ?y - block) (forall (?w ?z - block) 
               (imply (and (p ?z) (q ?z)) (or (ww ?z ?y))))))
:effect  (and (clear ?w) 
              (on ?w ?z) 
              (not (on ?z ?w))  
              (not (clear ?y)))
 )


(:axiom 
:vars (?y ?x ?z - type1)

:context (and (p ?z) (q ?z) (or (and (a ?x) (b ?y)) (b ?y)))

:implies  (not (b ?y)))
)
