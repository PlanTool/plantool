
(define (domain test)
(:requirements :adl :quantified-preconditions)
(:types pyramide block ball table)
(:predicates (clear ?x - (either block pyramide ball table)) 
             (on ?x - (either block ball pyramide) ?y - (either block table)))
          

(:action stack
:parameters (?x - (either block ball pyramide) ?y - (either block table))
:precondition (and (clear ?x) (clear ?y))
:effect  (and (not (clear ?y))
              (on ?x ?y))))

