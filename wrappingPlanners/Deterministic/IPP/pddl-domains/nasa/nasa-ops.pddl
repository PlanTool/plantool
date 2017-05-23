
(define (domain nasa)
   (:requirements :typing :adl :conditional-effects :domain-axioms)
   (:types pipe)
   (:predicates (flow-path ?p1 ?p2 - pipe)
                (flow-path1 ?p1 ?p2 - pipe)
    )


   (:axiom
        :vars (?a ?b - pipe)
        :context (flow-path ?a ?b)
        :implies (flow-path1 ?a ?b))

   (:axiom
        :vars (?a ?b - pipe)
        :context (flow-path1 ?a ?b)
        :implies (flow-path1 ?b ?a))

   (:axiom
        :vars (?a ?b - pipe)
        :context (exists (?z - pipe)
                   (and (flow-path1 ?a ?z) (flow-path1 ?z ?b)))
        :implies (flow-path1 ?a ?b))

   (:axiom
        :vars (?a - pipe)
        :implies (flow-path1 ?a ?a))

)

