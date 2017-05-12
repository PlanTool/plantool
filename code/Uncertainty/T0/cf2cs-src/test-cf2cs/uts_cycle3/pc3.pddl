(define (problem uts_cycle_3)
 (:domain uts_cycle_3)
 (:objects l1 l2 - label)
 (:init
  (oneof
   (and (at-node n0) (visited n0))
   (and (at-node n1) (visited n1))
   (and (at-node n2) (visited n2))
  )
  (oneof
   (and (edge-label n0 n1 l1) (edge-label n0 n2 l2))
   (and (edge-label n0 n1 l2) (edge-label n0 n2 l1))
  )
  (oneof
   (and (edge-label n1 n2 l1) (edge-label n1 n0 l2))
   (and (edge-label n1 n2 l2) (edge-label n1 n0 l1))
  )
  (oneof
   (and (edge-label n2 n0 l1) (edge-label n2 n1 l2))
   (and (edge-label n2 n0 l2) (edge-label n2 n1 l1))
  )
  )
 (:goal (and
        (visited n0)
        (visited n1)
        (visited n2)
       ))
)
