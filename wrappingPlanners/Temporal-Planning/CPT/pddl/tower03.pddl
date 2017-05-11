(define (problem bw-tower3)
(:domain prodigy-bw)
(:objects
 b1
 b2
 b3
)
(:init
(arm-empty)
(on-table b1)
(clear b1)
(on-table b2)
(clear b2)
(on-table b3)
(clear b3)
)
(:goal (and
(on b1 b2)
(on b2 b3)
)))
