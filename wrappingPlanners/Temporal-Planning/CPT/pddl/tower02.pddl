(define (problem bw-tower2)
(:domain prodigy-bw)
(:objects
 b1
 b2
)
(:init
(arm-empty)
(on-table b1)
(clear b1)
(on-table b2)
(clear b2)
)
(:goal (and
(on b1 b2)
)))
