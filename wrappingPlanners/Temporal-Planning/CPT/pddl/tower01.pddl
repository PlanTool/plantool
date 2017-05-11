(define (problem bw-tower2)
(:domain prodigy-bw)
(:objects
 b1
)
(:init
(arm-empty)
(on-table b1)
(clear b1)
)
(:goal (and
(on-table b1)
)))
