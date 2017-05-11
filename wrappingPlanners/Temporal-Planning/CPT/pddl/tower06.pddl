(define (problem bw-tower6)
(:domain prodigy-bw)
(:objects
 b1
 b2
 b3
 b4
 b5
 b6
)
(:init
(arm-empty)
(on-table b1)
(clear b1)
(on-table b2)
(clear b2)
(on-table b3)
(clear b3)
(on-table b4)
(clear b4)
(on-table b5)
(clear b5)
(on-table b6)
(clear b6)
)
(:goal (and
(on b1 b2)
(on b2 b3)
(on b3 b4)
(on b4 b5)
(on b5 b6)
)))
