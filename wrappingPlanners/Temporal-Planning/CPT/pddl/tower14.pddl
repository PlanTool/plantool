(define (problem bw-tower14)
(:domain prodigy-bw)
(:objects
 b1
 b2
 b3
 b4
 b5
 b6
 b7
 b8
 b9
 b10
 b11
 b12
 b13
 b14
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
(on-table b7)
(clear b7)
(on-table b8)
(clear b8)
(on-table b9)
(clear b9)
(on-table b10)
(clear b10)
(on-table b11)
(clear b11)
(on-table b12)
(clear b12)
(on-table b13)
(clear b13)
(on-table b14)
(clear b14)
)
(:goal (and
(on b1 b2)
(on b2 b3)
(on b3 b4)
(on b4 b5)
(on b5 b6)
(on b6 b7)
(on b7 b8)
(on b8 b9)
(on b9 b10)
(on b10 b11)
(on b11 b12)
(on b12 b13)
(on b13 b14)
)))
