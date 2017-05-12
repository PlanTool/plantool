


(define (problem mixed-f5-p10-u0-v5-d0-a0-n0-A0-B0-N0-F0)
   (:domain miconic)
   (:objects p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 - passenger
             f0 f1 f2 f3 f4 - floor)


(:init
(above f0 f1)
(above f0 f2)
(above f0 f3)
(above f0 f4)

(above f1 f2)
(above f1 f3)
(above f1 f4)

(above f2 f3)
(above f2 f4)

(above f3 f4)



(origin p0 f3)
(destin p0 f2)

(origin p1 f4)
(destin p1 f1)

(origin p2 f4)
(destin p2 f0)

(origin p3 f0)
(destin p3 f3)

(origin p4 f3)
(destin p4 f4)

(origin p5 f2)
(destin p5 f3)

(origin p6 f2)
(destin p6 f0)

(origin p7 f1)
(destin p7 f2)

(origin p8 f0)
(destin p8 f3)

(origin p9 f0)
(destin p9 f3)






(lift-at f0)
)


(:goal (forall (?p - passenger) (served ?p)))
)


