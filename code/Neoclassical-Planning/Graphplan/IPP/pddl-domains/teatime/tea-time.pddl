(define (problem serve_tea)
    (:domain tea_time)
  (:objects pioneer1 pioneer2
            hallway room1 room2 room3 room4)


(:init 
(robby pioneer1)
(robby pioneer2)
(location hallway) 
(location room1) 
(location room2) 
(location room3) 
(location room4)

(connected hallway room1) (connected hallway room2)
         (connected hallway room3) (connected hallway room4)
         (connected room1 room2) (connected room3 room4)
         (connected room1 hallway) (connected room2 hallway)
         (connected room3 hallway) (connected room4 hallway)
         (connected room2 room1) (connected room4 room3)
         (allowed pioneer1 room1) (allowed pioneer1 room2) 
         (allowed pioneer1 hallway)
         (allowed pioneer2 room3) (allowed pioneer2 room4) 
         (allowed pioneer2 hallway)
         (free pioneer1) (free pioneer2)
         (at pioneer1 room1) (at pioneer2 room3)
         (teamachine room1) (ordered room2) (cupstack room3)
         (ordered room4) (ordered room3) (ordered room1))
         
(:goal (and (served room2) (served room4) (served room3) (served room1))))
