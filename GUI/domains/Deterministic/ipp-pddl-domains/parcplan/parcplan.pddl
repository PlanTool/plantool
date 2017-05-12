;; 7 robots can move blocks simultaenuously
;; space on the table is limited to 5 positions

(define (problem parc)          
  (:domain parcplan)
  (:objects rob1 rob2 rob3 rob4 rob5 rob6 rob7 
   lgreen pink blue red margenta cyan lblue green yellow brown 
   t1 t2 t3 t4 t5)

(:init  

(position t1)
(position t2)
(position t3)
(position t4)
(position t5)

(block lgreen)
(block  pink)
(block blue)
(block red)
(block margenta)
(block cyan)
(block lblue)
(block green)
(block yellow)
(block brown)

(robot rob1)
(robot rob2)
(robot rob3)
(robot rob4)
(robot rob5)
(robot rob6)
(robot rob7)


(on_table blue t2) 
         (on_table margenta t3) 
         (on_table lgreen t4) 
         (on_table green t5) 
         (on yellow blue) (on brown margenta) (on cyan brown) (on red cyan)
         (on lblue lgreen) (on pink green) 
         (clear yellow) (clear red) (clear lblue) (clear pink) (clear t1)
         (arm_empty rob1) (arm_empty rob2) (arm_empty rob3) (arm_empty rob4) 
         (arm_empty rob5) (arm_empty rob6)  (arm_empty rob7))

(:goal (and (on_table red t1) (on_table green t2) (on_table brown t3) 
     (on blue red) (on pink blue) (on lgreen pink) (on margenta cyan) 
     (on cyan lblue) (on lblue green) (on yellow brown))))
