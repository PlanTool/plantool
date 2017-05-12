
(define (problem bmtuc-10-5)
   (:domain bmtuc)
   
   (:objects  p1  p2  p3  p4  p5  p6  p7  p8  p9  p10   - p   t1  t2  t3  t4  t5   - toilet)    
   (:init (and 

         (oneof (not (nclogged t1)) (nclogged t1))
     
         (oneof (not (nclogged t2)) (nclogged t2))
     
         (oneof (not (nclogged t3)) (nclogged t3))
     
         (oneof (not (nclogged t4)) (nclogged t4))
     
         (oneof (not (nclogged t5)) (nclogged t5))
     
     (oneof  (pos p1)  (pos p2)  (pos p3)  (pos p4)  (pos p5)  (pos p6)  (pos p7)  (pos p8)  (pos p9)  (pos p10)  ))) 
    (:goal (defused))
)
