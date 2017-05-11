
(define (problem engine1)
        (:domain nasa)
        (:objects
                p1 p2 p3 p4 - pipe)
        (:init
                (flow-path p1 p2)
                (flow-path p2 p3)
                )
       (:goal (and (flow-path1 p1 p2)    
                   (flow-path1 p2 p3))))

