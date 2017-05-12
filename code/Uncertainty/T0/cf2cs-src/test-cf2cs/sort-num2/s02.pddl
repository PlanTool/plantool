
(define (problem s2)
    (:domain s)
    (:init (and  
              (or (less n1 n2) (not (less n1 n2))) 
              (or (less n2 n1) (not (less n2 n1))) 
    ))
    (:goal (and 
             (less n1 n2) 
    ))
)

