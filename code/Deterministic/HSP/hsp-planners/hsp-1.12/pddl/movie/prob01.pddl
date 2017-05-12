(DEFINE (PROBLEM MOVIE-5)
   (:DOMAIN MOVIE-DOM)
   (:OBJECTS C1 C2 C3 C4 C5 ;; CHIPS
             D1 D2 D3 D4 D5 ;; DIP
             P1 P2 P3 P4 P5 ;; POP
             Z1 Z2 Z3 Z4 Z5 ;; CHEESE
             K1 K2 K3 K4 K5) ;; CRACKERS
   (:INIT (NOT (MOVIE-REWOUND))
          (NOT (COUNTER-AT-TWO-HOURS))
          (NOT (COUNTER-AT-ZERO))

          (CHIPS C1)
          (CHIPS C2)
          (CHIPS C3)
          (CHIPS C4)
          (CHIPS C5)
          (DIP D1)
          (DIP D2)
          (DIP D3)
          (DIP D4)
          (DIP D5)
          (POP P1)
          (POP P2)
          (POP P3)
          (POP P4)
          (POP P5)
          (CHEESE Z1)
          (CHEESE Z2)
          (CHEESE Z3)
          (CHEESE Z4)
          (CHEESE Z5)
          (CRACKERS K1)
          (CRACKERS K2)
          (CRACKERS K3)
          (CRACKERS K4)
          (CRACKERS K5))

   (:GOAL (AND (MOVIE-REWOUND)
               (COUNTER-AT-ZERO)
               (HAVE-CHIPS)
               (HAVE-DIP)
               (HAVE-POP)
               (HAVE-CHEESE)
               (HAVE-CRACKERS))))


