;; five blocks

(define (problem five) 
  (:domain blocks_world)
  (:objects A B C D E)
  (:init (on-table A) 
	 (on B A)
	 (on C B)
	 (clear C) 
	 (on-table D)
	 (on E D)
	 (clear E))
  (:goal (on A D)))

