(define (problem bw-sussman)       ; graphplan 6 steps
    (:domain prodigy-bw)
  (:objects A B C)
  (:init (on-table a) (on-table b) (on c a)
		(clear b) (clear c) (arm-empty))
  (:goal (and (on a b) (on b c))))
