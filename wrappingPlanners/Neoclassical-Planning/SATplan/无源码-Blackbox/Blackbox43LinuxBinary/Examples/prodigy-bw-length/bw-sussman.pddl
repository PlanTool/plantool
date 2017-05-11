(define (problem bw-sussman)       ; graphplan 6 steps
    (:domain prodigy-bw)
  (:length (:parallel 6) (:serial 6))
  (:objects A B C)
  (:init (on-table a) (on-table b) (on c a)
		(clear b) (clear c) (arm-empty))
  (:goal (and (on a b) (on b c))))
