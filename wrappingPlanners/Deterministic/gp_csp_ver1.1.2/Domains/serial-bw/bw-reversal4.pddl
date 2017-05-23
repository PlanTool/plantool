;;; bw-reversal4
;;;
;;; Initial:  a/b/c/d
;;; Goal:     d/c/b/a
;;; Length: 8

(define (problem bw-reversal4)
  (:domain prodigy-bw)
  (:objects a b c d)
  (:init (arm-empty)
	 (on a b)
	 (on b c)
	 (on c d)
	 (on-table d)
	 (clear a))
  (:goal (and
	  (on d c)
	  (on c b)
	  (on b a)
	  (on-table a)
	  (clear d))))
