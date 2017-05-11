;;; bw-large-a
;;;
;;; Initial:  C/B/A   E/D   I/H/G/F
;;; Goal:     A/E     H/I/D/    B/C/G/F
;;; Length: AB

(define (problem bw-large-a)
  (:domain prodigy-bw)
  (:objects A B C D E F G H I)
  (:init (arm-empty)
	 (on C B)
	 (on B A)
	 (on-table A)
	 (on E D)
	 (on-table D)
	 (on I H)
	 (on H G)
	 (on G F)
	 (on-table F)
	 (clear C)
	 (clear E)
	 (clear I))
  (:goal (and
	  (on A E)
	  (on-table E)
	  (on H I)
	  (on I D)
	  (on-table D)
	  (on B C)
	  (on C G)
	  (on G F)
	  (on-table F)
	  )))
