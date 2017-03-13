;; this small domain is due to
;; David Andre Furcy who reported 
;; a bug in a previous version.

(define (domain smalldomain)
  (:requirements :strips)
  (:predicates (s1) (s2) (s3))
  
  (:action op1
      :effect (s1))
  
  (:action op2
      :effect (s2))
  
  (:action op3
      :effect (and (s3) (not (s2)) (not (s1)))))

