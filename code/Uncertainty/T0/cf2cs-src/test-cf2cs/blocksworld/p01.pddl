(define (problem b2)
  (:domain blocks)
  (:objects A B - block)
  (:init
    (and (oneof (handempty) (holding A) (holding B))          ; (holding ?x)
         (oneof (holding A) (clear A) (on B A))               ; (above A ?x)
         (oneof (holding A) (ontable A) (on A B))             ; (on A ?x)
         (oneof (holding B) (clear B) (on A B))               ; (above B ?x)
         (oneof (holding B) (ontable B) (on B A))             ; (on B ?x)

         (or (not (handempty)) (not (holding A)))
         (or (not (handempty)) (not (holding B)))
         (or (not (holding A)) (not (holding B)))

         (or (not (holding A)) (not (clear A)))
         (or (not (holding A)) (not (on B A)))
         (or (not (clear A)) (not (on B A)))

         (or (not (holding A)) (not (ontable A)))
         (or (not (holding A)) (not (on A B)))
         (or (not (ontable A)) (not (on A B)))

         (or (not (holding B)) (not (clear B)))
         (or (not (holding B)) (not (on A B)))
         (or (not (clear B)) (not (on A B)))

         (or (not (holding B)) (not (clear B)))
         (or (not (holding B)) (not (on A B)))
         (or (not (clear B)) (not (on A B)))

         (or (not (on A B)) (not (on B A)))                   ; cycles
    )
  )
  (:goal (and (ontable A) (on B A)))
)
