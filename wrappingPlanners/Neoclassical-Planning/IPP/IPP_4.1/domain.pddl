(define (domain propositional)
  (:predicates
    (ON_B4_B5)
    (CLEAR_B7)
    (ON_B4_B6)
    (ON_B4_B9)
    (ON_B4_B10)
    (ON_B7_B4)
    (CLEAR_B2)
    (ON_B7_B5)
    (ON_B7_B6)
    (ON_B7_B9)
    (ON_B7_B10)
    (ON_B9_B2)
    (CLEAR_B1)
    (ON_B9_B4)
    (ON_B9_B5)
    (ON_B9_B6)
    (ON_B9_B7)
    (ON_B9_B10)
    (ON-TABLE_B1)
    (CLEAR_B3)
    (ON-TABLE_B3)
    (CLEAR_B8)
    (ON-TABLE_B4)
    (ON-TABLE_B7)
    (ON-TABLE_B9)
    (ON_B1_B2)
    (ON_B1_B4)
    (ON_B1_B5)
    (ON_B1_B6)
    (ON_B1_B7)
    (ON_B1_B8)
    (ON_B1_B9)
    (ON_B1_B10)
    (ON_B2_B1)
    (ON_B2_B3)
    (ON_B2_B4)
    (ON_B2_B5)
    (ON_B2_B6)
    (ON_B2_B7)
    (ON_B2_B8)
    (ON_B2_B9)
    (ON_B2_B10)
    (ON_B3_B1)
    (ON_B3_B2)
    (ON_B3_B4)
    (ON_B3_B5)
    (ON_B3_B6)
    (ON_B3_B7)
    (ON_B3_B9)
    (ON_B3_B10)
    (ON_B4_B1)
    (ON_B4_B2)
    (ON_B4_B3)
    (ON_B4_B8)
    (ON_B5_B1)
    (ON_B5_B2)
    (ON_B5_B3)
    (ON_B5_B4)
    (ON_B5_B6)
    (ON_B5_B7)
    (ON_B5_B8)
    (ON_B5_B9)
    (ON_B5_B10)
    (ON_B6_B1)
    (ON_B6_B2)
    (ON_B6_B3)
    (ON_B6_B4)
    (ON_B6_B5)
    (ON_B6_B7)
    (ON_B6_B8)
    (ON_B6_B9)
    (ON_B6_B10)
    (ON_B7_B1)
    (ON_B7_B3)
    (ON_B7_B8)
    (ON_B8_B1)
    (ON_B8_B2)
    (ON_B8_B3)
    (ON_B8_B4)
    (ON_B8_B5)
    (ON_B8_B6)
    (ON_B8_B7)
    (ON_B8_B9)
    (ON_B8_B10)
    (ON_B9_B3)
    (ON_B9_B8)
    (ON_B10_B1)
    (ON_B10_B2)
    (ON_B10_B3)
    (ON_B10_B4)
    (ON_B10_B5)
    (ON_B10_B6)
    (ON_B10_B7)
    (ON_B10_B8)
    (ON_B10_B9)
    (CLEAR_B6)
    (CLEAR_B5)
    (CLEAR_B4)
    (CLEAR_B9)
    (CLEAR_B10)
    (ON_B9_B1)
    (ON_B7_B2)
    (ON_B4_B7)
    (ON_B3_B8)
    (ON_B1_B3)
    (ON-TABLE_B10)
    (ON-TABLE_B8)
    (ON-TABLE_B6)
    (ON-TABLE_B5)
    (ON-TABLE_B2)
  )
  (:action MOVE-B-TO-T_B10_B9
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B9)
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-T_B10_B8
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B8)
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-T_B10_B7
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B7)
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-T_B10_B6
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B6)
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-T_B10_B5
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B5)
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-T_B10_B4
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B4)
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-T_B10_B3
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B3)
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-T_B10_B2
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B2)
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-T_B10_B1
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON-TABLE_B10)
        (CLEAR_B1)
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-T_B9_B8
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B8)
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-T_B9_B3
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B3)
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-T_B8_B10
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B10)
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-T_B8_B9
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B9)
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-T_B8_B7
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B7)
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-T_B8_B6
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B6)
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-T_B8_B5
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B5)
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-T_B8_B4
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B4)
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-T_B8_B3
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B3)
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-T_B8_B2
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B2)
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-T_B8_B1
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON-TABLE_B8)
        (CLEAR_B1)
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-T_B7_B8
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B8)
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-T_B7_B3
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B3)
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-T_B7_B1
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B1)
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-T_B6_B10
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B10)
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-T_B6_B9
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B9)
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-T_B6_B8
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B8)
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-T_B6_B7
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B7)
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-T_B6_B5
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B5)
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-T_B6_B4
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B4)
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-T_B6_B3
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B3)
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-T_B6_B2
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B2)
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-T_B6_B1
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON-TABLE_B6)
        (CLEAR_B1)
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-T_B5_B10
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B10)
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-T_B5_B9
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B9)
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-T_B5_B8
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B8)
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-T_B5_B7
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B7)
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-T_B5_B6
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B6)
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-T_B5_B4
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B4)
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-T_B5_B3
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B3)
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-T_B5_B2
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B2)
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-T_B5_B1
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON-TABLE_B5)
        (CLEAR_B1)
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-T_B4_B8
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B8)
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-T_B4_B3
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B3)
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-T_B4_B2
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B2)
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-T_B4_B1
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B1)
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-T_B3_B10
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B10)
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-T_B3_B9
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B9)
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-T_B3_B7
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B7)
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-T_B3_B6
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B6)
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-T_B3_B5
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B5)
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-T_B3_B4
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B4)
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-T_B3_B2
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B2)
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-T_B3_B1
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B1)
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-T_B2_B10
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B10)
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-T_B2_B9
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B9)
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-T_B2_B8
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B8)
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-T_B2_B7
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B7)
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-T_B2_B6
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B6)
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-T_B2_B5
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B5)
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-T_B2_B4
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B4)
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-T_B2_B3
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B3)
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-T_B2_B1
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON-TABLE_B2)
        (CLEAR_B1)
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-T_B1_B10
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B10)
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-T_B1_B9
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B9)
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-T_B1_B8
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B8)
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-T_B1_B7
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B7)
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-T_B1_B6
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B6)
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-T_B1_B5
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B5)
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-T_B1_B4
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B4)
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-T_B1_B2
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B2)
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B8
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B7
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B6
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B5
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B4
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B3
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B2
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B9_B1
    :precondition
      (and
        (ON_B10_B9)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B10_B9))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B9
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B7
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B6
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B5
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B4
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B3
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B2
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B8_B1
    :precondition
      (and
        (ON_B10_B8)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B10_B8))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B9
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B8
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B6
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B5
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B4
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B3
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B2
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B7_B1
    :precondition
      (and
        (ON_B10_B7)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B10_B7))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B9
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B8
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B7
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B5
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B4
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B3
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B2
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B6_B1
    :precondition
      (and
        (ON_B10_B6)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B10_B6))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B9
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B8
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B7
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B6
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B4
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B3
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B2
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B5_B1
    :precondition
      (and
        (ON_B10_B5)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B10_B5))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B9
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B8
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B7
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B6
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B5
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B3
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B2
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B4_B1
    :precondition
      (and
        (ON_B10_B4)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B10_B4))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B9
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B8
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B7
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B6
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B5
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B4
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B2
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B3_B1
    :precondition
      (and
        (ON_B10_B3)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B10_B3))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B9
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B8
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B7
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B6
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B5
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B4
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B3
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B2_B1
    :precondition
      (and
        (ON_B10_B2)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B10_B2))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B9
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B8
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B7
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B6
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B5
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B4
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B3
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B10_B1_B2
    :precondition
      (and
        (ON_B10_B1)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B10_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B8
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B3
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B10
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B7
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B6
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B5
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B4
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B3
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B2
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B8_B1
    :precondition
      (and
        (ON_B9_B8)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B9_B8))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B8
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B3
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B8
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B3
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B8
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B3
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B8
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B3
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B10
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B8
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B7
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B6
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B5
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B4
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B2
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B3_B1
    :precondition
      (and
        (ON_B9_B3)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B9_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B8
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B3
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B8
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B3
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B9
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B7
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B6
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B5
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B4
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B3
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B2
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B10_B1
    :precondition
      (and
        (ON_B8_B10)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B8_B10))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B10
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B7
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B6
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B5
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B4
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B3
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B2
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B9_B1
    :precondition
      (and
        (ON_B8_B9)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B8_B9))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B10
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B9
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B6
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B5
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B4
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B3
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B2
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B7_B1
    :precondition
      (and
        (ON_B8_B7)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B8_B7))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B10
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B9
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B7
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B5
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B4
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B3
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B2
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B6_B1
    :precondition
      (and
        (ON_B8_B6)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B8_B6))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B10
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B9
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B7
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B6
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B4
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B3
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B2
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B5_B1
    :precondition
      (and
        (ON_B8_B5)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B8_B5))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B10
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B9
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B7
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B6
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B5
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B3
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B2
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B4_B1
    :precondition
      (and
        (ON_B8_B4)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B8_B4))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B10
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B9
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B7
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B6
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B5
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B4
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B2
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B3_B1
    :precondition
      (and
        (ON_B8_B3)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B8_B3))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B10
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B9
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B7
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B6
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B5
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B4
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B3
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B2_B1
    :precondition
      (and
        (ON_B8_B2)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B8_B2))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B10
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B9
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B7
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B6
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B5
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B4
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B3
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B8_B1_B2
    :precondition
      (and
        (ON_B8_B1)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B8_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B8
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B3
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B1
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B8
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B3
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B1
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B10
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B9
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B6
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B5
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B4
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B3
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B2
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B8_B1
    :precondition
      (and
        (ON_B7_B8)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B7_B8))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B8
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B3
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B1
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B8
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B3
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B1
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B8
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B3
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B1
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B10
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B9
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B8
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B6
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B5
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B4
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B2
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B3_B1
    :precondition
      (and
        (ON_B7_B3)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B7_B3))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B8
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B3
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B1
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B10
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B9
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B8
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B6
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B5
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B4
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B3
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B1_B2
    :precondition
      (and
        (ON_B7_B1)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B7_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B9
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B8
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B7
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B5
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B4
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B3
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B2
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B10_B1
    :precondition
      (and
        (ON_B6_B10)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B6_B10))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B10
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B8
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B7
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B5
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B4
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B3
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B2
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B9_B1
    :precondition
      (and
        (ON_B6_B9)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B6_B9))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B10
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B9
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B7
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B5
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B4
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B3
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B2
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B8_B1
    :precondition
      (and
        (ON_B6_B8)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B6_B8))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B10
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B9
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B8
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B5
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B4
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B3
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B2
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B7_B1
    :precondition
      (and
        (ON_B6_B7)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B6_B7))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B10
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B9
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B8
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B7
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B4
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B3
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B2
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B5_B1
    :precondition
      (and
        (ON_B6_B5)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B6_B5))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B10
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B9
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B8
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B7
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B5
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B3
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B2
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B4_B1
    :precondition
      (and
        (ON_B6_B4)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B6_B4))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B10
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B9
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B8
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B7
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B5
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B4
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B2
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B3_B1
    :precondition
      (and
        (ON_B6_B3)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B6_B3))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B10
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B9
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B8
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B7
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B5
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B4
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B3
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B2_B1
    :precondition
      (and
        (ON_B6_B2)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B6_B2))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B10
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B9
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B8
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B7
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B5
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B4
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B3
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B6_B1_B2
    :precondition
      (and
        (ON_B6_B1)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B6_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B9
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B8
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B7
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B6
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B4
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B3
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B2
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B10_B1
    :precondition
      (and
        (ON_B5_B10)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B5_B10))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B10
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B8
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B7
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B6
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B4
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B3
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B2
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B9_B1
    :precondition
      (and
        (ON_B5_B9)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B5_B9))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B10
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B9
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B7
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B6
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B4
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B3
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B2
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B8_B1
    :precondition
      (and
        (ON_B5_B8)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B5_B8))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B10
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B9
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B8
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B6
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B4
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B3
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B2
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B7_B1
    :precondition
      (and
        (ON_B5_B7)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B5_B7))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B10
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B9
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B8
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B7
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B4
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B3
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B2
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B6_B1
    :precondition
      (and
        (ON_B5_B6)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B5_B6))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B10
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B9
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B8
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B7
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B6
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B3
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B2
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B4_B1
    :precondition
      (and
        (ON_B5_B4)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B5_B4))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B10
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B9
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B8
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B7
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B6
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B4
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B2
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B3_B1
    :precondition
      (and
        (ON_B5_B3)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B5_B3))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B10
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B9
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B8
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B7
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B6
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B4
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B3
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B2_B1
    :precondition
      (and
        (ON_B5_B2)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B5_B2))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B10
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B9
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B8
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B7
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B6
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B4
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B3
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B5_B1_B2
    :precondition
      (and
        (ON_B5_B1)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B5_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B8
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B3
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B2
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B1
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B8
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B3
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B2
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B1
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B10
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B9
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B7
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B6
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B5
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B3
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B2
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B8_B1
    :precondition
      (and
        (ON_B4_B8)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B4_B8))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B8
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B3
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B2
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B1
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B10
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B9
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B8
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B7
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B5
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B3
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B2
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B6_B1
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B10
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B9
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B8
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B7
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B6
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B3
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B2
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B5_B1
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B10
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B9
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B8
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B7
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B6
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B5
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B2
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B3_B1
    :precondition
      (and
        (ON_B4_B3)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B4_B3))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B10
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B9
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B8
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B7
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B6
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B5
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B3
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B2_B1
    :precondition
      (and
        (ON_B4_B2)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B4_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B10
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B9
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B8
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B7
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B6
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B5
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B3
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B4_B1_B2
    :precondition
      (and
        (ON_B4_B1)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B4_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B9
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B8
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B7
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B6
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B5
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B4
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B2
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B10_B1
    :precondition
      (and
        (ON_B3_B10)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B3_B10))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B10
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B8
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B7
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B6
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B5
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B4
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B2
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B9_B1
    :precondition
      (and
        (ON_B3_B9)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B3_B9))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B10
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B9
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B7
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B6
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B5
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B4
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B2
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B8_B1
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B10
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B9
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B8
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B6
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B5
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B4
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B2
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B7_B1
    :precondition
      (and
        (ON_B3_B7)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B3_B7))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B10
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B9
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B8
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B7
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B5
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B4
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B2
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B6_B1
    :precondition
      (and
        (ON_B3_B6)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B3_B6))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B10
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B9
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B8
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B7
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B6
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B4
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B2
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B5_B1
    :precondition
      (and
        (ON_B3_B5)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B3_B5))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B10
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B9
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B8
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B7
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B6
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B5
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B2
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B4_B1
    :precondition
      (and
        (ON_B3_B4)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B3_B4))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B10
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B9
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B8
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B7
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B6
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B5
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B4
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B2_B1
    :precondition
      (and
        (ON_B3_B2)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B3_B2))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B10
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B9
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B8
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B7
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B6
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B5
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B4
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B3_B1_B2
    :precondition
      (and
        (ON_B3_B1)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B3_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B9
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B8
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B7
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B6
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B5
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B4
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B3
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B10_B1
    :precondition
      (and
        (ON_B2_B10)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B2_B10))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B10
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B8
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B7
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B6
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B5
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B4
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B3
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B9_B1
    :precondition
      (and
        (ON_B2_B9)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B9)
        (not (CLEAR_B1))
        (not (ON_B2_B9))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B10
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B9
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B7
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B6
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B5
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B4
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B3
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B8_B1
    :precondition
      (and
        (ON_B2_B8)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B8)
        (not (CLEAR_B1))
        (not (ON_B2_B8))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B10
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B9
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B8
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B6
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B5
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B4
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B3
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B7_B1
    :precondition
      (and
        (ON_B2_B7)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B2_B7))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B10
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B9
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B8
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B7
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B5
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B4
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B3
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B6_B1
    :precondition
      (and
        (ON_B2_B6)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B2_B6))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B10
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B9
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B8
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B7
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B6
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B4
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B3
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B5_B1
    :precondition
      (and
        (ON_B2_B5)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B2_B5))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B10
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B9
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B8
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B7
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B6
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B5
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B3
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B4_B1
    :precondition
      (and
        (ON_B2_B4)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B2_B4))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B10
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B9
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B8
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B7
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B6
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B5
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B4
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B3_B1
    :precondition
      (and
        (ON_B2_B3)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (CLEAR_B3)
        (not (CLEAR_B1))
        (not (ON_B2_B3))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B10
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B9
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (CLEAR_B1)
        (not (CLEAR_B9))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B8
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (CLEAR_B1)
        (not (CLEAR_B8))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B7
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B6
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B5
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B4
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B2_B1_B3
    :precondition
      (and
        (ON_B2_B1)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (CLEAR_B1)
        (not (CLEAR_B3))
        (not (ON_B2_B1))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B9
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B8
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B10)
        (not (CLEAR_B8))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B7
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B6
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B5
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B4
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B3
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B10)
        (not (CLEAR_B3))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B10_B2
    :precondition
      (and
        (ON_B1_B10)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B1_B10))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B10
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B8
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B9)
        (not (CLEAR_B8))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B7
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B6
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B5
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B4
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B3
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B9)
        (not (CLEAR_B3))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B9_B2
    :precondition
      (and
        (ON_B1_B9)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B1_B9))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B10
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B8)
        (not (CLEAR_B10))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B9
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B8)
        (not (CLEAR_B9))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B7
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B8)
        (not (CLEAR_B7))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B6
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B8)
        (not (CLEAR_B6))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B5
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B8)
        (not (CLEAR_B5))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B4
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B8)
        (not (CLEAR_B4))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B3
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B8)
        (not (CLEAR_B3))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B8_B2
    :precondition
      (and
        (ON_B1_B8)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B8)
        (not (CLEAR_B2))
        (not (ON_B1_B8))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B10
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B9
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B8
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B7)
        (not (CLEAR_B8))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B6
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B5
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B4
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B3
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B7)
        (not (CLEAR_B3))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B7_B2
    :precondition
      (and
        (ON_B1_B7)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B1_B7))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B10
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B9
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B8
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B6)
        (not (CLEAR_B8))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B7
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B5
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B4
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B3
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B6)
        (not (CLEAR_B3))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B6_B2
    :precondition
      (and
        (ON_B1_B6)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B1_B6))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B10
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B9
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B8
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B5)
        (not (CLEAR_B8))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B7
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B6
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B4
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B3
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B5)
        (not (CLEAR_B3))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B5_B2
    :precondition
      (and
        (ON_B1_B5)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B1_B5))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B10
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B9
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B8
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B4)
        (not (CLEAR_B8))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B7
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B6
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B5
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B3
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B4)
        (not (CLEAR_B3))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B4_B2
    :precondition
      (and
        (ON_B1_B4)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B1_B4))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B10
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B3)
        (not (CLEAR_B10))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B9
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B3)
        (not (CLEAR_B9))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B8
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B3)
        (not (CLEAR_B8))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B7
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B3)
        (not (CLEAR_B7))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B6
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B3)
        (not (CLEAR_B6))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B5
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B3)
        (not (CLEAR_B5))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B4
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B3)
        (not (CLEAR_B4))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B3_B2
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (CLEAR_B3)
        (not (CLEAR_B2))
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B10
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B9
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B8
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (CLEAR_B2)
        (not (CLEAR_B8))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B7
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B6
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B5
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B4
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-B-TO-B_B1_B2_B3
    :precondition
      (and
        (ON_B1_B2)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (CLEAR_B2)
        (not (CLEAR_B3))
        (not (ON_B1_B2))
      )
  )
  (:action MOVE-T-TO-B_B10_B9
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B9)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B8
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B8)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B7
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B7)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B6
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B6)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B5
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B5)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B4
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B4)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B3
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B3)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B2
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B2)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B10_B1
    :precondition
      (and
        (ON-TABLE_B10)
        (CLEAR_B1)
        (CLEAR_B10)
      )
    :effect
      (and
        (ON_B10_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B10))
      )
  )
  (:action MOVE-T-TO-B_B9_B10
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B8
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B8)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B7
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B6
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B5
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B4
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B3
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B3)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B2
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B9_B1
    :precondition
      (and
        (ON-TABLE_B9)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B9))
      )
  )
  (:action MOVE-T-TO-B_B8_B10
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B10)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B9
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B9)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B7
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B7)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B6
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B6)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B5
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B5)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B4
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B4)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B3
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B3)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B2
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B2)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B8_B1
    :precondition
      (and
        (ON-TABLE_B8)
        (CLEAR_B1)
        (CLEAR_B8)
      )
    :effect
      (and
        (ON_B8_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B8))
      )
  )
  (:action MOVE-T-TO-B_B7_B10
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B9
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B8
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B8)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B6
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B5
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B4
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B3
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B3)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B2
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B7_B1
    :precondition
      (and
        (ON-TABLE_B7)
        (CLEAR_B1)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B7))
      )
  )
  (:action MOVE-T-TO-B_B6_B10
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B10)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B9
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B9)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B8
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B8)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B7
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B7)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B5
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B5)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B4
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B4)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B3
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B3)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B2
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B2)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B6_B1
    :precondition
      (and
        (ON-TABLE_B6)
        (CLEAR_B1)
        (CLEAR_B6)
      )
    :effect
      (and
        (ON_B6_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B6))
      )
  )
  (:action MOVE-T-TO-B_B5_B10
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B10)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B9
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B9)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B8
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B8)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B7
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B7)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B6
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B6)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B4
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B4)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B3
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B3)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B2
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B2)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B5_B1
    :precondition
      (and
        (ON-TABLE_B5)
        (CLEAR_B1)
        (CLEAR_B5)
      )
    :effect
      (and
        (ON_B5_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B5))
      )
  )
  (:action MOVE-T-TO-B_B4_B10
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B9
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B8
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B8)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B7
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B6
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B5
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B3
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B3)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B2
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B2)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B4_B1
    :precondition
      (and
        (ON-TABLE_B4)
        (CLEAR_B1)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B4))
      )
  )
  (:action MOVE-T-TO-B_B3_B10
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B10)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B9
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B9)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B8
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B7
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B7)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B6
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B6)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B5
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B5)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B4
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B4)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B2
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B2)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B3_B1
    :precondition
      (and
        (ON-TABLE_B3)
        (CLEAR_B1)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON_B3_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B3))
      )
  )
  (:action MOVE-T-TO-B_B2_B10
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B10)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B9
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B9)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B8
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B8)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B7
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B7)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B6
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B6)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B5
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B5)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B4
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B4)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B3
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B3)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B2_B1
    :precondition
      (and
        (ON-TABLE_B2)
        (CLEAR_B1)
        (CLEAR_B2)
      )
    :effect
      (and
        (ON_B2_B1)
        (not (CLEAR_B1))
        (not (ON-TABLE_B2))
      )
  )
  (:action MOVE-T-TO-B_B1_B10
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B10)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B10)
        (not (CLEAR_B10))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B9
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B9)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B9)
        (not (CLEAR_B9))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B8
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B8)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B8)
        (not (CLEAR_B8))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B7
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B7)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B7)
        (not (CLEAR_B7))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B6
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B6)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B6)
        (not (CLEAR_B6))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B5
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B5)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B5)
        (not (CLEAR_B5))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B4
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B4)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B4)
        (not (CLEAR_B4))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B3
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B3)
        (not (CLEAR_B3))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-T-TO-B_B1_B2
    :precondition
      (and
        (ON-TABLE_B1)
        (CLEAR_B2)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON_B1_B2)
        (not (CLEAR_B2))
        (not (ON-TABLE_B1))
      )
  )
  (:action MOVE-B-TO-T_B9_B10
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B10)
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-T_B9_B7
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B7)
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-T_B9_B6
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B6)
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-T_B9_B5
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B5)
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-T_B9_B4
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B4)
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-T_B9_B2
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B2)
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-T_B9_B1
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON-TABLE_B9)
        (CLEAR_B1)
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-T_B7_B10
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B10)
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-T_B7_B9
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B9)
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-T_B7_B6
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B6)
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-T_B7_B5
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B5)
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-T_B7_B4
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B4)
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-T_B7_B2
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON-TABLE_B7)
        (CLEAR_B2)
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-T_B4_B10
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B10)
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-T_B4_B9
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B9)
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-T_B4_B7
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B7)
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-T_B4_B6
    :precondition
      (and
        (ON_B4_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B6)
        (not (ON_B4_B6))
      )
  )
  (:action MOVE-B-TO-T_B4_B5
    :precondition
      (and
        (ON_B4_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON-TABLE_B4)
        (CLEAR_B5)
        (not (ON_B4_B5))
      )
  )
  (:action MOVE-B-TO-T_B3_B8
    :precondition
      (and
        (ON_B3_B8)
        (CLEAR_B3)
      )
    :effect
      (and
        (ON-TABLE_B3)
        (CLEAR_B8)
        (not (ON_B3_B8))
      )
  )
  (:action MOVE-B-TO-T_B1_B3
    :precondition
      (and
        (ON_B1_B3)
        (CLEAR_B1)
      )
    :effect
      (and
        (ON-TABLE_B1)
        (CLEAR_B3)
        (not (ON_B1_B3))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B7
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B6
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B5
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B4
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B2
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B10_B1
    :precondition
      (and
        (ON_B9_B10)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B10)
        (not (CLEAR_B1))
        (not (ON_B9_B10))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B10
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B6
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B5
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B4
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B7)
        (not (CLEAR_B4))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B2
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B7)
        (not (CLEAR_B2))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B7_B1
    :precondition
      (and
        (ON_B9_B7)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B7)
        (not (CLEAR_B1))
        (not (ON_B9_B7))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B10
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B7
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B6)
        (not (CLEAR_B7))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B5
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B4
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B2
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B6_B1
    :precondition
      (and
        (ON_B9_B6)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B6)
        (not (CLEAR_B1))
        (not (ON_B9_B6))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B10
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B7
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B5)
        (not (CLEAR_B7))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B6
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B4
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B2
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B5_B1
    :precondition
      (and
        (ON_B9_B5)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B5)
        (not (CLEAR_B1))
        (not (ON_B9_B5))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B10
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B7
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B4)
        (not (CLEAR_B7))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B6
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B5
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B2
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B4_B1
    :precondition
      (and
        (ON_B9_B4)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B4)
        (not (CLEAR_B1))
        (not (ON_B9_B4))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B10
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B7
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B2)
        (not (CLEAR_B7))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B6
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B5
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B4
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B2_B1
    :precondition
      (and
        (ON_B9_B2)
        (CLEAR_B1)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B1)
        (CLEAR_B2)
        (not (CLEAR_B1))
        (not (ON_B9_B2))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B10
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B10)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B10)
        (CLEAR_B1)
        (not (CLEAR_B10))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B7
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B7)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B7)
        (CLEAR_B1)
        (not (CLEAR_B7))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B6
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B6)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B6)
        (CLEAR_B1)
        (not (CLEAR_B6))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B5
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B5)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B5)
        (CLEAR_B1)
        (not (CLEAR_B5))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B4
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B4)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B4)
        (CLEAR_B1)
        (not (CLEAR_B4))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B9_B1_B2
    :precondition
      (and
        (ON_B9_B1)
        (CLEAR_B2)
        (CLEAR_B9)
      )
    :effect
      (and
        (ON_B9_B2)
        (CLEAR_B1)
        (not (CLEAR_B2))
        (not (ON_B9_B1))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B9
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B6
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B5
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B4
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B10)
        (not (CLEAR_B4))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B10_B2
    :precondition
      (and
        (ON_B7_B10)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B10)
        (not (CLEAR_B2))
        (not (ON_B7_B10))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B10
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B6
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B5
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B4
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B9)
        (not (CLEAR_B4))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B9_B2
    :precondition
      (and
        (ON_B7_B9)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B9)
        (not (CLEAR_B2))
        (not (ON_B7_B9))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B10
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B6)
        (not (CLEAR_B10))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B9
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B6)
        (not (CLEAR_B9))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B5
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B6)
        (not (CLEAR_B5))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B4
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B6)
        (not (CLEAR_B4))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B6_B2
    :precondition
      (and
        (ON_B7_B6)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B6)
        (not (CLEAR_B2))
        (not (ON_B7_B6))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B10
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B5)
        (not (CLEAR_B10))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B9
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B5)
        (not (CLEAR_B9))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B6
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B5)
        (not (CLEAR_B6))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B4
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B5)
        (not (CLEAR_B4))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B5_B2
    :precondition
      (and
        (ON_B7_B5)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B5)
        (not (CLEAR_B2))
        (not (ON_B7_B5))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B10
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B4)
        (not (CLEAR_B10))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B9
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B4)
        (not (CLEAR_B9))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B6
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B4)
        (not (CLEAR_B6))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B5
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B4)
        (not (CLEAR_B5))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B4_B2
    :precondition
      (and
        (ON_B7_B4)
        (CLEAR_B2)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B2)
        (CLEAR_B4)
        (not (CLEAR_B2))
        (not (ON_B7_B4))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B10
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B10)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B10)
        (CLEAR_B2)
        (not (CLEAR_B10))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B9
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B9)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B9)
        (CLEAR_B2)
        (not (CLEAR_B9))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B6
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B6)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B6)
        (CLEAR_B2)
        (not (CLEAR_B6))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B5
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B5)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B5)
        (CLEAR_B2)
        (not (CLEAR_B5))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B7_B2_B4
    :precondition
      (and
        (ON_B7_B2)
        (CLEAR_B4)
        (CLEAR_B7)
      )
    :effect
      (and
        (ON_B7_B4)
        (CLEAR_B2)
        (not (CLEAR_B4))
        (not (ON_B7_B2))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B9
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B10)
        (not (CLEAR_B9))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B7
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B10)
        (not (CLEAR_B7))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B6
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B10)
        (not (CLEAR_B6))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B10_B5
    :precondition
      (and
        (ON_B4_B10)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B10)
        (not (CLEAR_B5))
        (not (ON_B4_B10))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B10
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B9)
        (not (CLEAR_B10))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B7
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B7)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B7)
        (CLEAR_B9)
        (not (CLEAR_B7))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B6
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B9)
        (not (CLEAR_B6))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B9_B5
    :precondition
      (and
        (ON_B4_B9)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B9)
        (not (CLEAR_B5))
        (not (ON_B4_B9))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B10
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B10)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B10)
        (CLEAR_B7)
        (not (CLEAR_B10))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B9
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B9)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B9)
        (CLEAR_B7)
        (not (CLEAR_B9))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B6
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B6)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B6)
        (CLEAR_B7)
        (not (CLEAR_B6))
        (not (ON_B4_B7))
      )
  )
  (:action MOVE-B-TO-B_B4_B7_B5
    :precondition
      (and
        (ON_B4_B7)
        (CLEAR_B5)
        (CLEAR_B4)
      )
    :effect
      (and
        (ON_B4_B5)
        (CLEAR_B7)
        (not (CLEAR_B5))
        (not (ON_B4_B7))
      )
  )
)
