(define (domain grounded-pathways-propositional)
  (:requirements :typing) 
  (:predicates 
     (available.ap2)
     (available.cdc25c)
     (available.cdc25cp2)
     (available.cdk46p3-cycd)
     (available.cdk46p3-cycdp1)
     (available.cdk7)
     (available.chk1)
     (available.dmp1)
     (available.dmp1p1)
     (available.e2f13)
     (available.p130-e2f5p1-dp12)
     (available.p16)
     (available.p16-cdk7)
     (available.p300)
     (available.pcaf)
     (available.pcaf-p300)
     (available.prb-e2f4p1-dp12)
     (available.prbp1p2)
     (available.prbp1p2-ap2)
     (available.prbp2)
     (available.prbp2-ap2)
     (available.raf1)
     (available.raf1-p130-e2f5p1-dp12)
     (available.raf1-prb-e2f4p1-dp12)
     (available.sp1)
     (available.sp1-e2f13)
     (chosen.ap2)
     (chosen.cdc25c)
     (chosen.cdk46p3-cycd)
     (chosen.cdk46p3-cycdp1)
     (chosen.cdk7)
     (chosen.chk1)
     (chosen.dmp1)
     (chosen.e2f13)
     (chosen.p130-e2f5p1-dp12)
     (chosen.p16)
     (chosen.p300)
     (chosen.pcaf)
     (chosen.prb-e2f4p1-dp12)
     (chosen.prbp2)
     (chosen.raf1)
     (chosen.sp1)
     (num-subs.l0)
     (num-subs.l1)
     (num-subs.l2)
     (num-subs.l3)
     (goal1)
  )
  (:action dummy-action-1
   :precondition 
     (and 
      (available.pcaf-p300) 
     )
   :effect 
     (and 
      (goal1) 
     )
  )
  (:action dummy-action-1
   :precondition 
     (and 
      (available.prbp1p2-ap2) 
     )
   :effect 
     (and 
      (goal1) 
     )
  )
  (:action initialize.ap2
   :precondition 
     (and 
      (chosen.ap2) 
     )
   :effect 
     (and 
      (available.ap2) 
     )
  )
  (:action initialize.cdc25c
   :precondition 
     (and 
      (chosen.cdc25c) 
     )
   :effect 
     (and 
      (available.cdc25c) 
     )
  )
  (:action initialize.cdk46p3-cycd
   :precondition 
     (and 
      (chosen.cdk46p3-cycd) 
     )
   :effect 
     (and 
      (available.cdk46p3-cycd) 
     )
  )
  (:action initialize.cdk46p3-cycdp1
   :precondition 
     (and 
      (chosen.cdk46p3-cycdp1) 
     )
   :effect 
     (and 
      (available.cdk46p3-cycdp1) 
     )
  )
  (:action initialize.cdk7
   :precondition 
     (and 
      (chosen.cdk7) 
     )
   :effect 
     (and 
      (available.cdk7) 
     )
  )
  (:action initialize.chk1
   :precondition 
     (and 
      (chosen.chk1) 
     )
   :effect 
     (and 
      (available.chk1) 
     )
  )
  (:action initialize.dmp1
   :precondition 
     (and 
      (chosen.dmp1) 
     )
   :effect 
     (and 
      (available.dmp1) 
     )
  )
  (:action initialize.e2f13
   :precondition 
     (and 
      (chosen.e2f13) 
     )
   :effect 
     (and 
      (available.e2f13) 
     )
  )
  (:action initialize.p130-e2f5p1-dp12
   :precondition 
     (and 
      (chosen.p130-e2f5p1-dp12) 
     )
   :effect 
     (and 
      (available.p130-e2f5p1-dp12) 
     )
  )
  (:action initialize.p16
   :precondition 
     (and 
      (chosen.p16) 
     )
   :effect 
     (and 
      (available.p16) 
     )
  )
  (:action initialize.p300
   :precondition 
     (and 
      (chosen.p300) 
     )
   :effect 
     (and 
      (available.p300) 
     )
  )
  (:action initialize.pcaf
   :precondition 
     (and 
      (chosen.pcaf) 
     )
   :effect 
     (and 
      (available.pcaf) 
     )
  )
  (:action initialize.prb-e2f4p1-dp12
   :precondition 
     (and 
      (chosen.prb-e2f4p1-dp12) 
     )
   :effect 
     (and 
      (available.prb-e2f4p1-dp12) 
     )
  )
  (:action initialize.prbp2
   :precondition 
     (and 
      (chosen.prbp2) 
     )
   :effect 
     (and 
      (available.prbp2) 
     )
  )
  (:action initialize.raf1
   :precondition 
     (and 
      (chosen.raf1) 
     )
   :effect 
     (and 
      (available.raf1) 
     )
  )
  (:action initialize.sp1
   :precondition 
     (and 
      (chosen.sp1) 
     )
   :effect 
     (and 
      (available.sp1) 
     )
  )
  (:action choose.ap2.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.ap2)) 
     )
   :effect 
     (and 
      (chosen.ap2) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.cdc25c.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.cdc25c)) 
     )
   :effect 
     (and 
      (chosen.cdc25c) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.cdk46p3-cycd.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.cdk46p3-cycd)) 
     )
   :effect 
     (and 
      (chosen.cdk46p3-cycd) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.cdk46p3-cycdp1.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.cdk46p3-cycdp1)) 
     )
   :effect 
     (and 
      (chosen.cdk46p3-cycdp1) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.cdk7.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.cdk7)) 
     )
   :effect 
     (and 
      (chosen.cdk7) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.chk1.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.chk1)) 
     )
   :effect 
     (and 
      (chosen.chk1) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.dmp1.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.dmp1)) 
     )
   :effect 
     (and 
      (chosen.dmp1) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.e2f13.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.e2f13)) 
     )
   :effect 
     (and 
      (chosen.e2f13) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.p130-e2f5p1-dp12.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.p130-e2f5p1-dp12)) 
     )
   :effect 
     (and 
      (chosen.p130-e2f5p1-dp12) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.p16.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.p16)) 
     )
   :effect 
     (and 
      (chosen.p16) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.p300.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.p300)) 
     )
   :effect 
     (and 
      (chosen.p300) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.pcaf.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.pcaf)) 
     )
   :effect 
     (and 
      (chosen.pcaf) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.prb-e2f4p1-dp12.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.prb-e2f4p1-dp12)) 
     )
   :effect 
     (and 
      (chosen.prb-e2f4p1-dp12) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.prbp2.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.prbp2)) 
     )
   :effect 
     (and 
      (chosen.prbp2) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.raf1.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.raf1)) 
     )
   :effect 
     (and 
      (chosen.raf1) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.sp1.l1.l0
   :precondition 
     (and 
      (num-subs.l0) 
       (not (chosen.sp1)) 
     )
   :effect 
     (and 
      (chosen.sp1) 
      (num-subs.l1) 
      (not (num-subs.l0)) 
     )
  )
  (:action choose.ap2.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.ap2)) 
     )
   :effect 
     (and 
      (chosen.ap2) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.cdc25c.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.cdc25c)) 
     )
   :effect 
     (and 
      (chosen.cdc25c) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.cdk46p3-cycd.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.cdk46p3-cycd)) 
     )
   :effect 
     (and 
      (chosen.cdk46p3-cycd) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.cdk46p3-cycdp1.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.cdk46p3-cycdp1)) 
     )
   :effect 
     (and 
      (chosen.cdk46p3-cycdp1) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.cdk7.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.cdk7)) 
     )
   :effect 
     (and 
      (chosen.cdk7) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.chk1.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.chk1)) 
     )
   :effect 
     (and 
      (chosen.chk1) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.dmp1.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.dmp1)) 
     )
   :effect 
     (and 
      (chosen.dmp1) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.e2f13.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.e2f13)) 
     )
   :effect 
     (and 
      (chosen.e2f13) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.p130-e2f5p1-dp12.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.p130-e2f5p1-dp12)) 
     )
   :effect 
     (and 
      (chosen.p130-e2f5p1-dp12) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.p16.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.p16)) 
     )
   :effect 
     (and 
      (chosen.p16) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.p300.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.p300)) 
     )
   :effect 
     (and 
      (chosen.p300) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.pcaf.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.pcaf)) 
     )
   :effect 
     (and 
      (chosen.pcaf) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.prb-e2f4p1-dp12.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.prb-e2f4p1-dp12)) 
     )
   :effect 
     (and 
      (chosen.prb-e2f4p1-dp12) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.prbp2.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.prbp2)) 
     )
   :effect 
     (and 
      (chosen.prbp2) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.raf1.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.raf1)) 
     )
   :effect 
     (and 
      (chosen.raf1) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.sp1.l2.l1
   :precondition 
     (and 
      (num-subs.l1) 
       (not (chosen.sp1)) 
     )
   :effect 
     (and 
      (chosen.sp1) 
      (num-subs.l2) 
      (not (num-subs.l1)) 
     )
  )
  (:action choose.ap2.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.ap2)) 
     )
   :effect 
     (and 
      (chosen.ap2) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.cdc25c.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.cdc25c)) 
     )
   :effect 
     (and 
      (chosen.cdc25c) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.cdk46p3-cycd.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.cdk46p3-cycd)) 
     )
   :effect 
     (and 
      (chosen.cdk46p3-cycd) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.cdk46p3-cycdp1.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.cdk46p3-cycdp1)) 
     )
   :effect 
     (and 
      (chosen.cdk46p3-cycdp1) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.cdk7.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.cdk7)) 
     )
   :effect 
     (and 
      (chosen.cdk7) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.chk1.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.chk1)) 
     )
   :effect 
     (and 
      (chosen.chk1) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.dmp1.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.dmp1)) 
     )
   :effect 
     (and 
      (chosen.dmp1) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.e2f13.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.e2f13)) 
     )
   :effect 
     (and 
      (chosen.e2f13) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.p130-e2f5p1-dp12.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.p130-e2f5p1-dp12)) 
     )
   :effect 
     (and 
      (chosen.p130-e2f5p1-dp12) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.p16.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.p16)) 
     )
   :effect 
     (and 
      (chosen.p16) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.p300.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.p300)) 
     )
   :effect 
     (and 
      (chosen.p300) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.pcaf.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.pcaf)) 
     )
   :effect 
     (and 
      (chosen.pcaf) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.prb-e2f4p1-dp12.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.prb-e2f4p1-dp12)) 
     )
   :effect 
     (and 
      (chosen.prb-e2f4p1-dp12) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.prbp2.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.prbp2)) 
     )
   :effect 
     (and 
      (chosen.prbp2) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.raf1.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.raf1)) 
     )
   :effect 
     (and 
      (chosen.raf1) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action choose.sp1.l3.l2
   :precondition 
     (and 
      (num-subs.l2) 
       (not (chosen.sp1)) 
     )
   :effect 
     (and 
      (chosen.sp1) 
      (num-subs.l3) 
      (not (num-subs.l2)) 
     )
  )
  (:action associate-with-catalyze.cdc25c.chk1.cdc25cp2
   :precondition 
     (and 
      (available.cdc25c) 
      (available.chk1) 
     )
   :effect 
     (and 
      (available.cdc25cp2) 
      (not (available.cdc25c)) 
     )
  )
  (:action associate-with-catalyze.dmp1.cdk46p3-cycd.dmp1p1
   :precondition 
     (and 
      (available.dmp1) 
      (available.cdk46p3-cycd) 
     )
   :effect 
     (and 
      (available.dmp1p1) 
      (not (available.dmp1)) 
     )
  )
  (:action associate-with-catalyze.dmp1.cdk46p3-cycdp1.dmp1p1
   :precondition 
     (and 
      (available.dmp1) 
      (available.cdk46p3-cycdp1) 
     )
   :effect 
     (and 
      (available.dmp1p1) 
      (not (available.dmp1)) 
     )
  )
  (:action associate-with-catalyze.prbp2.cdk46p3-cycd.prbp1p2
   :precondition 
     (and 
      (available.prbp2) 
      (available.cdk46p3-cycd) 
     )
   :effect 
     (and 
      (available.prbp1p2) 
      (not (available.prbp2)) 
     )
  )
  (:action associate-with-catalyze.prbp2.cdk46p3-cycdp1.prbp1p2
   :precondition 
     (and 
      (available.prbp2) 
      (available.cdk46p3-cycdp1) 
     )
   :effect 
     (and 
      (available.prbp1p2) 
      (not (available.prbp2)) 
     )
  )
  (:action associate.p16.cdk7.p16-cdk7
   :precondition 
     (and 
      (available.p16) 
      (available.cdk7) 
     )
   :effect 
     (and 
      (available.p16-cdk7) 
      (not (available.cdk7)) 
      (not (available.p16)) 
     )
  )
  (:action associate.pcaf.p300.pcaf-p300
   :precondition 
     (and 
      (available.pcaf) 
      (available.p300) 
     )
   :effect 
     (and 
      (available.pcaf-p300) 
      (not (available.p300)) 
      (not (available.pcaf)) 
     )
  )
  (:action associate.prbp1p2.ap2.prbp1p2-ap2
   :precondition 
     (and 
      (available.prbp1p2) 
      (available.ap2) 
     )
   :effect 
     (and 
      (available.prbp1p2-ap2) 
      (not (available.ap2)) 
      (not (available.prbp1p2)) 
     )
  )
  (:action associate.prbp2.ap2.prbp2-ap2
   :precondition 
     (and 
      (available.prbp2) 
      (available.ap2) 
     )
   :effect 
     (and 
      (available.prbp2-ap2) 
      (not (available.ap2)) 
      (not (available.prbp2)) 
     )
  )
  (:action associate.raf1.p130-e2f5p1-dp12.raf1-p130-e2f5p1-dp12
   :precondition 
     (and 
      (available.raf1) 
      (available.p130-e2f5p1-dp12) 
     )
   :effect 
     (and 
      (available.raf1-p130-e2f5p1-dp12) 
      (not (available.p130-e2f5p1-dp12)) 
      (not (available.raf1)) 
     )
  )
  (:action associate.raf1.prb-e2f4p1-dp12.raf1-prb-e2f4p1-dp12
   :precondition 
     (and 
      (available.raf1) 
      (available.prb-e2f4p1-dp12) 
     )
   :effect 
     (and 
      (available.raf1-prb-e2f4p1-dp12) 
      (not (available.prb-e2f4p1-dp12)) 
      (not (available.raf1)) 
     )
  )
  (:action associate.sp1.e2f13.sp1-e2f13
   :precondition 
     (and 
      (available.sp1) 
      (available.e2f13) 
     )
   :effect 
     (and 
      (available.sp1-e2f13) 
      (not (available.e2f13)) 
      (not (available.sp1)) 
     )
  )
)
