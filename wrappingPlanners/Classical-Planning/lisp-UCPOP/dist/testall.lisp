" (c) 1990-1995 Copyright (c) University of Washington
  Written by Marc Friedman.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

;;; Load this file after loading the interface and the domains.
;;; Execute:
;;; 
;;;           (run-tests *all-tests*)
;;; 
;;;                        and compare with the output below.
;;; It's a nice test for checking if you degraded/improved UCPOP's
;;; searching (as opposed to plan-construction).  To see the plans,
;;; use bf-control rather than bf-test.
;;; 
;;; There are no guarantees that this file is up-to-date with the
;;; domains file.

(in-package "UCPOP")

(setq *all-tests*
    (list
     'sussman-anomaly
     'tower-invert3
     'tower-invert4
     (random-bw-problem 5)
     (road-test)
     (hanoi 3)
     'test-ferry
     'rat-insulin
     'r-test1
     'r-test2
     'monkey-test1
     'monkey-test2
     'monkey-test3
     'get-paid
     'get-paid2
     'get-paid3
     'get-paid4
     'fixit
     'fix1
     'fix2
     'fix3
     'fix4
     'fix5
     'ho-demo
     'Fixa
     'fixb
     'mcd-sussman-anomaly
     'mcd-tower-invert
     'mcd-sussman
     'mcd-tower
     'uget-paid
     'uget-paid2
     'uget-paid3
     'uget-paid4
     (sched-test1a)
     (sched-test2a)
     'prodigy-sussman
     'prodigy-p22
     'move-boxes
     'move-boxes-1
     'all-home
     'all3-home
     'office1
     'office2
     'office3
     'office4
     'office5
     'fixit2))

(defun run-tests (test-list)
  (dolist (tt test-list)
    (bf-test tt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; UCPOP(12): (run-tests *all-tests*)
;;; 
;;;      SUSSMAN-ANOMALY     38     23     +0.06 s +1.47826 bf.   3 steps. Complete!
;;;        TOWER-INVERT3     59     35     +0.09 s +1.54286 bf.   4 steps. Complete!
;;;        TOWER-INVERT4    330    213     +0.72 s  +1.3615 bf.   6 steps. Complete!
;;; Random 5 block blocksworld problem     58     30     +0.09 s     +1.7 bf.   4 steps. Complete!
;;;            ROAD-TEST     40     25     +0.05 s    +1.56 bf.   4 steps. Complete!
;;;                HANOI   2259   1588     +3.24 s +1.26008 bf.   4 steps. Hit limit.
;;;           TEST-FERRY    120     83     +0.13 s +1.19277 bf.   7 steps. Complete!
;;;          RAT-INSULIN   1447    981     +2.32 s  +1.2946 bf.  10 steps. Complete!
;;;              R-TEST1     26     17     +0.03 s +1.47059 bf.   3 steps. Complete!
;;;              R-TEST2    483    328      +0.9 s +1.21341 bf.   7 steps. Complete!
;;;         MONKEY-TEST1    165    105     +0.25 s  +1.4381 bf.   6 steps. Complete!
;;;         MONKEY-TEST2    765    483     +1.47 s +1.32712 bf.  10 steps. Complete!
;;;         MONKEY-TEST3   2501   1560     +5.63 s +1.28269 bf.   8 steps. Hit limit.
;;;             GET-PAID     20     12     +0.03 s +1.58333 bf.   3 steps. Complete!
;;;            GET-PAID2     76     48      +0.1 s +1.41667 bf.   5 steps. Complete!
;;;            GET-PAID3    326    203     +0.63 s  +1.3399 bf.   6 steps. Complete!
;;;            GET-PAID4    154     96     +0.21 s   +1.375 bf.   6 steps. Complete!
;;;                FIXIT   2232   1208     +4.19 s +1.65728 bf.   6 steps. Hit limit.
;;;                 FIX1     48     25     +0.06 s    +1.88 bf.   5 steps. Complete!
;;;                 FIX2     36     20     +0.04 s     +1.7 bf.   3 steps. Complete!
;;;                 FIX3   1076    589     +1.66 s +1.59762 bf.   6 steps. Complete!
;;;                 FIX4     40     19     +0.04 s +2.05263 bf.   4 steps. Complete!
;;;                 FIX5     19     10     +0.02 s     +1.8 bf.   1 steps. Complete!
;;;              HO-DEMO     71     43     +0.05 s  +1.2093 bf.   4 steps. Complete!
;;;                 FIXA    545    443     +0.86 s +1.22799 bf.   7 steps. Complete!
;;;                 FIXB   2195   1643      +4.2 s  +1.2185 bf.   9 steps. Hit limit.
;;;  MCD-SUSSMAN-ANOMALY    121     73     +0.14 s +1.30137 bf.   3 steps. Complete!
;;;     MCD-TOWER-INVERT   2710   1646     +5.27 s +1.21628 bf.   6 steps. Hit limit.
;;;          MCD-SUSSMAN     62     40     +0.06 s   +1.175 bf.   3 steps. Complete!
;;;            MCD-TOWER    133     87     +0.12 s +1.16092 bf.   4 steps. Complete!
;;;            UGET-PAID     26     13     +0.03 s +1.53846 bf.   3 steps. Complete!
;;;           UGET-PAID2    115     65     +0.11 s +1.35385 bf.   5 steps. Complete!
;;;           UGET-PAID3   1585    837     +2.18 s  +1.2497 bf.   6 steps. Complete!
;;;           UGET-PAID4    185    104     +0.21 s +1.34615 bf.   6 steps. Complete!
;;;                  NIL      6      4     +0.08 s    +1.25 bf.   2 steps. Complete!
;;;                  NIL      8      3     +0.01 s +2.33333 bf.   1 steps. Complete!
;;;      PRODIGY-SUSSMAN   1188    655     +1.51 s +1.45191 bf.   6 steps. Complete!
;;;          PRODIGY-P22   2400   1196     +3.61 s +1.67308 bf.   7 steps. Hit limit.
;;;           MOVE-BOXES   2192    742     +5.29 s +2.70081 bf.   6 steps. Hit limit.
;;;         MOVE-BOXES-1   2192    742     +5.63 s +2.70081 bf.   6 steps. Hit limit.
;;;             ALL-HOME     69     41     +0.09 s +1.65854 bf.   3 steps. Complete!
;;;            ALL3-HOME     67     43     +0.07 s +1.53488 bf.   3 steps. Complete!
;;;              OFFICE1     34     31     +0.03 s +1.06452 bf.   2 steps. Complete!
;;;              OFFICE2     54     33     +0.06 s +1.60606 bf.   2 steps. Complete!
;;;              OFFICE3     52     48     +0.05 s  +1.0625 bf.   2 steps. Complete!
;;;              OFFICE4    256    179     +0.33 s +1.40223 bf.   4 steps. Complete!
;;;              OFFICE5    555    387     +0.81 s +1.40052 bf.   6 steps. Complete!
;;;               FIXIT2   1180    656      +2.6 s +1.62805 bf.  19 steps. Complete!
;;; NIL
;;; UCPOP(13): 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

