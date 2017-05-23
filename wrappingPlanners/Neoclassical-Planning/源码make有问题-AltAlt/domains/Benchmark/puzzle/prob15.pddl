
(define (problem 15puzzle)
    (:domain longs-puzzle)
      (:objects
      1
      2
      3
      4
      5
      6
      7
      8
      9
      10
      11
      12
      13
      14
      15
      16
    	A
    	B
    	C
    	D
    	E
    	F
    	G
    	H
    	I
    	J
    	K
    	L
    	M
    	N
    	O)
    (:init
           (square 1)
           (square 2)
           (square 3)
           (square 4)
           (square 5)
           (square 6)
           (square 7)
           (square 8)
           (square 9)
           (square 10)
           (square 11)
           (square 12)
           (square 13)
           (square 14)
           (square 15)
           (square 16)
           (conn 1 2)
           (conn 1 5)
           (conn 2 1)
           (conn 2 6)
           (conn 2 3)
           (conn 3 2)
           (conn 3 7)
           (conn 3 4)
           (conn 5 1)
           (conn 5 6)
           (conn 5 9)
           (conn 6 2)
           (conn 6 5)
           (conn 6 7)
           (conn 6 10)
           (conn 7 3)
           (conn 7 6)
           (conn 7 8)
           (conn 7 11)
           (conn 8 7)
           (conn 8 4)
           (conn 8 12)
           (conn 9 5)
           (conn 9 10)
           (conn 9 13)
           (conn 10 6)
           (conn 10 9)
           (conn 10 11)
           (conn 10 14)
           (conn 11 10)
           (conn 11 7)
           (conn 11 12)
           (conn 11 15)
           (conn 12 11)
           (conn 12 8)
           (conn 12 16)
           (conn 13 9)
           (conn 13 14)
           (conn 14 10)
           (conn 14 13)
           (conn 14 15)
           (conn 15 14)
           (conn 15 11)
           (conn 15 16)
           (conn 16 15)
           (conn 16 12)
           (tile A)
           (tile B)
           (tile C)
           (tile D)
           (tile E)
           (tile F)
           (tile G)
           (tile H)
           (tile I)
           (tile J)
           (tile K)
           (tile L)
           (tile M)
           (tile N)
           (tile O)
           (at A 1)
           (at B 2)
           (at C 3)
           (at D 4)
           (at E 5)
           (at F 6)
           (at G 7)
           (at H 8)
           (at I 9)
           (at J 10)
           (at K 11)
           (at L 12)
           (at M 13)
           (at N 15)
           (at O 16)
           (at-blank 14))
    (:goal (and
           (at A 1)
           (at B 2)
           (at C 3)
           (at D 4)
           (at E 5)
           (at F 6)
           (at G 7)
           (at H 8)
           (at I 9)
           (at J 10)
           (at K 11)
           (at L 12)
           (at M 13)
           (at N 14)
           (at O 15)
           (at-blank 16))))


