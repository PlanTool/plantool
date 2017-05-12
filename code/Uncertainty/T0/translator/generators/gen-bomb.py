#! /usr/bin/env python
# -*- coding: latin-1 -*-

#  Copyright (C) 2006 Universitat Pompeu Fabra
#
#  Permission is hereby granted to distribute this software for
#  non-commercial research purposes, provided that this copyright
#  notice is included with any such distribution.
#
#  THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
#  EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
#  SOFTWARE IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU
#  ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
#
# Elaborated by Hector Palacios, hlp@ldc.usb.ve, hectorpal@gmail.com
# Joint work with Hector Geffner.

import sys
import os

name = "bomb"
if len(sys.argv) <> 3:
    print """
    usage: %s <nbomb> <ntoilet>
    generates files <nbomb>-<ntoilet>/d.pddl and p.pddl

    for the bomb toilet 
    """ % sys.argv[0]
    sys.exit(1)
nbomb = int(sys.argv[1])+1
ntoilet = int(sys.argv[2])+1

path="b" +str(nbomb-1)+"-"+"t"+str(ntoilet-1)
os.system("/bin/rm -rf ./"+path)
os.makedirs(path)
problem = file(path + "/p.pddl", "w")
problem2 = file(path + "/p2.pddl", "w")
domain  = file(path + "/d.pddl", "w")
domain2  = file(path + "/d2.pddl", "w")

print >> domain, """
(define (domain %s)
  (:requirements :typing)
    (:types bomb toilet)
    
(:predicates 
  (narmed ?x)
  (nclogged ?x)
)

(:action dunk
:parameters  (?bomb - bomb ?toilet - toilet)
:precondition (nclogged ?toilet)
:effect
(and
(when (not (narmed ?bomb)) (narmed ?bomb))
(not (nclogged ?toilet))))

(:action flush
:parameters  (?toilet - toilet)
:effect (when (not (nclogged ?toilet)) (nclogged ?toilet)))

)
""" % name

print >> domain2, """
(define (domain %s)
(:predicates (bomb ?x)
(toilet ?x)
(armed ?x)
(clogged ?x))

(:action dunk
:parameters  (?bomb ?toilet)
:precondition (and (bomb ?bomb) (toilet ?toilet)
(not (clogged ?toilet)))
:effect (and (when (armed ?bomb) (not (armed ?bomb)))
(clogged ?toilet)))

(:action flush
:parameters  (?toilet)
:precondition (toilet ?toilet)
:effect (when (clogged ?toilet) (not (clogged ?toilet))))

)
""" % name

print >> problem, "(define (problem bomb-%d-%d)\n(:domain %s)" % (nbomb-1,ntoilet-1,name)
print >> problem2, "(define (problem bomb-%d-%d)\n(:domain %s)" % (nbomb-1,ntoilet-1,name)
print >> problem, "   (:objects ",
print >> problem2, "   (:objects ",
for z in range(1, nbomb):
    print >> problem, "bomb%d " % z,
    print >> problem2, "bomb%d " % z,
print >> problem, " - bomb "
for z in range(1, ntoilet):
    print >> problem, "toilet%d " % z,
    print >> problem2, "toilet%d " % z,
print >> problem, " - toilet)"
print >> problem2, ")"

print >> problem, "   (:init (and"
print >> problem2, "   (:init "

for z in range(1, nbomb):
    print >> problem2, "(bomb bomb%d)" % z,
for z in range(1, ntoilet):
    print >> problem2, "(toilet toilet%d)" % z,

for z in range(1, nbomb):
    print >> problem, "      (oneof (narmed bomb%d) (not (narmed bomb%d)))" % (z,z)
    print >> problem2, "      (unknown (armed bomb%d))" % z
for z in range(1, ntoilet):
    print >> problem, "      (nclogged toilet%d)" % z,
print >> problem, "      ))"
print >> problem2, "      )"

print >> problem, "    (:goal (and"
print >> problem2, "    (:goal (and"
for z in range(1, nbomb):
    print >> problem, "      (narmed bomb%d)" % z,
    print >> problem2, "      (not (armed bomb%d))" % z,
print >> problem, "    )))"
print >> problem2, "    )))"



