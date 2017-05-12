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

name = "btuc"
if len(sys.argv) <> 2:
    print """
    usage: %s <npos>
    generates files %s-<npos>/d.pddl and p.pddl

    for the bomb toilet (version of MBP)
    """ % (sys.argv[0],name)
    sys.exit(1)
npos = int(sys.argv[1])+1

path=name+"-" +str(npos-1)
os.system("/bin/rm -rf ./"+path)
os.makedirs(path)
problem = file(path + "/p.pddl", "w")
domain  = file(path + "/d.pddl", "w")

print >> domain, """
(define (domain %s)
  (:requirements :typing)
  (:types p)
    
  (:predicates
    (pos ?x - p)
    (defused)
    (nclogged)
  )

  (:action dunk
   :parameters  (?x - p)
   :precondition (nclogged)
   :effect
    (and
       (oneof (not (nclogged)) (nclogged))
       (when (pos ?x) (defused))
    )
  )

  (:action flush
   :effect 
   (nclogged))
  )
""" % name


print >> problem, """
(define (problem %s-%s)
   (:domain %s)
""" % (name,npos-1,name)

print >> problem, """   
   (:objects """,
for z in range(1, npos):
    print >> problem, "p%d " % z,
print >> problem, """ - p)"""

print >> problem, """   
   (:init (and 
     (oneof (not (nclogged)) (nclogged))
     (oneof""",

for z in range(1, npos):
    print >> problem, " (pos p%s)" % z,
print >> problem, """
)))"""

print >> problem, """    (:goal (defused))
)
"""
