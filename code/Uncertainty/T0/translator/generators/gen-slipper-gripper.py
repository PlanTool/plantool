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

name = "gripper"

def usage():
    print """
    usage: %s <nballs> {cycle}
    generates files %s-<npos>{-cycle}/d.pddl and p.pddl

    for slippery-gripper as defined in AIJ 08
    """ % (sys.argv[0],name)
    sys.exit(1)

if len(sys.argv) > 3 or len(sys.argv) < 2:
    usage()

nballs = int(sys.argv[1])+1
cycle=False
det_pick_drop=False
if len(sys.argv) == 3:
    if sys.argv[2] == 'cycle':
        cycle=True
    else:
        usage()

if cycle:
    path=name+"-"+str(nballs-1)+"-cycle"
else:
    path=name+"-"+str(nballs-1)
os.system("/bin/rm -rf ./"+path)
os.makedirs(path)
problem = file(path + "/p.pddl", "w")
domain  = file(path + "/d.pddl", "w")

print >> domain, """
(define (domain %s)
   (:requirements :typing)
   (:types room ball gripper)
   (:constants left right - gripper)
   (:predicates (at-robby ?r - room)
                (det ?r1 ?r2 - room)
                (non-det ?from ?r1 ?r2 - room)
                (at ?b - ball ?r - room)
                (free ?g - gripper)
                (carry ?o - ball ?g - gripper))

   (:action move-d
       :parameters  (?from ?to - room)
       :precondition (det ?from ?to) 
       :effect (when (at-robby ?from) (and (at-robby ?to)
                     (not (at-robby ?from)))))

   (:action move-nd
       :parameters  (?from ?to1 ?to2 - room)
       :precondition (non-det ?from ?to1 ?to2) 
       :effect (when (at-robby ?from) 
                     (and (oneof (at-robby ?to1) (at-robby ?to2))
                          (not (at-robby ?from)))))

""" % name


if det_pick_drop:
    print >> domain, """
   (:action pick
       :parameters (?obj - ball ?room - room ?gripper - gripper)
       :precondition (and (at ?obj ?room) (at-robby ?room) (free ?gripper))
       :effect (and (carry ?obj ?gripper)
                    (not (at ?obj ?room)) 
                    (not (free ?gripper))))

   (:action drop
       :parameters  (?obj - ball ?room - room ?gripper - gripper)
       :precondition (and (carry ?obj ?gripper) (at-robby ?room))
       :effect (and (at ?obj ?room)
                    (free ?gripper)
                    (not (carry ?obj ?gripper)))))
    """
else:
    print >> domain, """
   (:action pick
       :parameters (?obj - ball ?room - room ?gripper - gripper)
       :effect (when (and  (at ?obj ?room) (at-robby ?room) (free ?gripper))
               (and (carry ?obj ?gripper)
                    (not (at ?obj ?room)) 
                    (not (free ?gripper)))))

   (:action drop
       :parameters  (?obj - ball ?room - room ?gripper - gripper)
       :effect (when  (and  (carry ?obj ?gripper) (at-robby ?room))
               (and (at ?obj ?room)
                    (free ?gripper)
                    (not (carry ?obj ?gripper))))))
    """


print >> problem, """
(define (problem %s)
   (:domain %s)""" % (path,name)

print >> problem, """   
   (:objects rooma roomb1 roomb2 roomc - room
             """,
for z in range(1, nballs):
    print >> problem, "ball%d " % z,
print >> problem, """ - ball)""",

print >> problem, """   
    (:init (and 
          (at-robby rooma)
          (free left)
          (free right)
          (det roomb1 roomc)
          (det roomc roomb1)

          (det roomb2 roomc)
          (det roomc roomb2)

          (non-det rooma roomb1 roomb2)
""",

if cycle:
    print >> problem, """
          (det roomc rooma)
""",
else:
    print >> problem, """
          (det roomb1 rooma)
          (det roomb2 rooma)
""",
    
for z in range(1, nballs):
    print >> problem, """
          (at ball%s rooma)""" % z,

print >> problem, """
    ))""",

print >> problem, """
    (:goal (and""",
for z in range(1, nballs):
    print >> problem, """
         (at ball%s roomc)""" % z,

print >> problem, """
    ))
)"""
