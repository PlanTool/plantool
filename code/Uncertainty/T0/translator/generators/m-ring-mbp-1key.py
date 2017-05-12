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

name = "ring-1key"
init_certainty = False
deterministic = False

def usage():
    print """
    usage: %s <n> {det|nondet} {ui}
    generates files <x>-%s-<n>/d.pddl and <x>-%s-<n>/p.pddl
    (where is the 2nd parameter: det or nondet)

    for the deterministic (d) or non-determistic (nd) ring 
    problem with <n> rooms
    (as used by MBP and KACMBP conformant planner)
    ui => total uncertainty at initial state
          otherwise windows are open and locked
    """ % (sys.argv[0], name, name)
    sys.exit(1)

if len(sys.argv) < 3 or len(sys.argv) > 4:
    usage()

if sys.argv[2] == 'det':
    deterministic = True
    name = "det-"+name
elif sys.argv[2] == 'nondet':
    deterministic = False
    name = "nondet-"+name
else:
    print 'Error on 2nd parameter'
    usage()

if len(sys.argv) == 4:
    if sys.argv[3] == 'ui':
        init_certainty = True
    else:
        print 'Error on last parameter'
        usage()
    
n = int(sys.argv[1])
if init_certainty:
    name_p="%s-ui-%d" % (name, n)
else:
    name_p="%s-%d" % (name, n)

path=name_p

os.system("/bin/rm -rf ./"+path)
os.makedirs(path)

problem = file(path + "/p.pddl", "w")
domain  = file(path + "/d.pddl", "w")

limit = n+1

def next(i):
    if i < n:
        return i+1
    else:
        return next(i-n)

def prev(i):
    if i > 1:
        return i-1
    else:
        return prev(i+n)


# Axiom: windows can change by themself
def window_can_change():
    global deterministic
    if deterministic:
        return ""

    r=""
    for i in range(1,limit):
        r += """
            (when (and (not (locked w%s)) (not (pos w%s))) 
                  (oneof (closed w%s) (not (closed w%s))))
""" % (i,i,i,i)
    return r

print >> domain, """
(define (domain ring)
  (:requirements :typing :equality)
  (:types window)
  (:constants """,

for i in range(1,limit):
    print >> domain, ("w%s " % i),

print >> domain, """- window)
  (:predicates
     (key-at ?w - window)
     (has-key)
     (pos ?w - window)
     (closed ?w - window)
     (locked ?w - window)
  )
  (:action fwd
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (pos w%s) (and (pos w%s) (not (pos w%s)))) """ %(i,next(i),i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action bwd
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (pos w%s) (and (pos w%s) (not (pos w%s)))) """ %(i,prev(i),i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action open
     :effect
       (and """,

for i in range(1,limit):
    print >> domain, """
            (when (and (pos w%s) (not (locked w%s))) (not (closed w%s))) """ %(i,i,i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action close
     :effect
       (and """,

for i in range(1,limit):
    print >> domain, """
            (when (and (pos w%s) (not (locked w%s))) (closed w%s)) """ %(i,i,i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action lock
     :precondition (has-key)
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (and ;(has-key) 
                       (pos w%s)) (locked w%s)) """ %(i,i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action unlock
     :precondition (has-key)
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (and ;(has-key) 
                       (pos w%s)) (not (locked w%s))) """ %(i,i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action pick-up
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (and (pos w%s) (key-at w%s)) (has-key)) """ % (i,i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )
  (:action put-down
     :effect
       (and
            (when (has-key) (not (has-key))) """,
for i in range(1,limit):
    print >> domain, """
            (when (and (has-key) (pos w%s)) (key-at w%s)) """ %(i,i),

print >> domain, window_can_change(),

print >> domain, """
       )
  )"""

print >> domain, """
)

"""

print >> problem, """
(define (problem ring-p-%s)
  (:domain ring-d-%s)
  (:init
     (and """ % (n,n),

if init_certainty:
    for i in range(1,limit):
        print >> problem, """
            (or (not (locked w%s)) (locked w%s))
            (or (not (closed w%s)) (closed w%s)) """ % (i,i,i,i),
else:
    for i in range(1,limit):
        print >> problem, """
            (locked w%s)
            ;(not (closed w%s)) """ % (i,i),
    

print >> problem, """
        (oneof""",
for i in range(1,(limit+1)/2+1):
    print >> problem, " (pos w%s)" % i,
print >> problem, """)"""

print >> problem, """
        (oneof""",
for i in range(((limit+1)/2)+1,limit):
    print >> problem, " (key-at w%s)" % i,
print >> problem, """)"""

print >> problem, """
     )
  )
""",

print >> problem, """
  (:goal (and """,

for i in range(1,limit):
    print >> problem, """
              ;(locked w%s)
              (closed w%s)""" % (i,i),

print >> problem, """
         )
  )
)
"""
