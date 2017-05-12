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

name = "ring"

def usage():
    print """
    usage: %s <n> {cff}
    generates files %s-<n>/d.pddl and %s-<n>/p.pddl

    for the ring problem with <n> rooms
    as described in Palacios & Geffner AAAI-06, ICAPS-07

    cff -> cff version (with unknown keyword)
    """ % (sys.argv[0], name, name)
    sys.exit(1)

if len(sys.argv) < 2 or len(sys.argv) > 3:
    usage()

cff=False
if len(sys.argv) == 3:
    if sys.argv[2] == 'cff':
        cff=True
    else:
        usage()
    
n = int(sys.argv[1])
name_p="%s-%d" % (name, n)
if cff:
    name_p += '-cff'
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

print >> domain, """
(define (domain d-%s)
  (:requirements :typing :equality)
  (:types window)
  (:constants """ % name_p,

for i in range(1,limit):
    print >> domain, ("w%s " % i),

print >> domain, """- window)
  (:predicates
     (pos ?w - window)
     (closed ?w - window)
     (open ?w - window)
     (locked ?w - window)
  )
  (:action fwd
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (pos w%s) (and (pos w%s) (not (pos w%s)))) """ %(i,next(i),i),

print >> domain, """
       )
  )
  (:action bwd
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (pos w%s) (and (pos w%s) (not (pos w%s)))) """ %(i,prev(i),i),

print >> domain, """
       )
  )
  (:action close
     :effect
       (and """,

for i in range(1,limit):
    print >> domain, """
            (when (pos w%s) (closed w%s)) """ %(i,i),

print >> domain, """
       )
  )
  (:action lock
     :effect
       (and""",

for i in range(1,limit):
    print >> domain, """
            (when (and (pos w%s) (closed w%s)) (locked w%s)) """ %(i,i,i),

print >> domain, """
       )
  )
)

"""

print >> problem, """
(define (problem %s)
  (:domain d-%s)
  (:init """ % (name_p,name_p),

if not cff:
    print >> problem, """
     (and 
"""

for i in range(1,limit):
    if cff:
        print >> problem, """
        (unknown (open w%s))
        (unknown (closed w%s))
        (unknown (locked w%s))""" % (i,i,i)
    print >> problem, """
        (oneof (open w%s) (closed w%s) (locked w%s)) """ % (i,i,i),
    
if cff:
    for i in range(1,limit):
        print >> problem, """
        (unknown (pos w%s))""" % i
    
print >> problem, """
        (oneof""",

for i in range(1,limit):
    print >> problem, " (pos w%s)" % i,

print >> problem, """)"""

if not cff:
    print >> problem, """
     )
"""

print >> problem, """
  )
""",

print >> problem, """
  (:goal (and """,

for i in range(1,limit):
    print >> problem, """
              (locked w%s)""" % i,

print >> problem, """
         )
  )
)

"""
