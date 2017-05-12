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

# Right generation of conformant blocks problems
# as reported by Blai Bonet.

# The resulting number of initial states for N blocks
# corresponds to N*f(N-1)*f(N)
# where f(n) = g(n,0) in Blocks World revisited
# by John Slaney and Silvie Thiebaux. AIJ 2001
# (Table 1, page 124)

import sys

def usage():
    print """
usage: %s <N> p.pddl
generates in p.pddl a "problem" pddl
which initial situation describe all the possible
configurations of <N> blocks.

WARNING: this is a RIGHT generation of all possible combinations
of blocks. Benchmarks at IPC5 and IPC6 had a bug,
allowing cycles of blocks "on".
The number of possible initial states of this
generator coincides with the reported 
in "Blocks World Revisited" by Slaney and Thiebaux ij AIJ 2001.
(Plus the fact the codifination use an arm)
""" % sys.argv[0]
    sys.exit(0)

if len(sys.argv) <> 2:
    usage()
try:
    nb=int(sys.argv[1])
except:
    usage()

print """
(define (problem b%d)
  (:domain blocks)
""" % nb,

name = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
blocks=[]
for i in range(0,nb):
    blocks.append(name[i])

print """
  (:objects """,

for b in blocks:
    print b,

print """ - block)
  (:init
    (and """,

print """
        (oneof (handempty)""", 
for b in blocks:
    print """(holding %s)""" % b,
print """) ; (holding ?x)""",

# ONEOF
for b in blocks:
    print """
        (oneof (holding %s) (clear %s)""" % (b,b), 
    for b2 in blocks:
        if b2 <> b: 
            print """(on %s %s)""" % (b2,b),
    print """) ; (above %s ?x)""" % b,

    print """
        (oneof (holding %s) (ontable %s)""" % (b,b), 
    for b2 in blocks:
        if b2 <> b: 
            print """(on %s %s)""" % (b,b2),
    print """) ; (on %s ?x)""" % b,

print """

        ; cycles"""
#CYCLES
# Actually, this should be a particular case of the following.
for i in range(0,len(blocks)):
    b=blocks[i]
    for j in range(i+1,len(blocks)):
        b2=blocks[j]
        print """
        (or (not (on %s %s)) (not (on %s %s)))""" % (b,b2,b2,b), 

# no cycle towers
# Taken from ....
def all_perms(str):
    if len(str) <=1:
        yield str
    else:
        for perm in all_perms(str[1:]):
            for i in range(len(perm)+1):
                yield perm[:i] + str[0:1] + perm[i:]
print
print
for p in all_perms(blocks):
    # Take prefix of the permitation
    for i in range(3,len(blocks)+1):
        print """
        (or""",
        for j in range(0,i-1):
            print """(not (on %s %s))""" % (p[j],p[j+1]),
        print """(not (on %s %s))""" % (p[i-1],p[0]),
        print """)""",


print """
    )
  )
  (:goal (and """,

print """
           (ontable A) """,
for i in range(1,len(blocks)):
    print """
           (on %s %s)""" % (blocks[i],blocks[i-1]),
    

print """
         ))
)
"""
