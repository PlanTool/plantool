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


import sys
import os

name = "s"
if len(sys.argv) < 2 or len(sys.argv) > 3:
    print """
    usage: %s <n> {cff}
    generates files d.pddl and p.pddl at directory %s-<n>

    for the problem of sorting <n> numbers

    if {cff} is passwd, then PDDL compatible with ConformantFF is generated.
    """ % (sys.argv[0], name)
    sys.exit(1)
n = int(sys.argv[1])
cff=False
if len(sys.argv) == 3:
    if sys.argv[2] == 'cff':
        cff = True
    else:
        print 'Param should be empty or cff:', sys.argv[2]
        sys.exit(1)

path=name+"-"+str(n)
name_p="%s-%d" % (name, n)
if cff:
    path += "-cff"
    name_p += "-cff"
    
os.system("/bin/rm -rf ./"+path)
os.makedirs(path)

domain  = file(path + "/d.pddl", "w")

cs = ''
for i in range(1,n+1):
    cs += ' n%d' % i

if cff:
    t = ''
else:
    t = ' - num'

print >> domain, """
(define (domain %s) """ % name + """
    (:requirements :typing :equality)
    (:types num)
    (:constants %s%s)
    (:predicates (foo) (less ?n1 ?n2%s))
""" % (cs,t,t)

def print_act(i,j):
    print >> domain, """        
    (:action cmpswap-%d-%d
     :effect (and (less n%d n%d) (not (less n%d n%d))""" % (i, j, i, j, j, i),
    for k in range(1,n+1):
        if k != i and k != j:
            print >> domain, """        
                  (when (less n%d n%d)
                        (and (less n%d n%d) (not (less n%d n%d))))""" % (
                k, i, k, j, j, k),
            print >> domain, """        
                  (when (and (less n%d n%d) (not (less n%d n%d)))
                        (not (less n%d n%d)))""" % (
                k, i, k, j, k, i),
            print >> domain, """        
                  (when (less n%d n%d)
                        (and (less n%d n%d) (not (less n%d n%d))))""" % (
                j, k, i, k, k, i),
            print >> domain, """        
                  (when (and (less n%d n%d) (not (less n%d n%d)))
                        (not (less n%d n%d)))""" % (
                j, k, i, k, j, k),
    print >> domain, """
    ))""" 
    
for i in range(1,n+1):
    for j in range(i+1,n+1):
        if i != j:
            print_act(i,j)

print >> domain, """
)
"""

problem = file(path + "/p.pddl", "w")

if cff:
    t = ''
else:
    t = '(and '

print >> problem, """
(define (problem s%d)
    (:domain %s)
    (:init %s""" % (n,name,t),
for i in range(1,n+1):
    for j in range(1,n+1):
        if i != j:
            print >> problem, """
              (or (less n%d n%d) (not (less n%d n%d)))""" % (i,j,i,j),
            if cff:
                print >> problem, """
               (unknown (less n%d n%d))""" % (i,j),


if cff:
    t = ''
else:
    t = ')'
print >> problem, """
    %s)
    (:goal (and""" % t,
for i in range(1,n):
    print >> problem, """
             (less n%d n%d)""" % (i,i+1),
print >> problem, """
    ))
)
""" 
