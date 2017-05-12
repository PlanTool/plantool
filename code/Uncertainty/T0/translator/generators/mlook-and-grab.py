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

trash_pos_x=1
trash_pos_y=1
object_disappear=True

name = "look-and-grab"

def usage():
    print """
    usage: %s <size> <n> <look-size> {dis|nodi}
    generates files %s-<size>-<n>-<look-size>/d.pddl and %s/p.pddl

    for the problem of picking <n> objects at a unknown position
    of a grid of <size>x<size>.
    The look-and-grab action see an action in square
    around of size 2*<look-size>+1 x 2*<look-size>+1
    (an pseudo ratio <look-size>).
    If there-is any object, it picks-them.

    dis => when pick up and holding an object, it disappears (default)
    nodis => when pick up and holding an object, it is left in current position
    """ % (sys.argv[0], name, name)
    sys.exit(1)

if len(sys.argv) < 4 or len(sys.argv) > 5:
    usage()

if len(sys.argv) == 5:
    if sys.argv[4] == 'dis':
        object_disappear = True
    elif sys.argv[4] == 'nodis':
        object_disappear = False
    else:
        print 'Error on last parameter'
        usage()
    
size = int(sys.argv[1])
n = int(sys.argv[2])
look_size = int(sys.argv[3])

name_p="%s-%d-%d-%d" % (name, size, n, look_size)
if object_disappear:
    name_p += "-disappear"
else:
    name_p += "-nodisappear"

path=name_p

os.system("/bin/rm -rf ./"+path)
os.makedirs(path)

problem = file(path + "/p.pddl", "w")
problem2 = file(path + "/p2.pddl", "w")
domain  = file(path + "/d.pddl", "w")
domain2  = file(path + "/d2.pddl", "w")

size+=1
n+=1

ctts_o = ''
for o in range(1, n):
    ctts_o += " o%d" % o

ctts_p = ''
for x in range(1, size):
    for y in range(1, size):
        ctts_p += "    p%d-%d\n" % (x,y)

def min_and_max(x):
    return max(1,x-look_size), min(size,x+look_size+1)

pick_ups = ''
for x in range(1, size):
    for y in range(1, size):
        x_i,x_e = min_and_max(x)
        y_i,y_e = min_and_max(y)
        print 'For x =',x,'i,e=',x_i,x_e
        print 'For y =',y,'i,e=',y_i,y_e
        whens_pick = ''
        for o in range(1, n):
            if object_disappear:
                also = ""
            else:
                also = "(obj-at o%d p%d-%d)" % (o,x,y)
            whens_pick += """
          (when (holding o%d)
                (and (handempty) (not (holding o%d)) %s))
            """ % (o,o,also)
        for x1 in range(x_i, x_e):
            whens_pick += """
            ; X = %d""" % x1
            for y1 in range(y_i, y_e):
                whens_pick += """
                ; Y = %d""" % y1
                for o in range(1, n):
                    whens_pick += """
          (when (and (handempty) (obj-at o%d p%d-%d))
                (and (not (handempty)) (holding o%d) (not (obj-at o%d p%d-%d))))
                                       """ % (o,x1,y1,o,o,x1,y1)
        pick_ups += """
   (:action pickup-%d-%d-look-%d
      :precondition (at p%d-%d) 
      :effect (and %s))
""" % (x,y,look_size,x,y,whens_pick)

whens_put = ''
for o in range(1, n):
    whens_put += """
                 (when (holding o%d)
                       (and (handempty) (not (holding o%d)) (obj-at o%d ?p)))
                       """ % (o,o,o)

d = """
(define (domain %s) 
""" % name + """
   (:requirements :strips :typing)
   (:types pos obj)
   (:constants %s - obj %s - pos) 
   (:predicates (adj ?i ?j) (at ?i) (holding ?o) (obj-at ?o ?i) (handempty))
   (:action move
      :parameters (?i -pos ?j - pos )
      :precondition (and (adj ?i ?j) (at ?i))
      :effect (and (not (at ?i)) (at ?j)))
%s
   (:action putdown
      :parameters (?p - pos)
      :precondition (at ?p)
      :effect (and %s))
      )
""" % (ctts_o,ctts_p,pick_ups,whens_put)

d2 = """
(define (domain %s) 
""" % name + """
   (:requirements :strips)
   (:constants %s %s) 
   (:predicates (adj ?i ?j) (at ?i) (holding ?o) (obj-at ?o ?i) (handempty))
   (:action move
      :parameters (?i ?j)
      :precondition (and (adj ?i ?j) (at ?i))
      :effect (and (not (at ?i)) (at ?j)))
%s
   (:action putdown
      :parameters (?p)
      :precondition (at ?p)
      :effect (and %s))
      )
""" % (ctts_o,ctts_p,pick_ups,whens_put)

print >> domain, d
print >> domain2, d2

head = "(define (problem %s)\n(:domain %s)\n" % (name_p,name)
print >> problem, head
print >> problem2, head
head3 = "   (:init"
print >> problem, head3
print >> problem2, head3

print >> problem, "    (and\n"
head4 = """
         (handempty)
         (at p%d-%d)
""" % (size/2, size/2)
print >> problem, head4
print >> problem2, head4
part2=""
for x1 in range(1, size-1):
    for y1 in range(1, size):
        part2 += "     (adj p%d-%d p%d-%d)\n" % (x1,y1, x1+1,y1)
        part2 += "     (adj p%d-%d p%d-%d)\n" % (x1+1,y1, x1,y1)
        part2 += "\n"
part2 += "\n"

for y1 in range(1, size-1):
    for x1 in range(1, size):
        part2 += "     (adj p%d-%d p%d-%d)\n" % (x1,y1, x1,y1+1)
        part2 += "     (adj p%d-%d p%d-%d)\n" % (x1,y1+1, x1,y1)
        part2 += "\n"
    

print >> problem, part2
print >> problem2, part2

oneof = ""
unknown = ""
for o in range(1, n):
    oneof += "     (oneof\n" 
    for x in range(1, size):
        for y in range(1, size):
            oneof += "        (obj-at o%d p%d-%d)\n" % (o,x,y)
            unknown += "        (unknown (obj-at o%d p%d-%d))\n" % (o,x,y)
    oneof += "     )\n" 

print >> problem, oneof
print >> problem2, oneof
print >> problem2, unknown

print >> problem, "    )"
goal=""
goal += "(and "
for o in range(1, n):
    goal += "    (obj-at o%d p%d-%d)\n" % (o, trash_pos_x, trash_pos_y)
goal += ")"

end =  "    )\n    (:goal %s))" % goal

print >> problem, end
print >> problem2, end

