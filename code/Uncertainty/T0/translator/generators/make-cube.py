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

corners = False
vars=['x','y','z']
acts={'x':['right','left'],'y':['down','up'],'z':['out','in']}

if 'room' in sys.argv[0]:
    vars = vars[:2]
    name = 'emptyroom'
elif 'cube' in sys.argv[0]:
    name = 'cube'
else:
    print "Filename (or symb link) should containg 'emptyroom' or 'cube' to infer the dimension, 2 or 3"
    sys.exit(1)
    
def usage():
    print """
    usage: %s <dim> <obj> {corners}
    generates files d<dim>-g<obj>/d.pddl and p.pddl

    for the %s problem with dimension <dim>.
    The goal is reach the position <obj>,<obj>,<obj>

    corners => uncertainty is being in one if the corners
    instead of being in any possible position
    """ % (sys.argv[0],name)
    sys.exit(1)

if len(sys.argv) < 3 or len(sys.argv) > 4:
    usage()

if len(sys.argv) == 4:
    if sys.argv[3] == 'corners':
        corners = True
    else:
        print 'Error on last parameter'
        usage()

dim = int(sys.argv[1])+1
obj = int(sys.argv[2])+1

prob_name = name+'-d'+str(dim-1)+"-g"+str(obj-1)
if corners:
    prob_name += '-corners'
    
print 'Creating the problem:',prob_name

os.system("/bin/rm -rf ./"+prob_name)
os.makedirs(prob_name)
problem = file(prob_name + "/p.pddl", "w")
problem2 = file(prob_name + "/p2.pddl", "w")
domain  = file(prob_name + "/d.pddl", "w")
domain2  = file(prob_name + "/d2.pddl", "w")

domain_t = """
(define (domain %s)
  (:requirements :typing :equality)
  %s
  (:constants
""" % (name,'%s')

for i in range(1, dim):
    domain_t += "  p%d" % i

domain_t += """
%s)
  (:predicates"""
for v in vars:
    domain_t += "(%s ?p %s)" % (v,'%s')
domain_t += "  )\n"

for v in vars:
    [act_u,act_d] = acts[v]
    # act_u
    domain_t += """
  (:action %s
      :effect
       (and
       """ % act_u
    for i in range(1, dim-1):
        domain_t += """
          (when (%s p%d) (and (not (%s p%d)) (%s p%d)))""" % (v,i,v,i,v,i+1)
    domain_t += """
       )
  )
  """
    # act_d
    domain_t += """
  (:action %s
      :effect
       (and
""" % act_d
    for i in range(2, dim):
        domain_t += """
          (when (%s p%d) (and (not (%s p%d)) (%s p%d)))""" % (v,i,v,i,v,i-1 )
    domain_t += """
       )
    )
"""
# End of actions
domain_t += ")\n"

t = '- pos'
rep = ['(:types pos)',t]
rep2 = ['','']
for i in range(0,len(vars)):
    rep.append(t)
    rep2.append('')
print >> domain, domain_t % tuple(rep)
print >> domain2, domain_t % tuple(rep2)


problem_t = """
(define (problem %s-%s)
 (:domain %s)
 (:init
%s
""" % (name,'%s',name,'%s')

if corners:
    step = dim-2
else:
    step = 1
    
unknown=''
for var in vars:
    problem_t += """
      (oneof """
    for i in range(1, dim, step):
        problem_t += "(%s p%d) " % (var,i)
        unknown += """(unknown (%s p%d))
        """ % (var,i)
    problem_t += """
      ) """
        
problem_t += """
      %s %s)"""

g = ''
for var in vars:
    g += '(%s p%d)' % (var,obj)
problem_t += """ 
 (:goal
       (and %s)
       ))
""" % g


print >> problem, problem_t % (prob_name,'(and','',')')
print >> problem2, problem_t % (prob_name,'',unknown,'')





