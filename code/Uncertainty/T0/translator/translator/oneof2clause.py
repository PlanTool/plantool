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
# Elaborated by Hector Palacios, hector.palacios@upf.edu, hectorpal@gmail.com
# Joint work with Hector Geffner.

import sys
import os
import re
import sets

name = "oneof2clause"
if len(sys.argv) <> 3 and len(sys.argv) <> 4:
    print """
    usage: %s {-or} <problem.pddl> <new-problem.pddl>
    generate to <new-problem.pddl> a PDDL problem that use logic instead of oneof
    -or to interpretate oneof as non-exclusive or
    """ % (sys.argv[0])
    sys.exit(1)

if( sys.argv[1] == '-or'):
    isor=True
    i=2
else:
    isor=False
    i = 1

pddli = file(sys.argv[i], "r")
pddlo = file(sys.argv[i+1], "w")

all = pddli.read()

exp = re.compile("""
\( (oneof) (?P<all>(
          [\t\n\ ]+
        | \( [\t\n\ ]* ([A-Za-z0-9_-]+ [\t\n\ ]+)*  [A-Za-z0-9_-]+ [\t\n\ ]* \)
        | \( [\t\n\ ]* not [\t\n\ ]* \( [\t\n\ ]*  ([A-Za-z0-9_-]+ [\t\n\ ]+)*  [A-Za-z0-9_-]+ [\t\n\ ]* \) [\t\n\ ]* \)
        )+)
\)

""", re.VERBOSE)

exp2 = re.compile("""
(
 \( [\t\n\ ]* ([A-Za-z0-9_-]+ [\t\n\ ]+)*  [A-Za-z0-9_-]+ [\t\n\ ]* \)
| \( [\t\n\ ]* not [\t\n\ ]* \( [\t\n\ ]*  ([A-Za-z0-9_-]+ [\t\n\ ]+)*  [A-Za-z0-9_-]+ [\t\n\ ]* \) [\t\n\ ]* \)
)
""", re.VERBOSE)

exp3 = re.compile("""
\( [\t\n\ ]* not [\t\n\ ]* (?P<all>(\( [\t\n\ ]*  ([A-Za-z0-9_-]+ [\t\n\ ]+)*  [A-Za-z0-9_-]+ [\t\n\ ]* \))) [\t\n\ ]* \)
""", re.VERBOSE)

def oneof(s):
    elems = s.expand(r"\2")
    items = exp2.findall(elems)
    res = '(or'
    for i in items:
        res = res + ' ' + i[0]
    res = res + ')\n'
    #return res
    s = sets.Set([])
    if(not isor):
        for i in items:
            for j in items:
                s.add((i[0], j[0]))
        for i in s:
            if i[0] != i[1] and i[0] < i[1]:
                res = res + '(or '
                r = exp3.match(i[0])
                if r == None:
                    res = res + '(not '
                    res = res + i[0]
                    res = res + ') '
                else:
                    res = res + r.expand(r"\1")
                res = res + ' '
                r = exp3.match(i[1])
                if r == None:
                    res = res + '(not '
                    res = res + i[1]
                    res = res + ') '
                else:
                    res = res + r.expand(r"\1")

                res = res + ')\n'
    return res

def oneof2(s):
    return '-> ' + s.string[s.start(0):s.end(0)] + ' ; '

res = re.sub(exp, oneof, all)

#res = re.sub(exp, oneof2, all)

pddlo.write(res)

