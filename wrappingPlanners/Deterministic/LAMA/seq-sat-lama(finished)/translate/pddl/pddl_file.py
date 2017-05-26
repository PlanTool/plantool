#! /usr/bin/env python
# -*- coding: latin-1 -*-

#######################################################################
#
# Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
# (C) Copyright 2003-2004 Malte Helmert
# Modified by: Silvia Richter (silvia.richter@nicta.com.au)
# (C) Copyright 2008: NICTA
#
# This file is part of LAMA.
#
# LAMA is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the license, or (at your option) any later version.
#
# LAMA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
#
#######################################################################

import sys
import os.path
import re

import parser

import tasks

def parse_pddl_file(type, filename):
  try:
    return parser.parse_nested_list(file(filename))
  except IOError, e:
    raise SystemExit("Error: Could not read file: %s\nReason: %s." %
                     (e.filename, e))
  except parser.ParseError, e:
    raise SystemExit("Error: Could not parse %s file: %s\n" % (type, filename))

def open(task_filename=None, domain_filename=None):
  '''
  if task_filename is None:
    if len(sys.argv) not in (2, 3):
      raise SystemExit("Error: Need exactly one or two command line arguments.\n"
                       "Usage: %s [<domain.pddl>] <task.pddl>" % sys.argv[0])

    task_filename = sys.argv[-1]
    if len(sys.argv) == 3:
      domain_filename = sys.argv[1]

  if not domain_filename:
    dirname, basename = os.path.split(task_filename)
    domain_filename = os.path.join(dirname, "domain.pddl")
    if not os.path.exists(domain_filename) and re.match(r"p[0-9][0-9]\b", basename):
      domain_filename = os.path.join(dirname, basename[:4] + "domain.pddl")
    if not os.path.exists(domain_filename) and re.match(r"p[0-9][0-9]\b", basename):
      domain_filename = os.path.join(dirname, basename[:3] + "-domain.pddl")
    if not os.path.exists(domain_filename) and re.match(r"p[0-9][0-9]\b", basename):
      domain_filename = os.path.join(dirname, "domain_" + basename)
    if not os.path.exists(domain_filename):
      raise SystemExit("Error: Could not find domain file using "
                       "automatic naming rules.")
  '''
  domain_pddl = parse_pddl_file("domain", domain_filename)
  task_pddl = parse_pddl_file("task", task_filename)
  return tasks.Task.parse(domain_pddl, task_pddl)

if __name__ == "__main__":
  open().dump()
