#######################################################################
#
# Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
# (C) Copyright 2003-2004 Malte Helmert
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

import cStringIO
import textwrap

__all__ = ["print_nested_list"]

def tokenize_list(obj):
  if isinstance(obj, list):
    yield "("
    for item in obj:
      for elem in tokenize_list(item):
        yield elem
    yield ")"
  else:
    yield obj

def wrap_lines(lines):
  for line in lines:
    indent = " " * (len(line) - len(line.lstrip()) + 4)
    line = line.replace("-", "_") # textwrap breaks on "-", but not "_"
    line = textwrap.fill(line, subsequent_indent=indent, break_long_words=False)
    yield line.replace("_", "-")

def print_nested_list(nested_list):
  stream = cStringIO.StringIO()
  indent = 0
  startofline = True
  pendingspace = False
  for token in tokenize_list(nested_list):
    if token == "(":
      if not startofline:
        stream.write("\n")
      stream.write("%s(" % (" " * indent))
      indent += 2
      startofline = False
      pendingspace = False
    elif token == ")":
      indent -= 2
      stream.write(")")
      startofline = False
      pendingspace = False
    else:
      if startofline:
        stream.write(" " * indent)
      if pendingspace:
        stream.write(" ")
      stream.write(token)
      startofline = False
      pendingspace = True

  for line in wrap_lines(stream.getvalue().splitlines()):
    print line
