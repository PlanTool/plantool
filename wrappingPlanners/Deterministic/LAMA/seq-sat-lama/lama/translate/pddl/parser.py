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

__all__ = ["ParseError", "parse_nested_list"]

class ParseError(Exception):
  pass

# Basic functions for parsing PDDL (Lisp) files.
def parse_nested_list(input_file):
  tokens = tokenize(input_file)
  next_token = tokens.next()
  if next_token != "(":
    raise ParseError("Expected '(', got %s." % next_token)
  result = list(parse_list_aux(tokens))
  for tok in tokens:  # Check that generator is exhausted.
    raise ParseError("Unexpected token: %s." % tok)
  return result
  
def tokenize(input):
  for line in input:
    line = line.split(";", 1)[0]  # Strip comments.
    line = line.replace("(", " ( ").replace(")", " ) ").replace("?", " ?")
    for token in line.split():
      yield token.lower()

def parse_list_aux(tokenstream):
  # Leading "(" has already been swallowed.
  while True:
    try:
      token = tokenstream.next()
    except StopIteration:
      raise ParseError()
    if token == ")":
      return
    elif token == "(":
      yield list(parse_list_aux(tokenstream))
    else:
      yield token

