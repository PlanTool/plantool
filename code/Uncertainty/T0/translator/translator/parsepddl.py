#! /usr/bin/env python2.5
# -*- coding: latin-1 -*-

#  Copyright (C) 2008 Universitat Pompeu Fabra
#  Copyright (C) 2009 Universidad Simon Bolivar
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

# Code for parsing of PDDL by Malte Helmert
# from FastDownward

class ParseError(Exception):
  pass

# Basic functions for parsing PDDL (Lisp) files.
def parse_nested_list(input_file):
  tokens = tokenize(input_file)
  return parse_tokens(tokens)

def parse_tokens(tokens):
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
    line = line.replace("(", " ( ").replace(")", " ) ")
    if 'UNKNOWN' not in line.upper(): # OJO: bad idea... just a fast hack
      for token in line.split():
        yield token.upper()

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

def parse_pddl_openfile(type, f):
  try:
    return parse_nested_list(f)
  except IOError, e:
    raise SystemExit("Error: Could not read file: %s\nReason: %s." %
                     (e.filename, e))
  except ParseError, e:
    raise SystemExit("Error: Could not parse %s file\n" % type)

def parse_pddl_file(type, filename):
  try:
    return parse_pddl_openfile(type, file(filename))
  except IOError, e:
    raise SystemExit("Error: Could not read file: %s\nReason: %s." %
                     (e.filename, e))
  except ParseError, e:
    raise SystemExit("Error: Could not parse %s file: %s\n" % (type, filename))

def get_predicates(nf):
  tokens = tokenize(file(nf))
  next_token = ""
  while next_token <> ":PREDICATES":
      next_token = tokens.next()
  return list(parse_list_aux(tokens))

