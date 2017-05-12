/*********************************************************************
 * Authors: Malte Helmert (helmert@informatik.uni-freiburg.de),
 *          Silvia Richter (silvia.richter@nicta.com.au)
 * (C) Copyright 2003-2004 Malte Helmert and Silvia Richter
 *
 * This file is part of LAMA.
 *
 * LAMA is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the license, or (at your option) any later version.
 *
 * LAMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 *********************************************************************/

#ifndef AXIOM_H
#define AXIOM_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

class Variable;

class Axiom {
public:
  struct Condition {
    Variable *var;
    int cond;
    Condition(Variable *v, int c) : var(v), cond(c) {}
  };
private:
  Variable *effect_var;
  int old_val; 
  int effect_val;
  vector<Condition> conditions;      // var, val
public:
  Axiom(istream &in, const vector<Variable *> &variables);

  bool is_redundant() const;
  void dump() const;
  void generate_cpp_input(ofstream &outfile) const;
  const vector<Condition> &get_conditions() const {return conditions;}
  Variable* get_effect_var() const {return effect_var;}
  int get_old_val() const {return old_val;}
  int get_effect_val() const {return effect_val;}
};

extern void strip_axioms(vector<Axiom> &axioms);

#endif
