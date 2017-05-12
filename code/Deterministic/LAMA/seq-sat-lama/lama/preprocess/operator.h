/*********************************************************************
 * Authors: Malte Helmert (helmert@informatik.uni-freiburg.de),
 *          Silvia Richter (silvia.richter@nicta.com.au)
 * (C) Copyright 2003-2004 Malte Helmert and Silvia Richter
 * (C) Copyright 2008 NICTA
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

#ifndef OPERATOR_H
#define OPERATOR_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

class Variable;

class Operator {
public:
  struct Prevail {
    Variable *var;
    int prev;
    Prevail(Variable *v, int p) : var(v), prev(p) {}
  };
  struct EffCond {
    Variable *var;
    int cond;
    EffCond(Variable *v, int c) : var(v), cond(c) {}
  };
  struct PrePost {
    Variable *var;
    int pre, post;
    bool is_conditional_effect;
    vector<EffCond> effect_conds;
    PrePost(Variable *v, int pr, int po) : var(v), pre(pr), post(po) {
      is_conditional_effect = false; }
    PrePost(Variable *v, vector<EffCond> ecs, int pr, int po) : var(v), pre(pr), 
	 post(po), effect_conds(ecs) { is_conditional_effect = true; }
  };
  
private:
  string name;
  vector<Prevail> prevail;      // var, val
  vector<PrePost> pre_post; // var, old-val, new-val
  int cost;
public:
  Operator(istream &in, const vector<Variable *> &variables);

  void strip_unimportant_effects();
  bool is_redundant() const;

  void dump() const;
  void generate_cpp_input(ofstream &outfile) const;
  string get_name() const {return name;}
  const vector<Prevail> &get_prevail() const {return prevail;}
  const vector<PrePost> &get_pre_post() const {return pre_post;}
};

extern void strip_operators(vector<Operator> &operators);

#endif
