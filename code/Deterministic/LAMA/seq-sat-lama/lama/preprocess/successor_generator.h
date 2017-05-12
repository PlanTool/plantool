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

#ifndef SUCCESSOR_GENERATOR_H
#define SUCCESSOR_GENERATOR_H

#include <list>
#include <vector>
#include <fstream>
#include <map>
using namespace std;

class GeneratorBase;
class Operator;
class Variable;

class SuccessorGenerator {
  GeneratorBase *root;

  typedef vector<pair<Variable *, int> > Condition;
  GeneratorBase *construct_recursive(int switchVarNo, list<int> &ops);
  SuccessorGenerator(const SuccessorGenerator &copy);

  vector<Condition> conditions;
  vector<Condition::const_iterator> next_condition_by_op;
  vector<Variable *> varOrder;

  // private copy constructor to forbid copying;
  // typical idiom for classes with non-trivial destructors
public:
  SuccessorGenerator();
  SuccessorGenerator(const vector<Variable *> &variables,
		     const vector<Operator> &operators);
  ~SuccessorGenerator();
  void dump() const;
  void generate_cpp_input(ofstream &outfile) const;
};

#endif
