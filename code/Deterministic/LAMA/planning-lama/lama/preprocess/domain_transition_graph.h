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

#ifndef DOMAIN_TRANSITION_GRAPH_H
#define DOMAIN_TRANSITION_GRAPH_H

#include <vector>
#include <fstream>
using namespace std;

class Operator;
class Axiom;
class Variable;

class DomainTransitionGraph {
public:
  typedef vector<pair<const Variable *, int> > Condition;
private:
  struct Transition {
    Transition(int theTarget, int theOp) : target(theTarget), op(theOp) {}
    bool operator==(const Transition &other) const {return target == other.target &&
						      op == other.op &&
						      condition == other.condition;}
    bool operator<(const Transition &other) const;
    int target;
    int op;
    Condition condition;
  };
  typedef vector<Transition> Vertex;
  vector<Vertex> vertices;
  int level;
public:
  DomainTransitionGraph(const Variable &var);
  void addTransition(int from, int to, const Operator &op, int op_index);
  void addAxTransition(int from, int to, const Axiom &ax, int ax_index);
  void finalize();
  void dump() const;
  void generate_cpp_input(ofstream &outfile) const;
  bool is_strongly_connected() const;
};

extern void build_DTGs(const vector<Variable *> &varOrder,
		       const vector<Operator> &operators,
		       const vector<Axiom> &axioms,
		       vector<DomainTransitionGraph> &transition_graphs);
extern bool are_DTGs_strongly_connected(const vector<DomainTransitionGraph> &transition_graphs); 
//extern vector<DomainTransitionGraph> &transition_graphs;

#endif
