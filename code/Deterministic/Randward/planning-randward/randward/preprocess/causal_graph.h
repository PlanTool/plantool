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

#ifndef CAUSAL_GRAPH_H
#define CAUSAL_GRAPH_H

#include <vector>
#include <map>
#include <fstream>
using namespace std;

class Operator;
class Axiom;
class Variable;

class CausalGraph {
  const vector<Variable *> &variables;
  const vector<Operator> &operators;
  const vector<Axiom> &axioms;
  const vector<pair<Variable *, int> > &goals;

  typedef map<Variable *, int> WeightedSuccessors;
  typedef map<Variable *, WeightedSuccessors> WeightedGraph;
  WeightedGraph weighted_graph;
  typedef map<Variable *, int> Predecessors;
  typedef map<Variable *, Predecessors> PredecessorGraph;
  // predecessor_graph is weighted_graph with edges turned around
  PredecessorGraph predecessor_graph;

  typedef vector<vector<Variable *> > Partition;
  typedef vector<Variable *> Ordering;
  Ordering ordering;
  bool acyclic;

  void weigh_graph_from_ops(const vector<Variable *> &variables,
			    const vector<Operator> &operators,
			    const vector<pair<Variable *, int> > &goals);
  void weigh_graph_from_axioms(const vector<Variable *> &variables,
			       const vector<Axiom> &axioms,
			       const vector<pair<Variable *, int> > &goals);
  void get_strongly_connected_components(Partition &sccs);
  void calculate_topological_pseudo_sort(const Partition &sccs);
  void calculate_important_vars();
  void dfs(Variable *from);
public:
  CausalGraph(const vector<Variable *> &variables,
	      const vector<Operator> &operators,
	      const vector<Axiom> &axioms,
	      const vector<pair<Variable *, int> > &the_goals);
  ~CausalGraph() {}
  const vector<Variable *> &get_variable_ordering() const;
  bool is_acyclic() const;
  void dump() const;
  void generate_cpp_input(ofstream &outfile, 
			  const vector<Variable *> & ordered_vars) const;
};

extern bool g_do_not_prune_variables;

#endif
