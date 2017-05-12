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

/* Main file, keeps all important variables.
 * Calls functions from "helper_functions" to read in input (variables, operators, 
 * goals, initial state),
 * then calls functions to build causal graph, domain_transition_graphs and 
 * successor generator
 * finally prints output to file "output"
 */

#include "helper_functions.h"
#include "successor_generator.h"
#include "causal_graph.h"
#include "domain_transition_graph.h"
#include "state.h"
#include "operator.h"
#include "axiom.h"
#include "variable.h"
#include <iostream>
using namespace std;

int main(int argc, const char **argv) {
  bool metric;
  vector<Variable *> variables;
  vector<Variable> internal_variables;
  State initial_state;
  vector<pair<Variable *, int> > goals;
  vector<Operator> operators;
  vector<Axiom> axioms;
  vector<DomainTransitionGraph> transition_graphs;

  if(argc != 1) {
      cout << "*** do not perform relevance analysis ***" << endl;
      g_do_not_prune_variables = true;
  }

  read_preprocessed_problem_description
    (cin, metric, internal_variables, variables, initial_state, goals, operators, axioms);
  //dump_preprocessed_problem_description
  //  (variables, initial_state, goals, operators, axioms);
  
  cout << "Building causal graph..." << endl;
  CausalGraph causal_graph(variables, operators, axioms, goals);
  const vector<Variable *> &ordering = causal_graph.get_variable_ordering();
  bool cg_acyclic = causal_graph.is_acyclic();

  // Remove unnecessary effects from operators and axioms, then remove
  // operators and axioms without effects.
  strip_operators(operators);
  strip_axioms(axioms);

  cout << "Building domain transition graphs..." << endl;
  build_DTGs(ordering, operators, axioms, transition_graphs);
  //dump_DTGs(ordering, transition_graphs);
  bool solveable_in_poly_time = false;
  if(cg_acyclic)
    solveable_in_poly_time = are_DTGs_strongly_connected(transition_graphs);
  cout << "solveable in poly time " << solveable_in_poly_time << endl;
  cout << "Building successor generator..." << endl;
  SuccessorGenerator successor_generator(ordering, operators);
  //successor_generator.dump();

  cout << "Writing output..." << endl;
  generate_cpp_input(solveable_in_poly_time, ordering, metric, initial_state, 
		     goals, operators, axioms, successor_generator, 
		     transition_graphs, causal_graph);
  cout << "done" << endl << endl;
}
