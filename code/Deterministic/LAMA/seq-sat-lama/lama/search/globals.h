/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au),
 *              Matthias Westphal (westpham@informatik.uni-freiburg.de)             
 * (C) Copyright 2008 NICTA and Matthias Westphal
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

#ifndef GLOBALS_H
#define GLOBALS_H

#include <iostream>
#include <string>
#include <vector>
using namespace std;

class AxiomEvaluator;
class Cache;
class CausalGraph;
class DomainTransitionGraph;
class Operator;
class Axiom;
class State;
class SuccessorGenerator;
class FFHeuristic;
class LandmarksCountHeuristic;
class LandmarksGraph;

void read_everything(istream &in, bool generate_landmarks, bool reasonable_orders);
void dump_everything();

void check_magic(istream &in, string magic);


struct hash_operator_ptr {
    size_t operator()(const Operator *key) const {
	return reinterpret_cast<unsigned long>(key);
    }
};

extern bool g_use_metric;
extern vector<string> g_variable_name;
extern vector<int> g_variable_domain;
extern vector<int> g_axiom_layers;
extern vector<int> g_default_axiom_values;

extern State *g_initial_state;
extern vector<pair<int, int> > g_goal;
extern vector<Operator> g_operators;
extern vector<Operator> g_axioms;
extern AxiomEvaluator *g_axiom_evaluator;
extern SuccessorGenerator *g_successor_generator;
extern vector<DomainTransitionGraph *> g_transition_graphs;
extern CausalGraph *g_causal_graph;
extern Cache *g_cache;
extern int g_cache_hits, g_cache_misses;

extern FFHeuristic *g_ff_heur;
extern LandmarksCountHeuristic *g_lm_heur;
extern LandmarksGraph *g_lgraph;

#endif
