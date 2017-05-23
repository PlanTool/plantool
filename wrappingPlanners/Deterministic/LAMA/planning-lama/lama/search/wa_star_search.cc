/*********************************************************************
 * Author: Silvia Richter (silvia.richter@nicta.coma.au)
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

#include "wa_star_search.h"
#include "globals.h"
#include "heuristic.h"
#include "successor_generator.h"
#include "operator.h"
#include "ff_heuristic.h"
#include "landmarks_count_heuristic.h"

#include <cassert>
using namespace std;


WAStarSearchEngine::WAStarSearchEngine(int w, int b)
    : BestFirstSearchEngine() {
    weight = w;
    bound = b;
}

void WAStarSearchEngine::debug_print_partial_plan(const State& state) {
    if(current_operator != 0) {
	Plan plan;
	closed_list.trace_path(state, plan);
	for(int i = 0; i < plan.size(); i++) {
	    cout << plan[i]->get_name() << " (" << plan[i]->get_cost() << ")" << endl;
	}
    }
}

void WAStarSearchEngine::initialize() {
    cout << "Conducting weighted A* search, weight is " << weight << ", bound is " 
	 << bound <<  "." << endl;
    assert(!open_lists.empty());
}

int WAStarSearchEngine::step() {
    // Invariants:
    // - current_state is the next state for which we want to compute the heuristic.
    // - current_predecessor is a permanent pointer to the predecessor of that state.
    // - current_operator is the operator which leads to current_state from predecessor.

    // Evaluate only if g-cost of state is lower than bound
    if(bound != -1 && current_state.get_g_value() >= bound) { 
	return fetch_next_state();
    }

    bool evaluate = false;
    const State *parent_ptr = 0;
    if(!closed_list.contains(current_state)) {  
	parent_ptr = closed_list.insert(
	    current_state, current_predecessor, current_operator);
	evaluate = true;
    }
    else {
	parent_ptr = closed_list.find(current_state);
	if(current_state.get_g_value() < parent_ptr->get_g_value()) {
	    // Re-evaluate, since we have found a shorter path to this state.
	    // We need a const_cast here, as we have to modify parent, but the 
            // STL Map underlying closed_list returns a const_iterator. However, cast
            // is safe as modification of parent does not effect its position in closed_list.
	    State *modifiable_parent_ptr = const_cast<State*> (parent_ptr);
	    // Change g-value and reached landmarks in state
	    modifiable_parent_ptr->change_ancestor(*current_predecessor, *current_operator);
	    // Record new predecessor for state in closed_list
	    closed_list.update(current_state, current_predecessor, current_operator); 
	    evaluate = true;
	}
    }
    if(evaluate) {
	if(g_lm_heur != NULL)
	    g_lm_heur->set_recompute_heuristic(current_state);
	if(g_ff_heur != NULL)
	    g_ff_heur->set_recompute_heuristic(); 
        for(int i = 0; i < heuristics.size(); i++) 
            heuristics[i]->evaluate(current_state);   
 	
	if(!is_dead_end()) {
	    if(check_goal())
		return SOLVED;
	    if(check_progress()) {
		report_progress();
		reward_progress();
	    }
	    generate_successors(parent_ptr);
	}
    }
    return fetch_next_state();
}

void WAStarSearchEngine::generate_successors(const State *parent_ptr) {
    vector<const Operator *> all_operators;
    g_successor_generator->generate_applicable_ops(current_state, all_operators);
    vector<const Operator *> preferred_operators;
    for(int i = 0; i < preferred_operator_heuristics.size(); i++) {
	Heuristic *heur = preferred_operator_heuristics[i];
	if(!heur->is_dead_end())
	    heur->get_preferred_operators(preferred_operators);
    }

    for(int i = 0; i < open_lists.size(); i++) {
	Heuristic *heur = open_lists[i].heuristic;
	if(!heur->is_dead_end()) {
	    int parent_h = heur->get_heuristic();
	    int parent_g = parent_ptr->get_g_value();
	    int parent_f = weight * parent_h + parent_g;
	    OpenList<OpenListEntry> &open = open_lists[i].open;
	    vector<const Operator *> &ops =
		open_lists[i].only_preferred_operators ?
		preferred_operators : all_operators;
	    for(int j = 0; j < ops.size(); j++) {
		int h = parent_f + ops[j]->get_cost();
		int tie_braker = parent_g + ops[j]->get_cost();
		open.insert(make_pair(h, tie_braker), 
			    OpenListEntry(parent_ptr, ops[j], parent_h));
	    }
	}
    }
    generated_states += all_operators.size();
}
