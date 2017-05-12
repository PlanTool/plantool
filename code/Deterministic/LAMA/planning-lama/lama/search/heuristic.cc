/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au)
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

#include "heuristic.h"
#include "state.h"

#ifndef NDEBUG
#include "operator.h"
#endif

#include <cassert>
using namespace std;

Heuristic::Heuristic(bool use_caching) {
    use_cache = use_caching;
    heuristic = INVALID;
}

Heuristic::~Heuristic() {
}

void Heuristic::set_preferred(const Operator *op) {
    preferred_operators.push_back(op);
}

void Heuristic::evaluate(const State &state) {
    if(use_cache) {
	map<State, EvaluationInfo>::iterator it =
	    state_cache.find(state);
	if(it != state_cache.end()) {
	    heuristic = it->second.heuristic;
	    preferred_operators = it->second.preferred_operators;
	    return;
	}
    }

    preferred_operators.clear();
    heuristic = compute_heuristic(state);
    assert(heuristic == DEAD_END || heuristic >= 0);

    if(heuristic == DEAD_END) {
	// It is ok to have preferred operators in dead-end states.
	// This allows a heuristic to mark preferred operators on-the-fly,
	// selecting the first ones before it is clear that all goals
	// can be reached.
	preferred_operators.clear();
    }

    if(use_cache) {
	EvaluationInfo info(heuristic, preferred_operators);
	state_cache[state] = info;
    }

#ifndef NDEBUG
    if(heuristic != DEAD_END) {
	for(int i = 0; i < preferred_operators.size(); i++)
	    assert(preferred_operators[i]->is_applicable(state));
    }
#endif
}

bool Heuristic::is_dead_end() {
    return heuristic == DEAD_END;
}

int Heuristic::get_heuristic() {
    // The -1 value for dead ends is an implementation detail which is
    // not supposed to leak. Thus, calling this for dead ends is an
    // error. Call "is_dead_end()" first.
    assert(heuristic >= 0);
    return heuristic;
}

void Heuristic::get_preferred_operators(std::vector<const Operator *> &result) {
    //assert(heuristic >= 0); Note: this should be "!heuristic_recomputation_needed" instead...
    //when called by local_landmarks_search for the first time, the heuristic is still INVALID, 
    //although the relaxed exploration for FF has been done (which is what we really need here)
    result.insert(result.end(),
		  preferred_operators.begin(),
		  preferred_operators.end());
}
