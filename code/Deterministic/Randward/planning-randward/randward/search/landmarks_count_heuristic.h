/*********************************************************************
 * Authors: Matthias Westphal (westpham@informatik.uni-freiburg.de),
 *          Silvia Richter (silvia.richter@nicta.com.au)
 * (C) Copyright 2008 Matthias Westphal and NICTA
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

#ifndef LANDMARKS_COUNT_HEURISTIC_H
#define LANDMARKS_COUNT_HEURISTIC_H

#include "state.h"
#include "heuristic.h"
#include "landmarks_graph.h"
#include "best_first_search.h"
#include "ff_heuristic.h"

class LandmarksCountHeuristic : public Heuristic {

    const LandmarksGraph& lgraph;
    const BestFirstSearchEngine& sengine;
    bool preferred_operators;
    int lookahead;
    FFHeuristic* ff_heuristic;
    bool ff_search_disjunctive_lms;

    lm_set goal;
    hash_set<const LandmarkNode*, hash_pointer> initial_state_landmarks;

    void collect_lm_leaves(bool disjunctive_lms,
			   hash_set<const LandmarkNode*,hash_pointer>& result,
			   vector<pair<int, int> >& leaves);
    int ff_search_lm_leaves(bool disjunctive_lms, const State& state,
			    hash_set<const LandmarkNode*, hash_pointer>& result);
  
    bool check_node_orders_disobeyed(const LandmarkNode& node, 
				     const hash_set<const LandmarkNode*, 
				     hash_pointer>& reached) const;
 
    void add_node_children(const LandmarkNode& node, 
			   const hash_set<const LandmarkNode*, 
			   hash_pointer>& reached) const;

    bool landmark_is_interesting(const State& s, const hash_set<const LandmarkNode*, 
				 hash_pointer>& reached, const LandmarkNode& lm) const;
    bool generate_helpful_actions(const State& state, 
				  const hash_set<const LandmarkNode*, hash_pointer>& reached);
protected:
    virtual int compute_heuristic(const State &state);
public:
    LandmarksCountHeuristic(const LandmarksGraph& l, const BestFirstSearchEngine& s,
                            bool use_preferred_operators, FFHeuristic *ff_heur);
    ~LandmarksCountHeuristic() {}
    void set_recompute_heuristic(const State &state);
    virtual bool dead_ends_are_reliable() {return false;}
};

#endif
