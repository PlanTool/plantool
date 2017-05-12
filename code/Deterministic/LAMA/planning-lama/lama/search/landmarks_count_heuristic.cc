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

#include <climits>

#include "landmarks_count_heuristic.h"
#include "globals.h"
#include "operator.h"
#include "search_engine.h"
#include "successor_generator.h"

LandmarksCountHeuristic::
LandmarksCountHeuristic(const LandmarksGraph& l, 
			const BestFirstSearchEngine& s,
			bool preferred_ops, FFHeuristic *ff_heur) : lgraph(l), sengine(s) {

    cout << "Initializing landmarks count heuristic..." << endl;
    preferred_operators = preferred_ops;
    ff_heuristic = ff_heur;
    lookahead = INT_MAX;
    // When generating preferred operators, we plan towards
    // non-disjunctive landmarks only
    ff_search_disjunctive_lms = false;
    // Turn goal into hash set
    goal.resize(g_goal.size());
    for(int i = 0; i < g_goal.size(); i++)
	goal.insert(make_pair( g_goal[i].first,g_goal[i].second));

}

void LandmarksCountHeuristic::set_recompute_heuristic(const State &state) {
    if(preferred_operators) {
        assert(ff_heuristic != 0);
        // Set additional goals for FF exploration
        vector<pair<int, int> > lm_leaves;
	hash_set<const LandmarkNode*, hash_pointer> result;
	state.check_partial_plan(result);
	collect_lm_leaves(ff_search_disjunctive_lms, result, lm_leaves);
        ff_heuristic->set_additional_goals(lm_leaves);
    }
}

int LandmarksCountHeuristic::compute_heuristic(const State &state) {
    // Get landmarks that have been true at some point (put into 
    // "reached_lms") and their cost
    hash_set<const LandmarkNode*, hash_pointer> reached_lms;
    const int reached_lms_cost = state.check_partial_plan(reached_lms);
    // Get landmarks that are needed again (of those in 
    // "reached_lms") because they have been made false in the meantime, 
    // but are goals or required by unachieved successors
    hash_set<const LandmarkNode*, hash_pointer> needed_lms;
    const int needed_lms_cost = state.get_needed_landmarks(needed_lms);
    assert(0 <= needed_lms_cost);
    assert(reached_lms_cost >= needed_lms_cost);
    assert(reached_lms.size() >= needed_lms.size());

    // Heuristic is total number (or cost, if action costs are used) of landmarks, 
    // minus the ones we have already achieved and do not need again
    int h;
    if(g_use_metric)
	h = lgraph.cost_of_landmarks() - reached_lms_cost + needed_lms_cost;
    else
	h = lgraph.number_of_landmarks() - reached_lms.size() + needed_lms.size();
    assert(h >= 0);

    // Test if goal has been reached even though the landmark heuristic is 
    // not 0. This may happen if landmarks are achieved before their parents 
    // in the landmarks graph, because they do not get counted as reached 
    // in that case. However, we must return 0 for a goal state.
    bool goal_reached = true;
    for(int i = 0; i < g_goal.size(); i++)
	if(state[g_goal[i].first] != g_goal[i].second)
	    goal_reached = false;
    if(goal_reached && h !=0) {
        cout << "Goal reached but Landmark heuristic != 0" << endl;
	/*
        cout << "Never accepted the following Lms:" << endl;
        const set<LandmarkNode*>& my_nodes = lgraph.get_nodes();
        set<LandmarkNode*>::const_iterator it;
        for(it = my_nodes.begin(); it != my_nodes.end(); ++it) {
            const LandmarkNode& node = **it;
            if(reached_lms.find(&node) == reached_lms.end())
                lgraph.dump_node(&node);
        }
	*/
        cout << "LM heuristic was " << h << " - returning zero" << endl;
        return 0;
    }

    // For debugging purposes, check whether heuristic is 0 even though 
    // goal is not reached. This should never happen unless action costs
    // are used where some actions have cost 0.
    if(h == 0 && !goal_reached) {
	assert(g_use_metric);
        cout << "WARNING! Landmark heuristic is 0, but goal not reached" << endl;
        for(int i = 0; i < g_goal.size(); i++)
            if(state[g_goal[i].first] != g_goal[i].second) {
                cout << "missing goal prop " << g_variable_name[g_goal[i].first] 
		     << " : " << g_goal[i].second << endl;
            }
        cout << reached_lms_cost << " " << needed_lms_cost << endl;
    }

    if(!preferred_operators || h == 0) {// no (need for) helpful actions, return
        return h;
    }

    // Try generating helpful actions (those that lead to new leaf LM in the 
    // next step). If all Lms have been reached before or no new ones can be 
    // reached within next step, helpful actions are those occuring in a PLAN 
    // to achieve one of the LM leaves.

    if(reached_lms_cost == lgraph.cost_of_landmarks() || 
       !generate_helpful_actions(state, reached_lms)) {

	assert(ff_heuristic != NULL);
	// Use FF to plan to a landmark leaf
	int dead_end = ff_search_lm_leaves(ff_search_disjunctive_lms, 
					   state, reached_lms);
	if(dead_end) {
            assert(dead_end == DEAD_END);
            ff_heuristic->exported_ops.clear();
            return DEAD_END;
	} 
	for(int i = 0; i < ff_heuristic->exported_ops.size(); i++) {
	    set_preferred(ff_heuristic->exported_ops[i]);
	}       
	ff_heuristic->exported_ops.clear();
    }
    return h;
}

void LandmarksCountHeuristic::
collect_lm_leaves(bool disjunctive_lms,
		  hash_set<const LandmarkNode*,hash_pointer>& reached_lms,
		  vector<pair<int, int> >& leaves) {

    set<LandmarkNode*>::const_iterator it;
    for(it = lgraph.get_nodes().begin(); it != lgraph.get_nodes().end(); it++) {
        LandmarkNode* node_p = *it;
    
        if(!disjunctive_lms && node_p->disjunctive )
            continue;

        if(reached_lms.find(node_p) == reached_lms.end() && 
	   !check_node_orders_disobeyed(*node_p, reached_lms)) {

            for(int i = 0; i < node_p->vars.size(); i++) { 
                pair<int, int> node_prop = make_pair(node_p->vars[i], 
						     node_p->vals[i]);
                leaves.push_back(node_prop);
            }
        }
    }
}

int LandmarksCountHeuristic::
ff_search_lm_leaves(bool disjunctive_lms, const State& state,
		    hash_set<const LandmarkNode*, hash_pointer>& reached_lms) {

    vector<pair<int, int> > leaves; 
    collect_lm_leaves(disjunctive_lms, reached_lms, leaves);
    if(ff_heuristic->plan_for_disj(leaves, state) == DEAD_END) {
        return DEAD_END;
    }
    else return 0;
}


bool LandmarksCountHeuristic::
check_node_orders_disobeyed(const LandmarkNode& node, 
			    const hash_set<const LandmarkNode*, 
			    hash_pointer>& reached) const {

    const hash_map<LandmarkNode*, edge_type, hash_pointer >& parents = node.parents;
    for(hash_map<LandmarkNode*, edge_type, hash_pointer >::const_iterator parent_it 
            = parents.begin(); parent_it != parents.end(); parent_it++) {
        const LandmarkNode& parent = *(parent_it->first);
        if(reached.find(&parent) == reached.end()) {
            return true;
        }
    }
    return false;
}


bool LandmarksCountHeuristic::
generate_helpful_actions(const State& state, 
			 const hash_set<const LandmarkNode*, hash_pointer>& reached) {

    /* Find actions that achieve new landmark leaves. If no such action exist, 
       return false. If a simple landmark can be achieved, return only operators 
       that achieve simple landmarks, else return operators that achieve 
       disjunctive landmarks */
    vector<const Operator *> all_operators;
    g_successor_generator->generate_applicable_ops(state, all_operators);
    vector<const Operator *> ha_simple;
    vector<const Operator *> ha_disj;
    
    for(int i = 0; i < all_operators.size(); i++) {
	const vector<PrePost>& prepost = all_operators[i]->get_pre_post();
	for(int j = 0; j < prepost.size(); j++) {
	    if(!prepost[j].does_fire(state))
		continue;
	    const pair<int, int> varval = make_pair(prepost[j].var, prepost[j].post);
	    const LandmarkNode* lm_p = lgraph.landmark_reached(varval);
	    if(lm_p != 0 && landmark_is_interesting(state, reached, *lm_p)) {
		
                if(lm_p->disjunctive) {
                        ha_disj.push_back(all_operators[i]);
                }
		else 
                    ha_simple.push_back(all_operators[i]);
	    }
	}
    }
    if(ha_disj.empty() && ha_simple.empty())
	return false;
 
    if(ha_simple.empty()) {
        for(int i = 0; i < ha_disj.size(); i++) {
            set_preferred(ha_disj[i]);
        }
    }
    else {
        for(int i = 0; i < ha_simple.size(); i++) {
            set_preferred(ha_simple[i]);
        }
    }
    return true;
}

bool LandmarksCountHeuristic::
landmark_is_interesting(const State& s, 
			const hash_set<const LandmarkNode*, hash_pointer>& reached, 
			const LandmarkNode& lm) const {
    /* A landmark is interesting if it hasn't been reached before and 
       its parents have all been reached, or if all landmarks have been 
       reached before, the LM is a goal, and it's not true at moment */

    if(lgraph.number_of_landmarks() != reached.size()) { 
	if(reached.find(&lm) != reached.end())
	    return false;
	else
            return !check_node_orders_disobeyed(lm, reached);
    }
    return lm.is_goal() && !lm.is_true_in_state(s);
}
