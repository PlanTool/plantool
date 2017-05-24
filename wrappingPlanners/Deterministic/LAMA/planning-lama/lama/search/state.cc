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

#include "state.h"

#include "axioms.h"
#include "globals.h"
#include "operator.h"
#include "landmarks_graph.h"

#include <algorithm>
#include <iostream>
#include <cassert>
using namespace std;

State::State(istream &in) {
    check_magic(in, "begin_state");
    for(int i = 0; i < g_variable_domain.size(); i++) {
	int var;
	cin >> var;
	vars.push_back(var);
    }
    check_magic(in, "end_state");

    g_default_axiom_values = vars;
    g_value = 0;
    reached_lms_cost = 0;
}

void State::update_reached_lms(const Operator &op) {
    if(g_lgraph == NULL)
	return;
    for(int j = 0; j < op.get_pre_post().size(); j++) {
	const PrePost &pre_post = op.get_pre_post()[j];
	// Test whether this effect got applied (it may have been conditional)
	if((*this)[pre_post.var] == pre_post.post) {
	    const LandmarkNode* node_p = 
		g_lgraph->landmark_reached(make_pair(pre_post.var, pre_post.post));
	    if(node_p != 0) {
		if(reached_lms.find(node_p) == reached_lms.end()) {
		    //cout << endl << "New LM reached! +++++++ ";
		    //g_lgraph->dump_node(node_p);
		    // Only add leaves of landmark graph to reached
		    const LandmarkNode& node = *node_p;
		    if(landmark_is_leaf(node, reached_lms)) { 
			//cout << "inserting new LM into reached. (1)" << endl; 
			reached_lms.insert(node_p);
			reached_lms_cost += node_p->min_cost;
		    } 
		    //else
		    //cout << "not inserting into reached, has parents" << endl;
		}
	    }
	}
    }
    const set<LandmarkNode*>& nodes = g_lgraph->get_nodes();
    set<LandmarkNode*>::const_iterator it = nodes.begin();
    for(; it != nodes.end(); ++it) {
	const LandmarkNode* node = *it;
	for(int i = 0; i < node->vars.size(); i++) {
	    if((*this)[node->vars[i]] == node->vals[i]) {
		if(reached_lms.find(node) == reached_lms.end()) {
		    //cout << "New LM reached by axioms: "; g_lgraph->dump_node(node);
		    if(landmark_is_leaf(*node, reached_lms)) { 
			//cout << "inserting new LM into reached. (2)" << endl; 
			reached_lms.insert(node);  
			reached_lms_cost += node->min_cost;
		    } 
		    //else
			//cout << "not inserting into reached, has parents" << endl;
		}
	    }
	}
    }
}

void State::change_ancestor(const State &new_predecessor, const Operator &new_op) {
    reached_lms = new_predecessor.reached_lms; // Can this be a problem?
    reached_lms_cost = new_predecessor.reached_lms_cost;
    update_reached_lms(new_op);
    g_value = new_predecessor.get_g_value() + new_op.get_cost();
    if (g_use_metric) // if using action costs, all costs have been increased by 1
	g_value--;
}

State::State(const State &predecessor, const Operator &op)
    : vars(predecessor.vars), reached_lms(predecessor.reached_lms), 
      reached_lms_cost(predecessor.reached_lms_cost) {
    assert(!op.is_axiom());

    // Update values affected by operator.
    for(int i = 0; i < op.get_pre_post().size(); i++) {
	const PrePost &pre_post = op.get_pre_post()[i];
	if(pre_post.does_fire(predecessor))
	    vars[pre_post.var] = pre_post.post;
    }
    g_axiom_evaluator->evaluate(*this);
    // Update set of reached landmarks.
    update_reached_lms(op);
    // Update g_value
    g_value = predecessor.get_g_value() + op.get_cost();
    if (g_use_metric) // if using action costs, all costs have been increased by 1
	g_value--;
}

void State::dump() const {
    for(int i = 0; i < vars.size(); i++)
	cout << "  " << g_variable_name[i] << ": " << vars[i] << endl;
}

bool State::operator<(const State &other) const {
    return lexicographical_compare(vars.begin(), vars.end(),
				   other.vars.begin(), other.vars.end());
}

void State::set_landmarks_for_initial_state() {
    hash_set<const LandmarkNode*, hash_pointer> initial_state_landmarks;
    if(g_lgraph == NULL) {
	// landmarks not used, set empty
	reached_lms = initial_state_landmarks;
	return;
    }
    for(int i = 0; i < g_variable_domain.size(); i++) {
        const pair<int, int> a = make_pair(i, (*g_initial_state)[i]);
        if(g_lgraph->simple_landmark_exists(a)) {
            LandmarkNode& node = g_lgraph->get_simple_lm_node(a);
            if(node.parents.size() == 0) {
                initial_state_landmarks.insert(&node);
                reached_lms_cost += node.min_cost;
            }
        }
	else{
	    set<pair<int, int> > a_set;
	    a_set.insert(a);
	    if (g_lgraph->disj_landmark_exists(a_set)) {
		LandmarkNode& node = g_lgraph->get_disj_lm_node(a);
		if(node.parents.size() == 0) {
		    initial_state_landmarks.insert(&node);
		    reached_lms_cost += node.min_cost;
		}
	    }
	}
    } 
    cout << initial_state_landmarks.size() << " initial landmarks, " 
	 << g_goal.size() << " goal landmarks" << endl; 
    reached_lms = initial_state_landmarks;
}

bool State::landmark_is_leaf(const LandmarkNode& node, 
			     const hash_set<const LandmarkNode*, hash_pointer>& reached) const {
//Note: this is the same as !check_node_orders_disobeyed
    const hash_map<LandmarkNode*, edge_type, hash_pointer >& parents 
        = node.parents;
    /*
      cout << "in is_leaf, reached is ----- " << endl;
      hash_set<const LandmarkNode*, hash_pointer>::const_iterator it;
      for(it = reached.begin(); it != reached.end(); ++it) {
      cout << *it << " ";
      lgraph.dump_node(*it);
      }
      cout << "---------" << endl;
    */
    for(hash_map<LandmarkNode*, edge_type, hash_pointer >::const_iterator parent_it = 
            parents.begin(); parent_it != parents.end(); parent_it++) {
        LandmarkNode* parent_p = parent_it->first;

        if(true) // Note: no condition on edge type here

            if(reached.find(parent_p) == reached.end()){

                //cout << "parent is not in reached: "; 
                //cout << parent_p << " ";
                //lgraph.dump_node(parent_p);
		return false;
            }
    }
    //cout << "all parents are in reached" << endl;
    return true;
}

bool State::check_lost_landmark_children_needed_again(const LandmarkNode& node) const {
    const hash_set<const LandmarkNode*, hash_pointer>& reached = this->reached_lms;
    const hash_map<LandmarkNode*, edge_type, hash_pointer >& children 
        = node.children;
  
    for(hash_map<LandmarkNode*, edge_type, hash_pointer >::const_iterator child_it = 
            children.begin(); child_it != children.end(); child_it++) {
        LandmarkNode* child_p = child_it->first;
        if(child_it->second == gn) // Note: condition on edge type here!
            if(reached.find(child_p) == reached.end()){
                return true;
            }
    }
    return false;
}

int State::get_needed_landmarks(hash_set<const LandmarkNode*, hash_pointer>& needed) const {
    // Calculate landmarks that will be needed again and their cost
    const hash_set<const LandmarkNode*, hash_pointer>& reached = this->reached_lms;
    int needed_lm_cost = 0;
    for(hash_set<const LandmarkNode*, hash_pointer>::const_iterator it 
            = reached.begin(); it != reached.end(); it++)
        if(!(*it)->is_true_in_state(*this)) {
            if((*it)->is_goal()) {
		needed.insert(*it);
		needed_lm_cost += (*it)->min_cost;
	    }
	    else {
	        const LandmarkNode& node = **it;
		if(check_lost_landmark_children_needed_again(node)) {
		    needed.insert(&node);
		    needed_lm_cost += node.min_cost;
		    //cout << "needed again is "; g_lgraph->dump_node(&node);
		}
	    }
	}
    return needed_lm_cost;
}


int State::check_partial_plan(hash_set<const LandmarkNode*, hash_pointer>& reached) const {
    // Return reached landmarks and their cost
    reached = reached_lms;
    return reached_lms_cost;
}
