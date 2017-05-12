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

#ifndef FF_HEURISTIC_H
#define FF_HEURISTIC_H

#include "heuristic.h"
#include "globals.h"
#include "landmarks_types.h"

#include <vector>
#include <ext/hash_set>
#include <ext/hash_map>
#include <cassert>

class Operator;
class State;

class Proposition;
class UnaryOperator;

struct Proposition {
    int var;
    int val;
    bool is_goal_condition;
    bool is_termination_condition;
    std::vector<UnaryOperator *> precondition_of;

    int h_add_cost;
    int h_max_cost;
    int depth;
    UnaryOperator *reached_by;

    Proposition() {
	is_goal_condition = false;
	is_termination_condition = false;
	h_add_cost = -1;
	h_max_cost = -1;
	reached_by = 0;
    }
  
    bool operator<(const Proposition &other) const {
        return var < other.var || (var == other.var && val < other.val);
    }
  
};

struct UnaryOperator {
    const Operator *op;
    std::vector<Proposition *> precondition;
    Proposition *effect;
    int base_cost; // 0 for axioms, 1 for regular operators

    int unsatisfied_preconditions;
    int h_add_cost;
    int h_max_cost;
    int depth;
    UnaryOperator(const std::vector<Proposition *> &pre, Proposition *eff,
		  const Operator *the_op, int base)
	: op(the_op), precondition(pre), effect(eff), base_cost(base) {}

  
    bool operator<(const UnaryOperator &other) const {
        if(*(other.effect) < *effect) 
            return false;
        else if (*effect < *(other.effect)) 
            return true;
      
        else {
            int i = 0;
            while (i != precondition.size()) {
                if (i == other.precondition.size() || *(other.precondition[i]) < *(precondition[i]))
                    return false;
                else if (*(precondition[i]) < *(other.precondition[i])) 
                    return true;
                i++;
            }
            return true;
        }
    }
};


class LocalLandmarksSearchEngine;
class LandmarksCountHeuristic;

class FFHeuristic : public Heuristic {
    friend class LocalLandmarksSearchEngine;
    friend class LandmarksCountHeuristic;

    typedef __gnu_cxx::hash_set<const Operator *, hash_operator_ptr> RelaxedPlan;

    std::vector<UnaryOperator> unary_operators;
    std::vector<std::vector<Proposition> > propositions;
    std::vector<Proposition *> goal_propositions;
    std::vector<Proposition *> termination_propositions;

    typedef std::vector<Proposition *> Bucket;
    std::vector<Bucket> reachable_queue;

    bool heuristic_recomputation_needed;

    void build_unary_operators(const Operator &op);
    void simplify();

    void setup_exploration_queue(const State &state, 
				 const std::vector<std::pair<int, int> >& excluded_props,
				 const __gnu_cxx::hash_set<const Operator *, 
				 hash_operator_ptr>& excluded_ops,
				 bool use_h_max);
    inline void setup_exploration_queue(const State &state, bool h_max) {
	std::vector<std::pair<int, int> > excluded_props;
	__gnu_cxx::hash_set<const Operator *, hash_operator_ptr> excluded_ops;
	setup_exploration_queue(state, excluded_props, excluded_ops, h_max);
    }
    void relaxed_exploration(bool use_h_max, bool level_out);
    void prepare_heuristic_computation(const State& state, bool h_max);
    void collect_relaxed_plan(Proposition *goal, RelaxedPlan &relaxed_plan, const State &state);

    int compute_hsp_add_heuristic();
    int compute_hsp_max_heuristic();
    int compute_ff_heuristic(const State &state);

    void collect_ha(Proposition *goal, RelaxedPlan &relaxed_plan, const State &state);

    void enqueue_if_necessary(Proposition *prop, int cost, int depth, UnaryOperator *op, 
			      bool use_h_max);
protected:
    virtual int compute_heuristic(const State &state);
public:
    int get_lower_bound(const State &state);
    void set_additional_goals(const std::vector<std::pair<int, int> >& goals);
    void set_recompute_heuristic() {heuristic_recomputation_needed = true;}
    void compute_reachability_with_excludes(std::vector<std::vector<int> >& lvl_var, 
					    std::vector<__gnu_cxx::hash_map<pair<int, int>, int, 
					    hash_int_pair> >& lvl_op,
					    bool level_out,
					    const std::vector<std::pair<int, int> >& excluded_props,
					    const __gnu_cxx::hash_set<const Operator *, 
					    hash_operator_ptr>& excluded_ops,
					    bool compute_lvl_ops);
    std::vector<const Operator *> exported_ops; // only needed for landmarks count heuristic ha
    int plan_for_disj(std::vector<std::pair<int, int> >& disj_goal, const State& state);
    FFHeuristic(bool use_cache=false);
    ~FFHeuristic();
};

#endif
