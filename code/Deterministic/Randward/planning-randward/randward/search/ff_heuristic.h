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
#include <queue>
#include <ext/hash_set>
#include <ext/hash_map>
#include <cassert>

class Operator;
class State;

class Proposition;
class UnaryOperator;

struct Proposition {
    int id;
    int var;
    int val;

    UnaryOperator *reached_by;                           // parent
    std::vector<UnaryOperator *> precondition_of;        // children

    bool is_goal_condition;
    bool is_termination_condition;

    int h_add_cost;
    int h_max_cost;

    int ranking;

    Proposition(int id, int var, int val) {
        this->id = id;
        this->var = var;
        this->val = val;
        is_goal_condition = false;
        is_termination_condition = false;
        h_add_cost = -1;
        h_max_cost = -1;
        reached_by = NULL;
    }
};

struct UnaryOperator {
    const Operator *op;

    // Preconditions
    std::vector<Proposition *> precondition;
    int unsatisfied_preconditions;

    // Effect
    Proposition *effect;

    int base_cost;      // 0 for axioms, 1 for regular operators
    int h_add_cost;
    int h_max_cost;

    bool is_primary;

    UnaryOperator(const std::vector<Proposition *> &pre, Proposition *eff, const Operator *the_op, int base)
    : op(the_op), precondition(pre), effect(eff), base_cost(base) {
    }
};

class PropositionComparator {
public:
    bool operator() (Proposition *lhs, Proposition *rhs) const {
        return lhs->ranking < rhs->ranking;
    }
};

class ReservableQueue : public std::priority_queue<Proposition *, vector<Proposition *>, PropositionComparator> {
public:
    void reserve(size_type n) {
        c.reserve(n);
    }

    void clear() {
        c.clear();
    }
};

class LandmarksCountHeuristic;

class FFHeuristic : public Heuristic {
    friend class LandmarksCountHeuristic;

    typedef __gnu_cxx::hash_set<const Operator *, hash_operator_ptr> RelaxedPlan;

    ReservableQueue prop_queue;

    bool heuristic_recomputation_needed;

    void build_unary_operators(const Operator &op);

    void setup_exploration_queue(const State &state, 
                 const std::vector<std::pair<int, int> >& excluded_props,
                 const __gnu_cxx::hash_set<const Operator *,  hash_operator_ptr>& excluded_ops,
                 bool use_h_max = false);

    inline void setup_exploration_queue(const State &state, bool h_max) {
        std::vector<std::pair<int, int> > excluded_props;
        __gnu_cxx::hash_set<const Operator *, hash_operator_ptr> excluded_ops;
        setup_exploration_queue(state, excluded_props, excluded_ops, h_max);
    }

    void relaxed_exploration(bool use_h_max, bool level_out);
    void prepare_heuristic_computation(const State& state, bool h_max);

    void collect_relaxed_plan(Proposition *goal, RelaxedPlan &relaxed_plan, const State &state);

    int compute_hsp_add_heuristic();
    int compute_ff_heuristic(const State &state);

    void collect_ha(Proposition *goal, RelaxedPlan &relaxed_plan, const State &state);
    void enqueue_if_necessary(Proposition *prop, int cost, UnaryOperator *op, bool use_h_max);

protected:
    std::vector<UnaryOperator> unary_operators;
    std::vector<std::vector<Proposition> > propositions;
    std::vector<Proposition *> goal_propositions;
    std::vector<Proposition *> termination_propositions;

    int ranking;

    virtual int compute_heuristic(const State &state);

public:
    std::vector<const Operator *> exported_ops;     // only needed for landmarks count heuristic ha

    void set_additional_goals(const std::vector<std::pair<int, int> >& goals);

    void set_recompute_heuristic() { heuristic_recomputation_needed = true; }

    void compute_reachability_with_excludes(std::vector<std::vector<int> >& lvl_var, 
                        std::vector<__gnu_cxx::hash_map<pair<int, int>, int, hash_int_pair> >& lvl_op,
                        bool level_out,
                        const std::vector<std::pair<int, int> >& excluded_props,
                        const __gnu_cxx::hash_set<const Operator *, hash_operator_ptr>& excluded_ops,
                        bool compute_lvl_ops);

    int plan_for_disj(std::vector<std::pair<int, int> >& disj_goal, const State& state);

    FFHeuristic(bool use_cache = false);
    ~FFHeuristic();
};

#endif
