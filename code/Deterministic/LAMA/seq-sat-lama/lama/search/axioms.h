/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
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

#ifndef AXIOMS_H
#define AXIOMS_H

#include <vector>

class State;

class AxiomEvaluator {
    struct AxiomRule;
    struct AxiomLiteral {
	std::vector<AxiomRule *> condition_of;
    };
    struct AxiomRule {
	int condition_count;
	int unsatisfied_conditions;
	int effect_var;
	int effect_val;
	AxiomLiteral *effect_literal;
	AxiomRule(int cond_count, int eff_var, int eff_val, AxiomLiteral *eff_literal)
	    : condition_count(cond_count), unsatisfied_conditions(cond_count),
	      effect_var(eff_var), effect_val(eff_val), effect_literal(eff_literal) {
	}
    };
    struct NegationByFailureInfo {
	int var_no;
	AxiomLiteral *literal;
	NegationByFailureInfo(int var, AxiomLiteral *lit)
	    : var_no(var), literal(lit) {}
    };

    std::vector<std::vector<AxiomLiteral> > axiom_literals;
    std::vector<AxiomRule> rules;
    std::vector<std::vector<NegationByFailureInfo> > nbf_info_by_layer;
public:
    AxiomEvaluator();
    void evaluate(State &state);
};

#endif
