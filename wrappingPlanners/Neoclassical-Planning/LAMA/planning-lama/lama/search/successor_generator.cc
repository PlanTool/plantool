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

#include "globals.h"
#include "operator.h"
#include "state.h"
#include "successor_generator.h"

#include <cstdlib>
#include <iostream>
#include <vector>
using namespace std;

class SuccessorGeneratorSwitch : public SuccessorGenerator {
    int switch_var;
    SuccessorGenerator *immediate_ops;
    vector<SuccessorGenerator *> generator_for_value;
    SuccessorGenerator *default_generator;
public:
    SuccessorGeneratorSwitch(istream &in);
    virtual void generate_applicable_ops(const State &curr,
					 vector<const Operator *> &ops);
    virtual void _dump(string indent);
};

class SuccessorGeneratorGenerate : public SuccessorGenerator {
    vector<const Operator *> op;
public:
    SuccessorGeneratorGenerate(istream &in);
    virtual void generate_applicable_ops(const State &curr,
					 vector<const Operator *> &ops);
    virtual void _dump(string indent);
};

SuccessorGeneratorSwitch::SuccessorGeneratorSwitch(istream &in) {
    cin >> switch_var;
    immediate_ops = read_successor_generator(in);
    for(int i = 0; i < g_variable_domain[switch_var]; i++)
	generator_for_value.push_back(read_successor_generator(in));
    default_generator = read_successor_generator(in);
}

void SuccessorGeneratorSwitch::generate_applicable_ops(
    const State &curr, vector<const Operator *> &ops) {
    immediate_ops->generate_applicable_ops(curr, ops);
    generator_for_value[curr[switch_var]]->generate_applicable_ops(curr, ops);
    default_generator->generate_applicable_ops(curr, ops);
}

void SuccessorGeneratorSwitch::_dump(string indent) {
    cout << indent << "switch on " << g_variable_name[switch_var] << endl;
    cout << indent << "immediately:" << endl;
    immediate_ops->_dump(indent + "  ");
    for(int i = 0; i < g_variable_domain[switch_var]; i++) {
	cout << indent << "case " << i << ":" << endl;
	generator_for_value[i]->_dump(indent + "  ");
    }
    cout << indent << "always:" << endl;
    default_generator->_dump(indent + "  ");
}

void SuccessorGeneratorGenerate::generate_applicable_ops(const State &,
							 vector<const Operator *> &ops) {
    ops.insert(ops.end(), op.begin(), op.end());
}

SuccessorGeneratorGenerate::SuccessorGeneratorGenerate(istream &in) {
    int count;
    cin >> count;
    for(int i = 0; i < count; i++) {
	int op_index;
	in >> op_index;
	op.push_back(&g_operators[op_index]);
    }
}

void SuccessorGeneratorGenerate::_dump(string indent) {
    for(int i = 0; i < op.size(); i++) {
	cout << indent;
	op[i]->dump();
    }
}

SuccessorGenerator *read_successor_generator(istream &in) {
    string type;
    in >> type;
    if(type == "switch") {
	return new SuccessorGeneratorSwitch(in);
    } else if(type == "check") {
	return new SuccessorGeneratorGenerate(in);
    }
    cout << "Illegal successor generator statement!" << endl;
    cout << "Expected 'switch' or 'check', got '" << type << "'." << endl;
    exit(1);
}

