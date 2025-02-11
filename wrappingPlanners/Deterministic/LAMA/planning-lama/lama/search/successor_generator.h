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

#ifndef SUCCESSOR_GENERATOR_H
#define SUCCESSOR_GENERATOR_H

#include <iostream>
#include <vector>
class Operator;
class State;

class SuccessorGenerator {
public:
    virtual ~SuccessorGenerator() {}
    virtual void generate_applicable_ops(const State &curr,
					 std::vector<const Operator *> &ops) = 0;
    void dump() {_dump("  ");}
    virtual void _dump(string indent) = 0;
};

SuccessorGenerator *read_successor_generator(std::istream &in);

#endif
