/*********************************************************************
 * Authors: Malte Helmert (helmert@informatik.uni-freiburg.de),
 *          Silvia Richter (silvia.richter@nicta.com.au)
 * (C) Copyright 2003-2004 Malte Helmert and Silvia Richter
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
#include "helper_functions.h"

class Variable;

State::State(istream &in, const vector<Variable *> &variables) {
  check_magic(in, "begin_state");
  for(int i = 0; i < variables.size(); i++) {
    int value;
    cin >> value; //for axioms, this is default value
    values[variables[i]] = value;
  }
  check_magic(in, "end_state");
}

int State::operator[](Variable *var) const {
  return values.find(var)->second;
}

void State::dump() const {
  for(map<Variable *, int>::const_iterator it = values.begin();
      it != values.end(); ++it)
    cout << "  " << it->first->get_name() << ": " << it->second << endl;
}
