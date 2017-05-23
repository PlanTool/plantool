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

#include "variable.h"

#include <cassert>
using namespace std;

Variable::Variable(istream &in) {
  in >> name >> range >> layer;
  level = -1;
  necessary = false;
}

void Variable::set_level(int theLevel) {
  assert(level == -1);
  level = theLevel;
}

int Variable::get_level() const {
  return level;
}

void Variable::set_necessary() {
  assert(necessary == false);
  necessary = true;
}

int Variable::get_range() const {
  return range;
}

string Variable::get_name() const {
  return name;
}

bool Variable::is_necessary() const {
  return necessary;
}

void Variable::dump() const {
  cout << name << " [range " << range;
  if(level != -1)
    cout << "; level " << level;
  if(is_derived())
    cout << "; derived; layer: "<< layer;
  cout << "]" << endl;
}
