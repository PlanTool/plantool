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

#ifndef VARIABLE_H
#define VARIABLE_H

#include <iostream>
using namespace std;

class Variable {
  int range;
  string name;
  int layer;
  int level;
  bool necessary;
public:
  Variable(istream &in);
  void set_level(int level);
  void set_necessary(); 
  int get_level() const;
  bool is_necessary() const; 
  int get_range() const;
  string get_name() const;
  int get_layer() const {return layer;}
  bool is_derived() const {return layer != -1;}
  void dump() const;
};

#endif
