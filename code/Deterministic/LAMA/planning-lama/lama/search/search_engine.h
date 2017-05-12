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

#ifndef SEARCH_ENGINE_H
#define SEARCH_ENGINE_H

#include <vector>

class Operator;

class SearchEngine {
public:
    typedef std::vector<const Operator *> Plan;
private:
    bool solved;
    Plan plan;
protected:
    enum {FAILED, SOLVED, IN_PROGRESS};
    virtual void initialize() {}
    virtual int step() = 0;

    void set_plan(const Plan &plan);
public:
    SearchEngine();
    virtual ~SearchEngine();
    virtual void statistics() const;
    bool found_solution() const;
    const Plan &get_plan() const;
    void search();
};

#endif
