/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au)
 * (C) Copyright 2008 NICTA
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

#ifndef CLOSED_LIST_H
#define CLOSED_LIST_H

#include <map>
#include <vector>

template<class Entry, class Annotation>
class ClosedList {
    struct PredecessorInfo {
	const Entry *predecessor;
	Annotation annotation;
	PredecessorInfo(const Entry *pred, const Annotation &annote)
	    : predecessor(pred), annotation(annote) {}
    };

    typedef std::map<Entry, PredecessorInfo> ClosedListMap;
    ClosedListMap closed;
public:
    ClosedList();
    ~ClosedList();
    const Entry *insert(const Entry &entry,
			const Entry *predecessor,
			const Annotation &annotation);
    void clear();

    bool contains(const Entry &entry) const;
    const Entry* find(const Entry &entry) const;
    const Entry *update(const Entry &entry, 
			const Entry *predecessor, 
			const Annotation &annotation);
    int size() const;
    void trace_path(const Entry &entry, std::vector<Annotation> &path) const;
};

#include "closed_list.cc" // HACK! Templates and the current Makefile don't mix well

#endif
