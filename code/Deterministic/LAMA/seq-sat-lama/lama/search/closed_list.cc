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

// HACK so that this will compile as a top-level target.
// This is to work around limitations of the Makefile wrt template stuff.

#ifdef CLOSED_LIST_H
// #include "closed_list.h"

// #include "state.h"

#include <algorithm>
#include <cassert>
using namespace std;

/*
  Map-based implementation of a closed list.
  Might be speeded up by using a hash_map, but then that's
  non-standard and requires defining hash keys.

  The closed list has two purposes:
  1. It stores which nodes have been expanded or scheduled to expand
  already to avoid duplicates (i.e., it is used like a set).
  These states "live" in the closed list -- in particular,
  permanently valid pointers to these states are obtained upon
  insertion.
  2. It can trace back a path from the initial state to a given state
  in the list.
  
  The datatypes used for the closed list could easily be
  parameterized, but there is no such need presently.
*/


template<class Entry, class Annotation>
ClosedList<Entry, Annotation>::ClosedList() {
}

template<class Entry, class Annotation>
ClosedList<Entry, Annotation>::~ClosedList() {
}

template<class Entry, class Annotation>
const Entry *ClosedList<Entry, Annotation>::insert(
    const Entry &entry, const Entry *predecessor, const Annotation &annotation) {
    typename map<Entry, PredecessorInfo>::iterator it =
 	closed.insert(make_pair(entry, PredecessorInfo(predecessor, annotation))).first;
    return &it->first;
}

template<class Entry, class Annotation>
const Entry *ClosedList<Entry, Annotation>::update(
    const Entry &entry, const Entry *predecessor, const Annotation &annotation) {
    typename map<Entry, PredecessorInfo>::iterator it =
 	closed.find(entry);
    it->second = PredecessorInfo(predecessor, annotation);
    return &it->first;
}

template<class Entry, class Annotation>
void ClosedList<Entry, Annotation>::clear() {
    closed.clear();
}

template<class Entry, class Annotation>
bool ClosedList<Entry, Annotation>::contains(const Entry &entry) const {
    return closed.count(entry) != 0;
}

template<class Entry, class Annotation>
const Entry* ClosedList<Entry, Annotation>::find(const Entry &entry) const {
    return &(closed.find(entry)->first);
}

template<class Entry, class Annotation>
int ClosedList<Entry, Annotation>::size() const {
    return closed.size();
}

template<class Entry, class Annotation>
void ClosedList<Entry, Annotation>::trace_path(
    const Entry &entry, vector<Annotation> &path) const {
    assert(path.empty());
    Entry current_entry = entry;
    for(;;) {
	const PredecessorInfo &info = closed.find(current_entry)->second;
	if(info.predecessor == 0)
	    break;
	path.push_back(info.annotation);
	current_entry = *info.predecessor;
    }

    reverse(path.begin(), path.end());
}

#endif
