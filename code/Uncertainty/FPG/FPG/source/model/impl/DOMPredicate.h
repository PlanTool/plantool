/*
 *  DOMPredicate.h
 *  
 *
 *  Created by Owen Thomas on 9/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_predicate
#define dom_predicate
class DOMDomain;

#include<set>

#include "DOMWrapper.h"
class DOMPredicate : public DOMWrapper
{
	
	public:

		/** 
		 * [daa] Shortcut types for lists over DOMPredicates. Used by planner.
                 */
                typedef std::set<DOMPredicate*> PredicateSet;
		typedef PredicateSet::iterator PredicateIt;
		typedef PredicateSet::const_iterator PredicateCIt;

		DOMPredicate (DOMElement* node)
		{
			this->node = node;
		}
		
		DOMPredicate (const char* name,DOMDocument* doc)
		{
			XMLCh* predicate = XMLString::transcode ("predicate");
			this->node = doc->createElement (predicate);
			
			setAttribute ("name",name);
		}
		
		/**
		 * Creates a predicate named name, and adds it 
		 * to domain.
		 */
		DOMPredicate (const char* name, DOMDomain* domain); 
			virtual ~DOMPredicate () {}
		
		/**
		 * Return the name of this predicate.
		 */
		char* getName () {
			return getAttribute  ("name");
		}

};

#endif
