/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  DOMPredicateCondition.h
 *  
 *
 *  Created by Owen Thomas on 10/03/06.
 *  
 *
 */

#ifndef dom_predicate_condition
#define dom_predicate_condition

#include "DOMConditionBase.h"
#include "DOMPredicate.h"

class DOMInternalCondition;



/**
 * A Predicate assertion condition, of the form:
 *
 * <predicate name="name"/>
 *
 * 
 */
class DOMPredicateCondition : public DOMCondition
{
	private:
		DOMPredicate* predicate;
		
	public:
	
		/**
		 * Create a DOMPredicateCondition over the
		 * underlying DOMElement node.
		 *
		 * @param node, the DOMElement representing the
		 * predicate condition.
		 * @param predicate, the predicate this condition is
		 * defined over.
		 */
		DOMPredicateCondition (DOMElement *node) : DOMCondition (node)
		{
			this->parent = NULL;
			this->predicate = NULL;
		}
		
		DOMPredicateCondition (DOMPredicate* predicate, DOMDocument* doc) : DOMCondition (doc->createElement (XMLString::transcode ("predicate")))
		{
	
			this->parent = NULL;
			this->predicate = predicate;
			setAttribute ("name", predicate->getName());
		}
		
		virtual ~DOMPredicateCondition () 
		{
			if(this->node->getParentNode()) {
				this->node->getParentNode()->removeChild (node);
			}
			node->release();
		}
		
		/**
		 * Visit this Predicate condition with an application specific 
		 * visitor.
		 */
		 
		 virtual bool visit (DOMConditionVisitor* visitor) {
			return visitor->visitPredicate (this);
		 }
		 
		
		/**
		 * Returns the operator name, for a Predicate 
		 * condition this is "predicate".
		 *
		 * @return "predicate".
		 */
		char* getOperatorName()
		{
			return getTagName ();
		}
		
		void setPredicate (DOMPredicate& predicate)
		{
			setAttribute ("name", predicate.getName ());
			this->predicate = &predicate;
		}
		
		DOMPredicate* getPredicate ()
		{
			return this->predicate;
		}
		
		char* getValue ()
		{
			return getAttribute ("name");
		}
		/**
		 * @return false.
		 */
		bool isInternal()
		{
			return false;
		}
};
#endif
