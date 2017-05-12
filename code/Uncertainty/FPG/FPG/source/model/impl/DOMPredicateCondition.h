/*
 *  DOMPredicateCondition.h
 *  
 *
 *  Created by Owen Thomas on 10/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
