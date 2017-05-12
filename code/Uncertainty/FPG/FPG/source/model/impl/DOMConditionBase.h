/*
 *  DOMConditionBase.h
 *  
 *
 *  Created by Owen Thomas on 10/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef inc_dom_condition_base
#define inc_dom_condition_base


#include<list>
#include "DOMWrapper.h"
#include "DOMConditionVisitor.h"

class DOMInternalCondition;

class DOMPredicateCondition;

class DOMDomain;

/**
 * A boolean Condition.
 *
 * Currently we consider only conditions over predicates, that is
 * and, or, not and predicate assertion.
 *
 * A Condition can either be internal and have children: and, or, not. Or
 * be a base condition and represent a predicate condition.
 *
 * These are defined through subclasses of this class: DOMInternalCondition
 * and DOMPredicateCondition.
 *
 * When we extend to function expressions we will add a new class,
 * DOMFunctionCondition to implement this. 
 *
 */
class DOMCondition : public DOMWrapper
{
	private:
		XMLCh* trueString;
		XMLCh* falseString;
		XMLCh* negatedString;

	

	public:

		/** 
		 * Shortcut types for lists over DOMConditions
                 */
		typedef list<DOMCondition*> ConditionList;
		typedef ConditionList::iterator ConditionIt;
		typedef ConditionList::const_iterator ConditionCIt;

		/**
		 * The (potentially NULL) parent internal condition
		 * of this Condition.
		 */
		DOMInternalCondition* parent;
		
		
		DOMCondition (DOMElement* node)
		{
			trueString = XMLString::transcode ("true");
			falseString = XMLString::transcode ("false");
			negatedString = XMLString::transcode ("negated");
			
			this->parent = NULL;
			this->node = node;
		}
		
		virtual ~DOMCondition () 
		{
			XMLString::release (&trueString);
			XMLString::release (&negatedString);
			XMLString::release (&falseString);
		} 
		
		/**
		 * The name of the operator, currently one of
		 * and, or, not or predicate.
		 *
		 * @return The name of the condition operator.
		 */ 
		virtual char* getOperatorName() = 0;
		
		/**
		 * The value of the Condition, this does not
		 * refer to the state of the condition in some
		 * evaluation - but to a display value for the 
		 * the condition. For instance the value of a 
		 * predicate would be the name of the predicate.
		 *
		 * @return the display value for the Condition.
		 */
		virtual char* getValue() = 0;
		
		/**
		 * Indicates if this is an internal condition, with children,
		 * or not.
		 *
		 * @return true if this Condition has children.
		 */
		virtual bool isInternal() = 0;

		void setNegated (bool value) 
		{
			if(value) {
				this->node->setAttribute (negatedString, trueString);
			} else {
				this->node->setAttribute (negatedString, falseString);
			}
		}
		
		bool isNegated ()
		{
			const XMLCh* negatedValue = this->node->getAttribute (negatedString);
			if(!negatedValue) return false;
			bool returnValue = XMLString::equals (negatedValue, trueString);
			return returnValue;
		}
		
		/**
		 * Visit this condition. Subclasses define an implementation,
		 * calling the appropriate method in DOMConditionVisitor.
		 *
		 * The concrete implementation of this abstract class will then
		 * call the appropriate method on visitor, corresponding to its
		 * subtype.
		 *
		 * @param visitor - the DOMConditionVisitor implementation providing
		 * application specific behaviour.
		 * @return can optionally return a bool to indicate success of operation.
		 */
		virtual bool visit (DOMConditionVisitor* visitor) = 0;
		
		char* getTypeName () { return  "DOMCondition";}
		
		
};
#endif
