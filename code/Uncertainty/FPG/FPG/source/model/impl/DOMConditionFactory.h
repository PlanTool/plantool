/*
 *  DOMConditionFactory.h
 *  
 *
 *  Created by Owen Thomas on 10/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_condition_factory
#define dom_condition_factory

class DOMDomain;

#include "DOMConditionBase.h"
#include "DOMPredicateCondition.h"
#include "DOMInternalCondition.h"
#include "DOMFunctionCondition.h"
/**
 * Provides a factory method for creating a concrete DOMCondition
 * from a DOMElement.
 *
 */
class DOMConditionFactory
{
		public:
		/**
		 * A Static Factory method for creating the appropriate
		 * DOMCondition subclass for the input DOMElement representing
		 * a condition.
		 *
		 *
		 * @param element, the DOMElement representing the boolean 
		 * condition tree.
		 */
		static DOMCondition* createCondition(DOMElement* element, DOMDomain& domain);
		
		
		
		/**
		 * Returns true if the DOMElement represents a Condition that
		 * DOMCondition recognises.
		 *
		 * @param element, a DOMElemenet to be tested.
		 *
		 * @return true if element is a boolean condition.
		 */
		static bool isCondition (DOMElement* element)
		{
		static XMLCh* predicate = XMLString::transcode ("predicate");
		static XMLCh* conditionString = XMLString::transcode ("condition");
		static XMLCh* functionString = XMLString::transcode ("functionCondition");

			 return (0 == XMLString::compareIString (conditionString, element->getNodeName()) 
				|| 0 == XMLString::compareIString (predicate, element->getNodeName())
				|| 0 == XMLString::compareIString (functionString, element->getNodeName()) );
				
			
		}

};



#endif
