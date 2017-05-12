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
 *  DOMConditionFactory.h
 *  
 *
 *  Created by Owen Thomas on 10/03/06.
 *  
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
