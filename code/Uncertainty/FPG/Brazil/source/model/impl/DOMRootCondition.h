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
 *  DOMActionCondition.h
 *  
 *
 *  Created by Owen Thomas on 11/05/06.
 *  
 *
 */

#ifndef dom_action_condition
#define dom_action_condition

#include "DOMConditionBase.h"
#include "DOMInternalCondition.h"

class DOMDomain;

/**
 * A DOMRootCondition represents a root condition in
 * an action, for example a precondition or an overall
 * element.
 *
 * It's assuemd that a DOMActionCondition has one child,
 * which is potentially NULL, defining the condition this
 * action condition represents.
 */
class DOMRootCondition : public DOMInternalCondition
{
	
				
	public:
		
		/**
		 * Construct a DOMActionCondition with the specified parent
		 * action and DOMElement node defining the action condition.
		 */
		DOMRootCondition (DOMElement* node, DOMDomain& domain) 
			: DOMInternalCondition (node, domain)
		{
			
		}
		
		~DOMRootCondition () 
		{
		}
		
		virtual bool isConjunctive () {
			//For root condition (e.g. goal) if there is no conjunctive
			if(!this->node->hasAttribute (typeString)) return true;
			else return XMLString::equals (andString,this->node->getAttribute (typeString) );
		}
		
};

#endif
