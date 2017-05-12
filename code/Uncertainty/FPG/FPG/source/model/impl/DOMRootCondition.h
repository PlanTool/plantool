/*
 *  DOMActionCondition.h
 *  
 *
 *  Created by Owen Thomas on 11/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
