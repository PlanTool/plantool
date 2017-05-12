/*
 *  NewDOMPredicateCondition.cpp
 *  
 *
 *  Created by Owen Thomas on 9/10/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "NewModel.h"

void NewDOMPredicateCondition::callNew () {
	
					
		DOMPredicateCondition* newPredicateCondition = 
			new DOMPredicateCondition((*(parent->getDomain()->getPredicates().begin())), 
			parent->getDOMElement()->getOwnerDocument());
						
			parent->addChild (sibling, newPredicateCondition);
		
}
