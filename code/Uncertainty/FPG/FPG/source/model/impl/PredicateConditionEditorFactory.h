/*
 *  PredicateConditionEditorFactory.h
 *  
 *
 *  Created by Owen Thomas on 11/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "BrazilController.h"
#include "PredicateConditionEditor.h"

class PredicateConditionEditorFactory
{
	private:
		NewPredicateConditionAction* npca;
		NewConditionGroupAction* ncga;
		
		
	public:
		PredicateConditionEditorFactory (NewPredicateConditionAction* npca, 
			NewConditionGroupAction* ncga)
		{
			this->npca = npca;
			this->ncga = ncga;
		}
		
		virtual PredicateConditionEditor* createPredicateConditionEditor (DOMPredicateCondition* condition,
			DOMDomain* domain, BrazilController* controller, QWidget* parent, bool enableDeleteButton = true, 
			bool alternatingBase = false)
			
		{
			return new PredicateConditionEditor (condition, domain, controller, parent, npca, ncga, enableDeleteButton,
				alternatingBase);
		}
};
