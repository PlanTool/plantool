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
 *  PredicateConditionEditorFactory.h
 *  
 *
 *  Created by Owen Thomas on 11/07/06.
 *  
 *
 */

#ifndef predicate_condition_editor_factory
#define predicate_condition_editor_factory

#include "BrazilController.h"
#include "PredicateConditionEditor.h"
class PredicateConditionEditorFactory
{
	private:
		NewPredicateConditionAction* npca;
		NewConditionGroupAction* ncga;
		DeleteConditionAction* dca;
		
	public:
		PredicateConditionEditorFactory (NewPredicateConditionAction* npca, 
			NewConditionGroupAction* ncga, DeleteConditionAction* dca)
		{
			this->npca = npca;
			this->ncga = ncga;
			this->dca = dca;
		}
		
		virtual ~PredicateConditionEditorFactory() {};

		virtual PredicateConditionEditor* createPredicateConditionEditor (DOMPredicateCondition* condition,
			DOMDomain* domain, BrazilController* controller, QWidget* parent, bool enableDeleteButton = true)
			
		{
			return NULL;
			//return new PredicateConditionEditor (condition, domain, controller, parent, npca, ncga, dca, enableDeleteButton);
		}
		/*
		virtual InitialPredicateConditionEditor* createInitialPredicateConditionEditor
			(DOMInternalCondition* parentCondition, DOMDomain* domain, BrazilController* controller, QWidget* parent) {
			return NULL;
			//return new InitialPredicateConditionEditor (parentCondition, domain, controller, npca, ncga, dca, parent);
		}*/
		
		virtual DeleteConditionAction* getDeleteConditionAction () {
			return dca;
		}
		
		
};

#endif

