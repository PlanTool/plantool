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
