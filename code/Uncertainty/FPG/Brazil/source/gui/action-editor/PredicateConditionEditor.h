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
 *  PredicateConditionEditor.h
 *  
 *
 *  Created by Owen Thomas on 29/11/06.
 *  
 *
 */
 
#ifndef predicate_condition_editor
#define predicate_condition_editor

#include "../../model/impl/DOMPredicateCondition.h"

#include "ConditionEditor.h"
#include "PredicateComboBox.h"

#include <QPushButton>

class PredicateConditionEditor : public ConditionEditor {
	Q_OBJECT
	private:
		PredicateComboBox* predicateComboBox;
	protected slots:
		
		//activated in response to negated button 
		virtual void negateCondition (bool negated) {
			
			condition->setNegated (negated);
		}
		
		//activated in response to combo box selection
		//change.
		virtual void setPredicate () {
			DOMPredicate* predicate = predicateComboBox->getCurrentPredicate ();
			if(predicate) {
				((DOMPredicateCondition*)condition)->setPredicate (*predicate);
			}
		}
				
	public:
		
		PredicateConditionEditor (DOMPredicateCondition& condition,
			DOMDomain& domain,
			PredicateComboBox* predicateComboBox,
			QWidget* parent =  NULL);
		
		QSize sizeHint () const {
			return minimumSize ();
		}
};

#endif
