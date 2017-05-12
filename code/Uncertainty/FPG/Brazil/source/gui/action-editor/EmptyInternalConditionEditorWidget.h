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
 *  EmptyInternalConditionEditorWidget.h
 *  
 *
 *  Created by Owen Thomas on 30/11/06.
 *  
 *
 */

/**
 * Widget displayed when an internal condition is created with
 * no child conditions. Allows addition of a predicate or
 * function condition.
 */
 
#include <QWidget>
#include <QPushButton>

#include "../../model/impl/DOMDomain.h"
#include "../../model/DomainListener.h"

class DOMInternalCondition;
class PredicateComboBox;
class FunctionComboBox;

class EmptyInternalConditionEditorWidget : public QWidget, DomainListener {

	Q_OBJECT
	
	private:
		DOMInternalCondition* internalCondition;
		DOMDomain* domain;
		
		QPushButton* predicateAddButton;
		QPushButton* functionAddButton;
		
		PredicateComboBox* predicateComboBox;
		FunctionComboBox* functionComboBox;
		
	protected slots:
		
		void addPredicateCondition ();
		void addFunctionCondition ();
		
	public:
		EmptyInternalConditionEditorWidget (DOMInternalCondition& internalCondition,
			DOMDomain& domain, QWidget* parent = NULL);
		
		virtual ~EmptyInternalConditionEditorWidget () {
			this->domain->removeListener (this);
		}
		//Domain listener
		virtual void predicateAdded (DOMPredicate*) {
			predicateAddButton->setEnabled (true);
		}
				
		virtual void functionAdded (DOMFunction*) {
			functionAddButton->setEnabled (true);
		}
				
			virtual void actionAdded (DOMAction*) { }
			virtual void actionRemoved (DOMAction*) { }
			virtual void nameChanged (const char*) { }	
};
