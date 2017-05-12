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
 *  ConditionEditor.h
 *  
 *
 *  Created by Owen Thomas on 29/11/06.
 *  
 *
 */
 
#ifndef condition_editor
#define condition_editor

#include <QAction>
#include <QIcon>
#include <QFrame>
#include <QPushButton>
#include <QMenu>

class DOMCondition;
class DOMFunction;
class DOMPredicate;
class DOMAction;

#include "../../model/DomainListener.h"
#include "../../model/impl/DOMDomain.h"

#include <iostream>
using namespace std;
/**
 * Abstract base class of all condition editors.
 */
class ConditionEditor : public QFrame, public DomainListener {

	Q_OBJECT
	
	signals:
		
		void requestDelete (DOMCondition* condition);
		
		//To-Do: Tie these signals in with the actions in
		//the new button menu!
		void requestNewInternalCondition (ConditionEditor*);
		void requestNewFunctionCondition (ConditionEditor*);
		void requestNewPredicateCondition (ConditionEditor*);
		
	private:
		ConditionEditor (); //No default constructor.
	
	protected slots:
		
		virtual void handleDeleteButtonPressed () {
			emit requestDelete (this->condition);
		}

		virtual void handleNewConditionGroupPressed () {
			emit requestNewInternalCondition (this);
		}
		
		virtual void handleNewFunctionConditionPressed () {
			emit requestNewFunctionCondition (this);
		}
		
		virtual void handleNewPredicateConditionPressed () {
			emit requestNewPredicateCondition (this);
		}
		
	protected:
		
		DOMCondition* condition; //Condition cannot be NULL.
		
		DOMDomain* domain; //Domain condition is drawn from.
		
		//Each ConditionEditor has a new button and a delete button
		//that look identical. Created (but not positioned) in this 
		//constructor.
		QPushButton* deleteButton;
		QPushButton* newButton;
		
		QAction* newPredicateConditionAction;
		QAction* newFunctionConditionAction;
		
		ConditionEditor (DOMCondition& condition, DOMDomain& domain, 
			QWidget* parent = NULL) : QFrame (parent) {
			
			this->condition = &condition;
			this->domain = &domain;
			
			this->domain->addListener (this);
			
			//Create delete & new button.
			QIcon* minusIcon = new QIcon ("minus.png");
			deleteButton = new QPushButton ();
			deleteButton->setIcon (*minusIcon);
			deleteButton->setIconSize (QSize( 18,18));
			deleteButton->setMaximumWidth (21);
			deleteButton->setMaximumHeight (21);	
	
			deleteButton->hide (); //Shown by subclass when positioned correctly.
			
			QIcon* plusIcon = new QIcon ("plus.png");
			newButton = new QPushButton ();
			newButton->setIcon (*plusIcon);
			newButton->setIconSize (QSize (18,18));
			newButton->setMaximumSize (QSize (35,21));
			
			QMenu* newButtonMenu = new QMenu ();
			QAction* newConditionGroupAction = 
				newButtonMenu->addAction ("New Condition Group");
			newPredicateConditionAction = 
				newButtonMenu->addAction ("New Predicate Condition");
				
			newFunctionConditionAction =
				newButtonMenu->addAction ("New Function Condition");
			
			newPredicateConditionAction->setEnabled (domain.getPredicates().size() > 0);
			newFunctionConditionAction->setEnabled (domain.getFunctions().size() > 0);

			
			newButton->setMenu (newButtonMenu);
			
			newButton->hide ();
			
			QObject::connect (deleteButton, SIGNAL (pressed ()),
				this, SLOT (handleDeleteButtonPressed ()));
				
			QObject::connect (newConditionGroupAction, SIGNAL (triggered (bool)),
				this, SLOT (handleNewConditionGroupPressed()));
				
			QObject::connect (newPredicateConditionAction, SIGNAL (triggered (bool)),
				this, SLOT (handleNewPredicateConditionPressed ()));
				
			QObject::connect (newFunctionConditionAction, SIGNAL (triggered (bool)),
				this, SLOT (handleNewFunctionConditionPressed ()));
		}
		
	public:
		
		virtual ~ConditionEditor () {
			this->domain->removeListener (this);
		}
		virtual DOMCondition* getDOMCondition () {
			return condition;
		}
		
		virtual void hideDeleteButton () {
			deleteButton->hide ();
		}
		
		virtual void functionAdded (DOMFunction*) {
			newFunctionConditionAction->setEnabled (true);
			newFunctionConditionAction->setVisible (false);
			newFunctionConditionAction->setVisible (true);
		}
		
		
		virtual void predicateAdded (DOMPredicate*) {
			newPredicateConditionAction->setEnabled (true);
		}
		
		virtual void actionAdded (DOMAction*) {}
		virtual void actionRemoved (DOMAction*) {}
		virtual void nameChanged (const char*) {}
};

#endif
