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
 *  ResultBar.h
 *  
 *
 *  Created by Owen Thomas on 3/08/06.
 *  
 *
 */
#ifndef initial_result_bar
#define initial_result_bar

#include <QWidget>
#include <QHBoxLayout>
#include <QPushButton>

#include "ResultBar.h"

#include "../../model/impl/DOMProblem.h"
#include "../../model/impl/DOMDomain.h"
#include "../../model/DomainListener.h"
#include "../../model/InternalConditionListener.h"

/**
 * Displayed in the main Brazil Application window, allows initiation
 * of planning. Assumes that no results have been generated for
 * its domain.
 */
 
class InitialResultBar : public ResultBar, public DomainListener,
	public InternalConditionListener {
	Q_OBJECT
	
	signals:
		void plan ();
		void pause ();
	
	private:
		DOMDomain* domain;
		QHBoxLayout* layout;
		QPushButton* planButton;
		
		bool planning;
	
	private slots:
		void callPlan () {
			
			if(!planning) emit plan ();
			else emit pause ();
			
			planning = !planning;

			if(planning) {
				planButton->setText ("Pause");
			}
			else planButton->setText ("Plan");

		}
	
		
	public:
		InitialResultBar (DOMDomain& domain, QWidget* parent = NULL) 
			: ResultBar (parent) {
			
			this->domain = &domain;
			this->domain->addListener (this);
			
			//Listen on the goal condition.
			this->domain->getProblem()->getGoal ()->addListener (this);
			
			layout = new QHBoxLayout ();
			setLayout (layout);
			
			planButton = new QPushButton ("Plan");
			
			QObject::connect (planButton, SIGNAL (clicked(bool)),
				this, SLOT (callPlan ()));
			
			layout->addStretch (10);
			layout->addWidget (planButton);
			
			planButton->setEnabled (this->domain->getActions().size () &&
				this->domain->getProblem ()->getGoal ()->getChildren ().size ());
				
			planning = false;
			
		}
		
		virtual ~InitialResultBar () {
			this->domain->getProblem()->getGoal()->removeListener (this);
			this->domain->removeListener (this);
		}
		
		void setDomain (DOMDomain& domain) {
			this->domain->removeListener (this);
			this->domain->getProblem()->getGoal()->removeListener (this);

			
			this->domain = &domain;
			this->domain->getProblem()->getGoal ()->addListener (this);
			this->domain = &domain;

			planButton->setEnabled (this->domain->getActions().size() 
				&& this->domain->getProblem()->getGoal()->getChildren().size());

		}
		
		//
		//DomainListener impl.
		//
		//The ability to plan depends on the number of
		//actions, hence these are the only methods 
		//implemented.
		
		virtual void predicateAdded(DOMPredicate*) { }
		virtual void functionAdded (DOMFunction*) {}
		virtual void nameChanged (const char*) { }
		
		virtual void actionAdded (DOMAction*) {
			planButton->setEnabled (this->domain->getActions().size() 
				&& domain->getProblem()->getGoal()->getChildren().size());
				
		}
		
		virtual void actionRemoved (DOMAction*) {
			planButton->setEnabled (this->domain->getActions().size() 
				&& domain->getProblem()->getGoal()->getChildren().size());
		}
		
		//
		//InternalConditionListener impl
		//
		//Listening on the Problem goal condition which is
		//a root condition and can have only one child.
		//When this is child is defined planning is possible
		//When not, planning is impossible.
		//
		virtual void conditionAdded (DOMCondition*) {
			planButton->setEnabled (this->domain->getActions().size() 
				&& domain->getProblem()->getGoal()->getChildren().size());
		}
		
		virtual void conditionRemoved (DOMCondition*) {
			planButton->setEnabled (this->domain->getActions().size() 
				&& domain->getProblem()->getGoal()->getChildren().size());
		}
		
		virtual void setNegated (bool) {
		}
};

#endif

