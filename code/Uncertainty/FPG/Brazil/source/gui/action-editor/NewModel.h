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
 *  NewModel.h
 *  
 *
 *  Created by Owen Thomas on 19/04/06.
 *  
 *
 */

#ifndef new_model
#define new_model

#include "../../model/impl/DOMAction.h"
#include <vector>
#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMPredicateEffect.h"
#include <list>
#include <QColor>
#include "ProbabilityController.h"
class NewModel
{
	public:
		virtual void callNew () = 0;
		virtual bool canCallNew () = 0;
		virtual void setModel (DOMWrapper* model) = 0;
		virtual DOMWrapper* getModel () = 0;
		virtual ~NewModel () { }
};
class NewDOMOutcome : public NewModel
{
	private:
		DOMElement* outcomePrototype;
		DOMProbabilistic* parent;
		ProbabilityController* probabilityController;
		std::vector<QColor> colours;
		unsigned int currentPosition;
		int newOutcomeCount;
		
	public:
		NewDOMOutcome (DOMElement& outcomePrototype,std::vector<QColor>& colours)
		{
			this->outcomePrototype = &outcomePrototype;
			this->parent = NULL;
			this->colours = colours;
			currentPosition = 0;
			newOutcomeCount = 1;
		}

		void setModel (DOMWrapper* wrapper)
		{
			if(strcmp( wrapper->getTypeName (), "DOMAction") == 0) {
				DOMAction* action = (DOMAction*) wrapper;
				if(!action->getProbabilisticChildren().size()) {
					DOMProbabilistic* probabilistic = new DOMProbabilistic (wrapper->getDOMElement()->getOwnerDocument(), ((DOMAction*)wrapper));
					((DOMAction*)wrapper)->addProbabilistic (probabilistic);
					parent = probabilistic;
				}
				else {
					parent = *action->getProbabilisticChildren().begin ();
				}
			}
			else if(strcmp (wrapper->getTypeName(), "DOMOutcome") == 0) {
				parent =  (DOMProbabilistic*)((DOMOutcome*)wrapper)->getProbabilistic ();
			}
			else {
				parent = NULL;
			}
			
			if(parent) probabilityController = new ProbabilityController (parent);
		} 
		
		DOMWrapper* getModel ()
		{
			return (DOMWrapper*) parent;
		}
		
		bool canCallNew () 
		{
			return parent != NULL;
		}
		void callNew ()
		{
			if(parent) {
				DOMElement* outcomeElement = (DOMElement*) parent->getDOMElement()->getOwnerDocument()->importNode (outcomePrototype,true);
				DOMOutcome* outcome = new DOMOutcome (outcomeElement, parent);
				
				cout << "Probability Controller: " << probabilityController << endl;
				probabilityController->addOutcome (outcome);
				
				if(colours.size() > 0) {
					outcome->setBackgroundColour (colours [(currentPosition ++) % colours.size()].name().toUtf8().data());
				}
				QString outcomeLabelNumber;
				outcomeLabelNumber.setNum (newOutcomeCount++);
				QString newLabel;
				newLabel.append (outcome->getLabel());
				newLabel.append (" ");
				newLabel.append(outcomeLabelNumber);
				outcome->setLabel (newLabel.toUtf8().data());
				
				
			}
		}
};


class NewDOMInternalCondition : public NewModel
{
	private:
		DOMInternalCondition* parent;
		DOMCondition* sibling;
	public:
		NewDOMInternalCondition ()
		{
			parent = NULL;
			sibling = NULL;
		}
		
		void setModel (DOMWrapper* model)
		{
			if(strcmp(model->getTypeName (), "DOMCondition") == 0) {
				if( ((DOMCondition*)model)->isInternal ()) {
					parent = (DOMInternalCondition*)model;
					sibling = NULL;
				}
				else {
					sibling = (DOMCondition*) model;
					parent = ((DOMCondition*)model)->parent;
				}
			}
		}
		
		DOMWrapper* getModel () 
		{
			return (DOMWrapper*) parent;
		}
		
		bool canCallNew ()
		{
			return parent != NULL;
		}
		
		void callNew ()
		{
			if(!canCallNew()) return;
			
			DOMInternalCondition* newAndCondition = parent->createAndCondition ();
			DOMPredicateCondition* newPredicateCondition = 
				new DOMPredicateCondition ((*parent->getDomain()->getPredicates().begin()), parent->getDOMElement()->getOwnerDocument());
				
			newAndCondition->addChild (newPredicateCondition);
			
			parent->addChild (sibling,newAndCondition);
		}
};

class NewDOMPredicateCondition : public NewModel
{
	private:
		DOMInternalCondition* parent;
		DOMCondition* sibling;
		bool addToRootCondition;
		
	public:
		NewDOMPredicateCondition ()
		{
			parent = NULL;
			this->sibling = NULL;
			addToRootCondition = false;
		}
		
		void setModel (DOMWrapper* model)
		{
			addToRootCondition = false;
			if(strcmp (model->getTypeName(), "DOMCondition") == 0) {
				if(((DOMCondition*)model)->isInternal()) {
					parent = (DOMInternalCondition*) model;
					sibling = NULL;
					
				}
				else {
					sibling = (DOMCondition*)model;
					parent = ((DOMCondition*) model)->parent;
				}
				if(parent->parent == NULL) addToRootCondition = true;
			} else {
				parent = NULL;
				sibling = NULL;
			}
		}
		
		DOMWrapper* getModel () 
		{
			return (DOMWrapper*) parent;
		}
		
		bool canCallNew ()
		{
			if(parent != NULL) {
				DOMDomain* domain = parent->getDomain ();
				return domain->getPredicates().size();
			}
			return false;
		}
		
		void callNew ();
	
};

#endif

