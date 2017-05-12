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
 *  OutcomeWidgetFactory.h
 *  
 *
 *  Created by Owen Thomas on 26/05/06.
 *  
 *
 */
#ifndef outcome_widget_factory
#define outcome_widget_factory

#include "../../model/impl/DOMDuration.h"
#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMEffectSet.h"

#include "BrazilController.h"
#include "OutcomeWidget.h"
#include "DelayedEffectWidgetFactory.h"

class OutcomeWidgetFactory
{
	private:
		int height;
		BrazilController* controller;
		DelayedEffectWidgetFactory* effectWidgetFactory;
		
	public:
		OutcomeWidgetFactory (int height, BrazilController* controller, 
		DelayedEffectWidgetFactory* effectWidgetFactory)
		{
			this->height = height;
			this->controller = controller;
			this->effectWidgetFactory = effectWidgetFactory;
		}
		
		virtual ~OutcomeWidgetFactory () { }
			
		virtual OutcomeWidget* createOutcomeWidget (DOMOutcome* outcome, 
		QWidget* parent, double scale)
		{
				OutcomeWidget* outcomeWidget = 
				new OutcomeWidget (parent, outcome, effectWidgetFactory,scale, controller);
			
			return outcomeWidget;
		}
		
		virtual void setDomain (DOMDomain& domain) {
			this->effectWidgetFactory->setDomain (domain);
		}
		
		/**
		protected:
		 virtual ActionDelegate* createNewDelegate ();
		 
		 virtual ActionDelegate* createDeleteDelegate ();
		 
		 
		 */
};

#endif

