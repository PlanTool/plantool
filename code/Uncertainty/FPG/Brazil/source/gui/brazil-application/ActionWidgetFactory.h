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
 *  ActionWidgetFactory.h
 *  
 *
 *  Created by Owen Thomas on 7/06/06.
 *  
 *
 */
#ifndef action_widget_factory
#define action_widget_factory

#include <map>
#include <QString>
#include <QStringList>
#include "../../model/impl/DOMDomain.h"
#include "BrazilController.h"
#include "../../model/impl/DOMDuration.h"
#include "ActionEffectWidget.h"
#include "ActionEffectWidgetFactory.h"

class ActionWidgetFactory 
{
	private:
		int outcomeSpacing;
		int barWidth;
		int numDurationUnitsPerBar;
		OutcomeWidgetFactory* owf;
		ActionEffectWidgetFactory* aewf;
		BrazilController* controller;
		
	public:
		ActionWidgetFactory (int outcomeSpacing, int barWidth, int numDurationUnitsPerBar,
			OutcomeWidgetFactory* owf, ActionEffectWidgetFactory* aewf,
			BrazilController* controller)
			
		{
			this->outcomeSpacing = outcomeSpacing;
			this->barWidth = barWidth;
			this->numDurationUnitsPerBar = numDurationUnitsPerBar;
			this->owf = owf;
			this->aewf = aewf;
			this->controller = controller;
		}
		
		virtual ~ActionWidgetFactory () { } 
		
		virtual ActionWidget* createActionWidget (DOMDomain* domain, DOMAction* action)
		{
			ActionEffectWidget* widget = aewf->createActionEffectWidget (action, NULL, (double) barWidth / numDurationUnitsPerBar);
			
			ActionWidget* actionWidget = new ActionWidget (NULL, 
				outcomeSpacing, barWidth, numDurationUnitsPerBar, domain, widget, owf, controller);
				
			return actionWidget;
		}
		
		virtual void setDomain (DOMDomain& domain) {
			this->aewf->setDomain (domain);
			//this->owf->setDomain (domain);
		}
};
#endif

