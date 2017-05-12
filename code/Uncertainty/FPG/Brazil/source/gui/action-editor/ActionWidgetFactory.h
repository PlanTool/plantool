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
#include "ActionWidget.h"
#include "ActionEffectWidget.h"
#include "ActionEffectWidgetFactory.h"
#include "OutcomeWidgetFactory.h"

class ActionWidgetFactory 
{
	private:
		int outcomeSpacing;
		int numDurationUnitsPerBar;
		int barWidth;
		OutcomeWidgetFactory* owf;
		ActionEffectWidgetFactory* aewf;
		BrazilController* controller;
		QMenu* outcomeContextMenu;
		QMenu* actionEffectContextMenu;
		
	public:
		ActionWidgetFactory (int outcomeSpacing, int barWidth, int numDurationUnitsPerBar,
			OutcomeWidgetFactory* owf, ActionEffectWidgetFactory* aewf,
			BrazilController* controller, 
			
			QMenu* outcomeContextMenu, QMenu* actionEffectContextMenu)
			
		{
			this->outcomeSpacing = outcomeSpacing;
			this->numDurationUnitsPerBar = numDurationUnitsPerBar;
			this->barWidth = barWidth;
			this->owf = owf;
			this->aewf = aewf;
			this->controller = controller;
			
			this->outcomeContextMenu = outcomeContextMenu;
			this->actionEffectContextMenu = actionEffectContextMenu;
		}
		
		virtual ~ActionWidgetFactory () { } 
		
		virtual ActionWidget* createActionWidget (DOMDomain* domain, DOMAction* action)
		{
			ActionEffectWidget* widget = aewf->createActionEffectWidget (action, NULL, (double) barWidth / numDurationUnitsPerBar);
			ActionWidget* actionWidget = new ActionWidget (NULL, 
				action, outcomeSpacing, barWidth, numDurationUnitsPerBar, domain, widget, owf, controller);
			actionWidget->setBackgroundRole (QPalette::Base);
			actionWidget->setSizePolicy (QSizePolicy::Ignored, QSizePolicy::Ignored);
			
			actionWidget->setAlternatingBackgroundColours (QColor ("#EEEEEE"), QColor ("#DDDDDD"));
			actionWidget->setHotAreaColour (QColor ("#FFE6F9"));
			actionWidget->setHotAreaBorderColour (QColor ("#7A005C"));
			actionWidget->setOutcomeContextMenu (outcomeContextMenu);
			actionWidget->setActionEffectContextMenu (actionEffectContextMenu);
			
			return actionWidget;
		}
		
		virtual void setDomain (DOMDomain& domain) {
			owf->setDomain (domain);
			aewf->setDomain (domain);
		}
};
#endif

