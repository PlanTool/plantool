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
 *  DelayedEffectWidgetFactory.h
 *  
 *
 *  Created by Owen Thomas on 26/05/06.
 *  
 *
 */

#ifndef action_effect_widget_factory
#define action_effect_widget_factory
#include <map>
#include <QString>
#include <QStringList>
#include "../../model/impl/DOMDomain.h"
#include "BrazilController.h"
#include "../../model/impl/DOMDuration.h"
#include "ActionEffectWidget.h"

#include "DelayedEffectWidgetFactory.h"



class ActionEffectWidgetFactory
{
	private:
		int height;
		BrazilController* controller;
		DelayedEffectWidgetFactory* effectWidgetFactory;
		
	public:
		ActionEffectWidgetFactory (int height, BrazilController* controller, 
		DelayedEffectWidgetFactory* effectWidgetFactory)
		{
			this->height = height;
			this->controller = controller;
			this->effectWidgetFactory = effectWidgetFactory;
		}
		
		virtual ~ActionEffectWidgetFactory () { }
		virtual ActionEffectWidget* createActionEffectWidget (DOMAction* action, QWidget* parent, double scale)
		{
		//	DelayedEffectWidget* effectWidget = 
		//		effectWidgetFactory->createDelayedEffectWidget 
		//		(action->getEffectSet(),NULL, action->getBackgroundColour(), scale);
			
			ActionEffectWidget* actionEffectWidget
				= new ActionEffectWidget (parent, action, effectWidgetFactory, scale,  controller);
		
			return actionEffectWidget;
		}
		
		virtual void setDomain (DOMDomain& domain) {
			this->effectWidgetFactory->setDomain (domain);
		}
};

#endif

