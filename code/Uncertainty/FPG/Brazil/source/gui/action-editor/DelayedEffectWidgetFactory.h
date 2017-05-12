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

#ifndef delayed_effect_widget_factory
#define delayed_effect_widget_factory

#include <map>
#include <QString>
#include <QStringList>
#include <QWidget>

#include "DelayedEffectWidget.h"
#include "BrazilController.h"

#include "../../model/impl/DOMEffectSet.h"

class DelayedEffectWidgetFactory
{
	private:
		int height;

		map<QString, DOMDuration*> defaultDurations;
		DOMDomain* domain;
		BrazilController* controller;
		QStringList* durationNames;
	
	protected:
		
		void createDefaultDurations (DOMDomain& domain) {
			DOMDocument* doc = domain.getDOMElement ()->getOwnerDocument ();
			
			//Delete old defaultDurations.
			
			DOMFixedDuration* defaultFixed = new DOMFixedDuration(36000, doc);
			DOMNormalDuration* defaultNormal = new DOMNormalDuration(36000,3600,doc);
			DOMUniformDuration* defaultUniform = new DOMUniformDuration (18000, 36000, doc);
			DOMExponentialDuration* defaultExponential = new DOMExponentialDuration(0.0001, doc);
	
			defaultDurations [defaultFixed->getName()] = defaultFixed;
			defaultDurations [defaultNormal->getName()] = defaultNormal;
			defaultDurations [defaultUniform->getName()] = defaultUniform;
			defaultDurations [defaultExponential->getName()] = defaultExponential;
		}
		
	public:
	
		DelayedEffectWidgetFactory (int height,
			DOMDomain* domain, BrazilController* controller, 
			QStringList* durationNames)
		{
			this->height = height;
			createDefaultDurations (*domain);
			this->domain = domain;
			this->controller = controller;
			this->durationNames = durationNames;
		}
		
				
		virtual ~DelayedEffectWidgetFactory () { }
		
		virtual DelayedEffectWidget* createDelayedEffectWidget
		    (DOMEffectSet* atStartEffect, DOMEffectSet* effect, QWidget* parent, QColor backgroundColour, double scale)
		{
			DelayedEffectWidget* widget = new DelayedEffectWidget 
			    (parent, atStartEffect, effect, controller, defaultDurations, backgroundColour, 
				height, scale, domain);
			widget->setDurationNames (*durationNames);
			return widget;
		}
		
		virtual void setDomain (DOMDomain& domain) {
			this->domain = &domain;
			createDefaultDurations (domain);
		}
};

#endif

