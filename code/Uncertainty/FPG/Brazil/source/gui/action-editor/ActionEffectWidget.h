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
 *  ActionEffectWidget.h
 */  

#ifndef action_effect_widget
#define action_effect_widget

#include <QColor>
#include <QWidget>
#include <map>
#include <QString>
#include "BrazilController.h"
#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMAction.h"
#include "../../model/impl/DOMOutcome.h"

#include "../../model/impl/DOMDuration.h"

#include "OutcomeWidget.h"
#include "../../model/ActionListener.h"

/**
 *  Displays the deterministic effect for a Brazil Action.
 * 
 *  That is, a labeled DelayedEffectWidget, where the label maps
 *  to the name of its Brazil Action. 
 *
 *  Created by Owen Thomas on 26/03/06.
 *  
 *
 */
class ActionEffectWidget : public QWidget, public ActionListener
{
	Q_OBJECT
	private:
	
		int barWidth;
		int numDurationUnitsPerBar;
		
		//The action represented by this ActionEffectWidget.
		DOMAction* action;
		
		//The Widget responsible for drawing the duration of the
		//action's deterministic effect and its effect.
		DelayedEffectWidget* effect;
		
		//The name of the action.
		InvisibleLineEdit* nameField;
		
		BrazilController* controller;
		
	protected slots:
		void setActiveModel ()
		{
			controller->setActiveModel (action);
		}
	public slots:
	
	    /**
		 * Indicates that the name within the 
		 * name line edit has changed.
		 */
		void changeName ();
		
		/**
		 * Indicates that the size of the underlying
		 * DelayedEffectWidget has changed and that
		 * this should resize.
		 */
		void resizeActionEffect ();
		
		
	signals:
		/**
		 * Emitted when this has been resized.
		 */
		void resizing (ActionEffectWidget* widget);
		
	public:
	
		/**
		 * Creates an ActionEffectWidget with the specified 
		 * widget as a parent, that manipulates the specified DOMAction
		 * and with the specified default duration values.
		 *
		 * This creates a DelayedEffectWidget responsible for displaying the duration,
		 * and effects of the action - passing to it the default durations.
		 * 
		 */
		ActionEffectWidget (QWidget* parent, DOMAction* action, 
			DelayedEffectWidgetFactory* effectFactory,
			double scale,
			BrazilController* controller);
			
		~ActionEffectWidget ();
		
		DelayedEffectWidget* getEffectWidget ()
		{
			return effect;
		}
		
		DOMAction* getAction ()
		{
			return action;
		}
		
		void setScale (double scale) {
			effect->setScale (scale);
		}
		
		double getScale () {
			return effect->getScale ();
		}

		void setHeight (int height)
		{
			effect->setHeight (height);
		}
		
		/**
		 * This method exists because the width() of the action
		 * effect may be different to the width of its duration
		 * if the duration is very short, because then the width
		 * of this action effect will be the width of the label +
		 * the width of the drop down are larger.
		 *
		 * Any probabilistic outcomes should be positioned
		 * after the width of the duration, not the width of
		 * the entire effect area.
		 */
		int getDurationWidth ()
		{
			return effect->getDurationWidth ();
		}
		
		//ActionListener method
		void nameChanged ();
		void backgroundColourChanged ();
		void probabilisticAdded (DOMProbabilistic*) { }
		void probabilisticRemoved (DOMProbabilistic*) { }
		void focusInEvent(QFocusEvent*) { effect->setFocus (); }
		
		void setContextMenu (QMenu* menu)
		{
			effect->setContextMenu (menu);
		}
};

#endif

