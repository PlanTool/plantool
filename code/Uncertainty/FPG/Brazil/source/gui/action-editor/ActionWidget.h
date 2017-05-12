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
 *  ActionWidget.h
 *  Created by Owen Thomas on 26/03/06.
 *  
 */

#ifndef action_widget
#define action_widget


#include "OutcomeWidget.h"
#include "ActionEffectWidget.h"
#include "OutcomeWidgetFactory.h"

#include "../../model/impl/DOMOutcome.h"
#include "../../model/impl/DOMAction.h"
#include "../../model/impl/DOMDomain.h"


#include "../../model/ProbabilisticListener.h"
#include "../../model/impl/DOMDuration.h"
#include "../../model/ActionListener.h"

#include <QString>
#include <QStringList>
#include <QPaintEvent>
#include <QBrush>
#include <QWidget>

#include <map>
#include <list>

class InternalConditionEditor;

/** 
 * Displays a Brazil Action.
 *
 *  A Brazil Action is represented as a single ActionEffectWidget
 *  for display and manipulation of the Action's deterministic 
 *  effect. Together with a collection of OutcomeWidgets, for display
 *  and manipulation of the Action's single probabilistic collection of
 *  Outcomes.
 *
 *  The ActionEffectWidget is displayed in the top left hand corner. 
 *  The OutcomeWidgets are stacked vertically, benath and to the right of
 *  the AcitonEffectWidget.
 *
 *
 */

class ActionWidget : public QWidget, public ProbabilisticListener, public ActionListener
{
	Q_OBJECT
	
	private:	
		//The action this widget manipulates.
		DOMAction* action;
		
		ActionEffectWidget* actionEffect;
		list<OutcomeWidget*> outcomes;
		
		//Default DOMDuration values for the ActionEffectWidget
		//and each Outcome.
		map<QString, DOMDuration*>defaultDurations;
		
		//Standard DOMDuration names.
		QStringList durationNames;
		
		//Lookups from DOMOutcomes to their widgets and
		//vice versa.
		map<DOMOutcome*, OutcomeWidget*> outcomeWidget;
		map<OutcomeWidget*, DOMOutcome*> widgetOutcome;
		
		//Whether we show the striped background or not.
		bool showBackground;
		
		QBrush backgroundStripeA;
		QBrush backgroundStripeB;
		QColor backgroundStripeAColour;
		QColor backgroundStripeBColour;
		
		QPen backgroundStripePen;
		
		//The number of pixels to place between each OutcomeWidget
		int outcomeSpacing;
		
		//The number of duration units (seconds) per vertical bar.
		int numDurationUnitsPerBar;
		
		int barWidth;
				
		QColor hotAreaColour;
		QColor hotAreaBorderColour;
		
		//Display names for effect operators.
		map<QString, QString> operatorDisplayNames;
		
		DOMDomain* domain;
		
		BrazilController* controller;
		
		QMenu* outcomeContextMenu;
		
		int leftMargin;
		
		QPushButton* displayConditionEditorWidget;
		
		list<QColor> backgroundColours;
				
		OutcomeWidgetFactory* outcomeWidgetFactory;
		
		DOMProbabilistic* probabilisticListeningOn;
		
		//Condition Editor for precondition
		InternalConditionEditor* preconditionEditor;
		
	protected:
		
		/**
		 * Create an OutcomeWidget for the specified
		 * DOMOutcome and add it to the display of
		 * OutcomeWidgets.
		 */
		void addOutcomeWidget (DOMOutcome* outcome);
		
		/**
		 * Remove the OutcomeWidget for the specified
		 * DOMOutcome from the display of OutcomeWidgets.
		 * The associated OutcomeWidget is deleted.
		 * If no OutcomeWidget is associated with the 
		 * DOMOutcome, in this ActionWidget, nothing
		 * happens.
		 */
		void removeOutcomeWidget (DOMOutcome* outcome);
		
		
	public slots:
	    /**
		 * The width of the AcitonEffectWidget has changed,
		 * reposition the OutcomeWidgets.
		 */
		void updateActionEffectSize (ActionEffectWidget* widget);
		
		void toggleConditionEditor ();
		
	public:
		
		/**
		 * Create an ActionWidget.
		 * Create an ActionEffectWidget to represent the Action's
		 * deterministic delayed effect. Create OutcomeWidgets for
		 * each probabilistic outcome of the action.
		 */
		ActionWidget (QWidget* parent, DOMAction* action,
			int outcomeSpacing,
			int barWidth,
			int numDurationUnitsPerBar,
			DOMDomain* domain,
			ActionEffectWidget* actionEffect,
			OutcomeWidgetFactory* outcomeWidgetFactory,
			BrazilController* controller);
		
		virtual ~ActionWidget() 
		{
			this->action->removeActionListener (this);
		}
		
		bool isShowBackground() { return showBackground; }
		void setShowBackground(bool value) { showBackground = value;}
		
		
		
		void setNumDurationUnitsPerBar (int num);
		void setBarWidth (int width);
		
		void setEffectHeight (int height)
		{
			actionEffect->setHeight (height);
		}
		
		void setOutcomeHeights (int height);
		
		void setHotAreaColour (QColor colour);
		void setHotAreaBorderColour (QColor colour);
		
		void setAlternatingBackgroundColours (QColor a, QColor b)
		{
			backgroundStripeAColour = a;
			backgroundStripeBColour = b;
			backgroundStripeA.setColor (backgroundStripeAColour);
			backgroundStripeB.setColor (backgroundStripeBColour);
		}
		
		//ProbabilisticListener methods.
		void outcomeAdded (DOMOutcome* outcome)
		{
			addOutcomeWidget (outcome);
			outcomeWidget [outcome]->getEffectWidget()->setFocus ();
		}
		
		void outcomeRemoved (DOMOutcome* outcome)
		{
			removeOutcomeWidget (outcome);
		}
		
		void probabilityChanged (DOMOutcome* outcome, double probability);
		
		//Action Listener methods
		virtual void nameChanged() { }
		
		virtual void backgroundColourChanged ( ) { }
		
		virtual void probabilisticAdded (DOMProbabilistic* probabilistic)
		{
			if(probabilisticListeningOn == NULL)			{
				probabilisticListeningOn = probabilistic;
				probabilistic->setProbabilisticListener (this);
			}
		}
		
		virtual void probabilisticRemoved (DOMProbabilistic* probabilistic) {
			if(probabilisticListeningOn == probabilistic) {
				//TO-Do.
			}
		}
		
		/**
		 * QT System function called whenever the widget needs
		 * to redraw itself.
		 *
		 * This simply draws the background of the widget,
		 * the child widgets are responsible for their own
		 * drawing.
		 */
		void paintEvent (QPaintEvent* event);
		
		void focusInEvent (QFocusEvent*)
		{
			controller->setActiveModel (action);
		}
		
		void setOutcomeContextMenu (QMenu* menu);
		
		void setActionEffectContextMenu (QMenu* menu)
		{
			actionEffect->setContextMenu (menu);
		}
	};

#endif

