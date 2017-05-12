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
 *  OutcomeWidget.h
 *  
 *
 *  Created by Owen Thomas on 15/03/06.
 *  
 *
 */

#ifndef outcome_widget
#define outcome_widget

#include <iostream>

#include <stdexcept>

#include <QLineEdit>
#include <QPalette>
#include <QBrush>
#include <QPen>
#include <QString>

#include "ProbabilityController.h"
#include "DelayedEffectWidget.h"
#include "../../model/OutcomeListener.h"
#include "../../model/impl/DOMOutcome.h"

#include "DelayedEffectWidgetFactory.h"

#include "BrazilController.h"

#define RESIZE_THRESHOLD 100

using namespace std;

/**
 * InvisibleLineEdit.
 * 
 * A QLineEdit subclass that when not focused has a transparent
 * background.
 *
 */
class InvisibleLineEdit : public QLineEdit
{
	Q_OBJECT
	
	private:
		QBrush invisible;
		QBrush solid;
		bool dynamicUpdateWidth;
		
	signals:
		void enter();
		void leave();
		void focusIn();
		void focusOut();
		
	protected slots:
		void updateWidth (const QString& text);
	public:

		InvisibleLineEdit (QWidget* parent = NULL, bool dynamicUpdateWidth = true) : QLineEdit (parent) 
		{ 
			invisible.setStyle (Qt::NoBrush);
			solid.setStyle(Qt::SolidPattern);
			solid.setColor(QColor(255,255,255));
			
			QPalette labelPalette = palette();
			labelPalette.setBrush(QPalette::Normal, QPalette::Base, invisible);
			
			QObject::connect (this, SIGNAL (textChanged (const QString&)),
				this, SLOT (updateWidth (const QString&)));
				
			this->dynamicUpdateWidth = dynamicUpdateWidth;
			setPalette (labelPalette);
		}
		
		void focusInEvent(QFocusEvent*)
		{
			
			QPalette labelPalette = palette();
			labelPalette.setBrush(QPalette::Normal, QPalette::Base, solid);
			setPalette (labelPalette);
			
			emit focusIn ();
		}
		
		void focusOutEvent(QFocusEvent*)
		{
			QPalette labelPalette = palette();
			labelPalette.setBrush(QPalette::Normal, QPalette::Base, invisible);
			setPalette (labelPalette);
	
			emit editingFinished ();
			emit focusOut ();
		}
		
		void enterEvent (QEvent*)
		{
			emit enter();
		}
		
		void leaveEvent (QEvent*)
		{
			emit leave ();
		}
};

/**
 * Displays a single probabilistic outcome from a Brazil probabilistc
 * grouping.
 *
 * Each OutcomeWidget manipulates a single DOMOutcome instance.
 * 
 * Each OutcomeWidget is drawn as a delayed effect (through a 
 * child DOMDelayedEffect widget) with a label text field and 
 * probability text field, that manipulate the name and probability
 * of the DOMOutcome.
 *
 */
class OutcomeWidget: public QWidget, public OutcomeListener//, public ControllerDelegateProvider
{
	Q_OBJECT
	
	private:
		//Label text field.
		InvisibleLineEdit* labelField;
		
		//Probability text field
		InvisibleLineEdit* probabilityField;
		
		//Widget for manipulating delayed effect.
		DelayedEffectWidget* effectWidget;
		
		//The underlying outcome being manipulated.
		DOMOutcome* outcome;
		
		BrazilController* controller;
	
		//Manages modifications to outcome probabilities.
		ProbabilityController* probabilityController;
		
		int numDurationUnitsPerBar;
		int barWidth;
		
	signals:
		/**
		 * Signals that this OutcomeWidget is being resized.
		 */
		void resizing (OutcomeWidget* widget);
		
		/**
		 * Request that the EffectEditor is displayed at the relative x position
		 * to the global position of the ActionEffectWidget.
		 */
		//void openEffectEditor (EffectEditor* editor, int xPosition, OutcomeWidget* widget);

	protected slots:
		
		void setActiveModel ()
		{
			this->controller->setActiveModel (outcome);
		}
		
	public slots:
	
		/**
		 * Indicates that the label text has changed
		 * and is ready to be passed to the Outcome model.
		 */
		void changeLabelText ()
		{
			outcome->setLabel (labelField->text().toUtf8().data());
		} 
		
		/**
		 * Indicates that the probability text 
		 * has changed and, if valid, is ready to be 
		 * passed to the Outcome model.
		 */
		void changeProbabilityText ();
		
		void probabilityChanged ();
		/**
		 * Indicates that we should resize the Outcome. Probably because
		 * the underlying DelayedEffectWidget has changed it's width.
		 */
		void resizeOutcome ()
		{
			resize (effectWidget->getWidth(), effectWidget->getHeight());
			emit resizing (this);
		}
		
	    
	protected:
		int getRoundedProbability();
		
		
	public:
		
		/**
		 * Create an OutcomeWidget to display and manipulate the specified
		 * DOMOutcome instance. It is constructed as a child of the
		 * specified parent widget. With the specified default duration
		 * values.
		 *
		 * Default durations are necessary so that when a new Duration
		 * is selected from the Duration drop down (within the contained
		 * delayed effect widget), that hasn't been selected before, the
		 * DelayedEffectWidget can display something sensible straight away.
		 */
		OutcomeWidget 
			(QWidget* parent, DOMOutcome* outcome, 
			DelayedEffectWidgetFactory* effectWidgetFactory,
			double scale,
			BrazilController* controller);
			
			
		~OutcomeWidget ()
		{
			delete labelField;
			delete probabilityField;
			delete effectWidget;
		}
		
		/**
		 * Returns the minimum height that this widget
		 * must occupy. This is essentially the minimum
		 * height of the contained effectWidget.
		 */
		int minimumHeight ()
		{
			return effectWidget->minimumHeight();
		}
		
		DelayedEffectWidget* getEffectWidget ()
		{
			return effectWidget;
		}
		
		
		void labelChanged ()
		{
			labelField->setText (outcome->getLabel());
			update();
		}
		
		void backgroundColourChanged ()
		{
			QColor colour (outcome->getBackgroundColour());
			effectWidget->setDurationColour(colour);
			update();
		}
		
		/**
		 * Return the DOMOutcome manipulated by this
		 * widget.
		 *
		 * @return the DOMOutcome manipulated by this
		 * widget.
		 */
		DOMOutcome* getDOMOutcome ()
		{
			return outcome;
		}
		
		void setScale (double scale) {
			effectWidget->setScale (scale);
		}
		
		double getScale () {
			return effectWidget->getScale ();
		}
		
		void setHeight (int height)
		{
			effectWidget->setHeight (height);
		}				
		
		void setContextMenu (QMenu* menu)
		{
			effectWidget->setContextMenu (menu);
		}
		/**
		 * Sets the duration names, that appear in the duration
		 * drop down of the DelayedEffectWidget.
		 */
		void setDurationNames (const QStringList& durationNames)
		{
			effectWidget->setDurationNames (durationNames);
		}
		
		const QLineEdit* getLabelField ()
		{
			return labelField;
		}
		
		const QLineEdit* getProbabilityField ()
		{
			return probabilityField;
		}
		
		void focusInEvent(QFocusEvent*) { effectWidget->setFocus(); }
		
		
};

#endif

