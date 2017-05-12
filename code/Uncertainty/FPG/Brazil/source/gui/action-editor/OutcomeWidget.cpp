/*
 *  OutcomeWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "OutcomeWidget.h"

OutcomeWidget::OutcomeWidget 
	(QWidget* parent, DOMOutcome* outcome,
	
	DelayedEffectWidgetFactory* effectWidgetFactory,
	double scale,
	BrazilController* controller) : QWidget(parent)
{
	 //Create the DelayedEffectWidget - responsible for manipulating the durative
	 //effect of the outcome. Passing in the outcome's delayed effect and the
	 //default duration values.
	
	  this->effectWidget = effectWidgetFactory->createDelayedEffectWidget
	      ( outcome->getAtStartEffects(), *(outcome->getEffects().begin ()), this, outcome->getBackgroundColour (), scale);
		
	
	 effectWidget->setParent (this);
	 effectWidget->move(0,0);
	 
	 this->outcome = outcome;
	 outcome->setListener(this);
	 
	 this->probabilityController = new ProbabilityController (outcome->getProbabilistic ());
	 
	 //resize this OutcomeWidget to the size of the contained DelayedEffectWidget.
	 resize (effectWidget->getWidth (), effectWidget->getHeight());
	 
	 labelField = new InvisibleLineEdit (this);
	 labelField->setFrame(false);
	 labelField->move (0, 0);
	 
	 probabilityField = new InvisibleLineEdit (this, false);
	 probabilityField->setFrame(false);
	 probabilityField->move(0,labelField->height());
	 
	 labelField->show ();
	 probabilityField->show ();
	 
	 effectWidget->lower ();
	 //Set the initial probability text field value.
	 QString probabilityString;
	 probabilityString.setNum(getRoundedProbability ());
	 probabilityString.append ("%");
	 
	 this->controller = controller;
	 
	 probabilityField->setText(probabilityString);
	 
	 //Set the initial label text field value.
	 labelField->setText (outcome->getLabel ());
	 
	 //When the label text field has finished editing, update
	 //the Outcome's label value.
	 QObject::connect (labelField, SIGNAL (editingFinished()),
					   this, SLOT (changeLabelText()));
	 
	 //When the probability text field has finished editing, update
	 //the Outcome's probability value.
	 QObject::connect (probabilityField, SIGNAL (editingFinished()),
					   this, SLOT (changeProbabilityText()));
	 
	 //When the effect widget resizes, this OutcomeWidget must be 
	 //resized to contain it's new size.			
	 QObject::connect (effectWidget, SIGNAL (resizing()),
					   this, SLOT (resizeOutcome()));
	 
	 
	 QObject::connect (labelField, SIGNAL (enter()),
						effectWidget, SLOT (siblingWidgetEntered ()));
						
	 QObject::connect (labelField, SIGNAL (leave ()),
						effectWidget, SLOT (siblingWidgetLeft ()));
						
	 QObject::connect (labelField, SIGNAL (focusIn()),
						effectWidget, SLOT (siblingWidgetFocusIn()));
			
	 QObject::connect (labelField, SIGNAL (focusOut()),
						effectWidget, SLOT (siblingWidgetFocusOut()));
						

	 QObject::connect (probabilityField, SIGNAL (enter()),
						effectWidget, SLOT (siblingWidgetEntered ()));
						
	 QObject::connect (probabilityField, SIGNAL (leave ()),
						effectWidget, SLOT (siblingWidgetLeft ()));
						
	 QObject::connect (probabilityField, SIGNAL (focusIn()),
						effectWidget, SLOT (siblingWidgetFocusIn()));
			
	 QObject::connect (probabilityField, SIGNAL (focusOut()),
						effectWidget, SLOT (siblingWidgetFocusOut()));

	
	 //Manage active model
	 QObject::connect (labelField, SIGNAL (focusIn ()),
						this, SLOT (setActiveModel ()));
						
	 QObject::connect (probabilityField, SIGNAL (focusIn ()),
						this, SLOT (setActiveModel ()));
						
	  								
	 
}

int OutcomeWidget::getRoundedProbability ()
{
	return (int) (100 * outcome->getProbability());
}

void OutcomeWidget::changeProbabilityText ()
{
	QString value = probabilityField->text();
	
	bool error = false;
	
	//If we have a % sign at th end remove it.
	if(value.endsWith(QString("%")))
	{
		value.remove (value.size()-1,value.size()-1);
	}
	
	bool validInteger = true;
	
	int valueInt =  value.toInt (&validInteger);
	
	if(!validInteger || valueInt < 0)
	{
		error = true;
	}
	else
	{
		try
	{
		
		probabilityController->setProbability (outcome, (valueInt / 100.0));
		
	}
		catch (const invalid_argument inv)
	{
			error = true;
	}
	}
	
	//If there was an error converting the text value to the
	//underyling probability, then set the text to the old
	//probability value and highlight it.
	if(error)
	{
		QString probabilityString;
		probabilityString.setNum(getRoundedProbability());
		probabilityString.append(QString("%"));
		
		probabilityField->setText (probabilityString);
		probabilityField->selectAll();
		probabilityField->setFocus(); 
	}
}

void OutcomeWidget::probabilityChanged()
{
	//Retrieve the underlying probability value and
	//convert it to a percentage and update the probability
	//text field.
	QString probabilityString;
	probabilityString.setNum( getRoundedProbability());
	probabilityString.append ("%");
	probabilityField->setText(probabilityString);
	update ();
}


