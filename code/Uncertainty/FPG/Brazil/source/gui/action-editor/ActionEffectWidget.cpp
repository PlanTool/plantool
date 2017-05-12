/*
 *  ActionWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 26/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "ActionEffectWidget.h"

void ActionEffectWidget::changeName ()
{
	action->setName (nameField->text().toUtf8().data());
}

void ActionEffectWidget::resizeActionEffect ()
{
	resize(effect->getWidth(), effect->getHeight());	
	emit resizing (this);
}


ActionEffectWidget::ActionEffectWidget (QWidget* parent, DOMAction* action,
	DelayedEffectWidgetFactory* effectWidgetFactory,
	double scale,
	BrazilController* controller)
	: QWidget(parent)
{

	this->controller = controller;

		
		
	
	nameField = new InvisibleLineEdit (this, true);
	nameField->setFrame(false);
	nameField->move(0,0);
	nameField->setText(action->getName());


	//Create the DelayedEffectWidget to display the duration and effects for this
	//class.
	
	this->effect = effectWidgetFactory->createDelayedEffectWidget
	    ( action->getAtStartEffectSet(), action->getEffectSet(), this, action->getBackgroundColour (), scale);
	effect->move(0,0);

	setFocusProxy (effect);
	
	resize(effect->getWidth(), effect->getHeight());

	nameField->show ();
	effect->show ();
	effect->lower ();
	//When editing has finished on the nameField we want to update the 
	//value of the name in the model.
	QObject::connect (nameField, SIGNAL (editingFinished()),
		this, SLOT (changeName()));
	
	//When the nameField loses focus we also want to update
	//the name.
	QObject::connect (nameField, SIGNAL (focusOut()),
		this, SLOT (changeName()));

	//When the DelayedEffectWidget changes size, we must also change our
	//size.
	QObject::connect (effect, SIGNAL (resizing()),
		this, SLOT (resizeActionEffect()));

		
	 QObject::connect (nameField, SIGNAL (enter()),
						effect, SLOT (siblingWidgetEntered ()));
						
	 QObject::connect (nameField, SIGNAL (leave ()),
						effect, SLOT (siblingWidgetLeft ()));
						
	 QObject::connect (nameField, SIGNAL (focusIn()),
						effect, SLOT (siblingWidgetFocusIn()));
			
	 QObject::connect (nameField, SIGNAL (focusOut()),
						effect, SLOT (siblingWidgetFocusOut()));
						
						
	 QObject::connect (nameField, SIGNAL (focusIn()),
						this, SLOT (setActiveModel()));
			
	this->action = action;
	
	this->action->addActionListener(this);
}

ActionEffectWidget::~ActionEffectWidget()
{
	//Disconnect slots, QT might do this for us.
	QObject::disconnect (nameField, SIGNAL (editingFinished()),
		this, SLOT (changeName()));

	this->action->removeActionListener(this);
	delete effect;
	delete nameField;
}

void ActionEffectWidget::nameChanged ()
{
	nameField->setText (action->getName());
}

void ActionEffectWidget::backgroundColourChanged ()
{
	QColor colour (action->getBackgroundColour());
	effect->setDurationColour (colour);
}






