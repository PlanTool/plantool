/*
 *  ActionWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 26/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "ActionWidget.h"
#include <QPainter>
#include <QRect>
#include <QPoint>

#include "InternalConditionEditor.h"

ActionWidget::ActionWidget (
	QWidget* parent, //Parent widget
	DOMAction* action,	//DOMAction this is representing
	int outcomeSpacing,	//Vertical spacing between Outcome Widgets
	int barWidth,		//Width (in pixels) of each vertical bar
	int numDurationUnitsPerBar, //Number of seconds in each vertical bar.
	DOMDomain* domain,	
	ActionEffectWidget* actionEffect,
	OutcomeWidgetFactory* outcomeWidgetFactory,
	BrazilController* controller	//Brazil Action Controller
									//Contains tool bar actions that modify the
									//DOMAction this represents.
	) : QWidget (parent)
{
	this->action = action;
	this->action->addActionListener (this);
	this->defaultDurations = defaultDurations;
	this->durationNames = durationNames;
	
	this->outcomeSpacing = outcomeSpacing;
	this->numDurationUnitsPerBar = numDurationUnitsPerBar;
	this->barWidth = barWidth;
	this->operatorDisplayNames = operatorDisplayNames;
	this->domain = domain;
	
	this->showBackground = true;
	this->controller = controller;
	this->outcomeWidgetFactory = outcomeWidgetFactory;
	this->outcomeContextMenu = NULL;
	this->leftMargin = 15;

	if(action->getPrecondition()) {
	
		preconditionEditor = new InternalConditionEditor (*(action->getPrecondition ()),
			*domain, NULL);
		
		preconditionEditor->setWindowFlags (Qt::Popup);
		
		QString operatorText(" are met then ");
		operatorText.append (action->getName ());
		operatorText.append (" can start.");
	
		preconditionEditor->setOperatorBarText (operatorText);
		preconditionEditor->hide ();	
		preconditionEditor->hideDeleteButton ();
		
	}
	
	backgroundStripePen.setStyle(Qt::DashLine);
	backgroundStripePen.setColor(QColor ("black"));
	
	backgroundStripeA.setStyle (Qt::SolidPattern);
	backgroundStripeB.setStyle (Qt::SolidPattern);
	
	this->actionEffect = actionEffect;
	actionEffect->setParent (this);
	actionEffect->move(leftMargin,0);
	actionEffect->setVisible (true);
	
	QObject::connect (actionEffect, SIGNAL (resizing(ActionEffectWidget*)),
		this, SLOT (updateActionEffectSize(ActionEffectWidget*)));

		
	if(action->getProbabilisticChildren().size()) {
		DOMProbabilistic* probabilistic = *(action->getProbabilisticChildren().begin());
		probabilistic->setProbabilisticListener(this);
		map<DOMOutcome*, double> domOutcomes = probabilistic->getOutcomes();
		map<DOMOutcome*, double>::iterator it;
		
		for(it = domOutcomes.begin(); it != domOutcomes.end(); it++)
		{
			addOutcomeWidget (it->first);
		}
		probabilisticListeningOn = probabilistic;
	}
	else {
		probabilisticListeningOn = NULL;
	}
		
	setFocusPolicy(Qt::ClickFocus);
	
	
	displayConditionEditorWidget = new QPushButton (this);
	displayConditionEditorWidget->setGeometry (0,0,leftMargin,actionEffect->height());
	
	QObject::connect (displayConditionEditorWidget, SIGNAL (pressed ()),
		this, SLOT (toggleConditionEditor ()));
	
}

void ActionWidget::updateActionEffectSize (ActionEffectWidget* widget)
{
	//Move each of the OutcomeWidgets to the left of the ActionEffectWidgets
	//duration position (reached through getDurationWidth()).
	
	list<OutcomeWidget*>::iterator it;
	for(it = this->outcomes.begin(); it != this->outcomes.end(); it++)
	{
		(*it)->move ( leftMargin + widget->getDurationWidth (), (*it)->y());
	}
}


void ActionWidget::paintEvent(QPaintEvent* event)
{
	
	//painter.drawRect (0, 0, leftMargin, outcomeHeight);
	
	if(showBackground)
	{	
	
		QPainter painter(this);
	
		//Paint the striped background.
		QPen clearPen (Qt::NoPen);
		
		//Boolean flag indicating which stripe we are currently painting.
		bool paintA = true;
		
		painter.setPen (clearPen);
		painter.setBrush (backgroundStripeA);
		
		//Draws the first (wider) vertical bar.
		painter.drawRect (0, 0, leftMargin, event->rect().height());
		
		//Width of each bar.

		
		for(int i = leftMargin; i < width(); i = i + barWidth)
		{
			painter.setPen(clearPen);
			if(paintA) painter.setBrush(backgroundStripeA);
			else painter.setBrush(backgroundStripeB);
			paintA = !paintA;
				
			painter.drawRect (i, 0, barWidth, height());
			
			painter.setPen(backgroundStripePen);
			
			painter.drawLine (i+ barWidth - 1, event->rect().y(), i+barWidth-1, 
				event->rect().y() + event->rect().height());
		}
	}
}

void ActionWidget::addOutcomeWidget(DOMOutcome* outcome)
{
	OutcomeWidget* widget = outcomeWidgetFactory->createOutcomeWidget (outcome, this, (double)barWidth / numDurationUnitsPerBar);
	

	widget->getEffectWidget()->setHotAreaColour (this->hotAreaColour);
	widget->getEffectWidget()->setHotAreaBorderColour (this->hotAreaBorderColour);

	
	widget->move (leftMargin + actionEffect->getDurationWidth(), actionEffect->height ()
	 + outcomeSpacing * (outcomes.size() +1) + outcomes.size() * widget->height()); 

	outcomes.push_back(widget);
	
	outcomeWidget [outcome] = widget;
	widgetOutcome [widget] = outcome;
		
		
	widget->setContextMenu (outcomeContextMenu);
	widget->setVisible (true);
	
}

void ActionWidget::removeOutcomeWidget(DOMOutcome* outcome)
{
	if(!outcomeWidget.count (outcome)) return;
	
	OutcomeWidget* removeWidget = outcomeWidget [outcome];
	
	//Iterate through outcomewidget list. After
	//we remove the outcomewidget we then reposition
	//the outcomes beneath it up.
	list<OutcomeWidget*>::iterator it;
	
	bool foundRemoveOutcome = false;
	bool setFocusOnNextWidget = false;
	for(it = outcomes.begin (); it != outcomes.end (); it ++) {
	
		foundRemoveOutcome = foundRemoveOutcome || (*it == removeWidget);
		
		
		//reposition
		if(foundRemoveOutcome &&  (*it != removeWidget)) {
		
			(*it)->move (actionEffect->getDurationWidth() + leftMargin, (*it)->y() 
			- outcomeSpacing - (*it)->height ());
			
			if(!setFocusOnNextWidget) {
				(*it)->setFocus ();
				setFocusOnNextWidget = true;
			}
		}
	}
	
	outcomes.remove (removeWidget);
	
	
	//found outcome remove was the last outcome in the list.
	if(outcomes.size() && foundRemoveOutcome && ! setFocusOnNextWidget) {
		
		list<OutcomeWidget*>::reverse_iterator rit = outcomes.rbegin ();
		(*rit)->setFocus ();
	}
	
	outcomeWidget.erase (outcome);
	widgetOutcome.erase (removeWidget);
	
	delete removeWidget;
}

void ActionWidget::setNumDurationUnitsPerBar (int num) {
	this->numDurationUnitsPerBar = num;
	actionEffect->setScale ( (double) barWidth / numDurationUnitsPerBar);
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++)
	{
		(*it)->setScale( (double) barWidth / numDurationUnitsPerBar);
	}
}

void ActionWidget::setBarWidth (int width) {
	this->barWidth = width;
	actionEffect->setScale ( (double) barWidth / numDurationUnitsPerBar);
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++)
	{
		(*it)->setScale( (double) barWidth / numDurationUnitsPerBar);
	}
}

void ActionWidget::setHotAreaColour (QColor colour)
{
	this->hotAreaColour = colour;
	if(NULL != actionEffect->getEffectWidget ()) {
		actionEffect->getEffectWidget()->setHotAreaColour (colour);
	}
	
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++) {
		if((*it)->getEffectWidget() != NULL) {
			(*it)->getEffectWidget()->setHotAreaColour (colour);
		}
	}
}

void ActionWidget::setHotAreaBorderColour (QColor colour)
{
	this->hotAreaBorderColour = colour;
	if(NULL != actionEffect->getEffectWidget ()) {
		actionEffect->getEffectWidget()->setHotAreaBorderColour (colour);
	}
	
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++) {
		if((*it)->getEffectWidget() != NULL) {
			(*it)->getEffectWidget()->setHotAreaBorderColour (colour);
		}
	}
}

void ActionWidget::setOutcomeHeights (int height)
{
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++) {
		(*it)->setHeight(height);
	}
}

void ActionWidget::setOutcomeContextMenu (QMenu* menu)
{
	this->outcomeContextMenu = menu;
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++) {
		(*it)->setContextMenu(menu);
	}
}

void ActionWidget::toggleConditionEditor ()
{
	if(preconditionEditor) {
		preconditionEditor->show ();
		
		preconditionEditor->raise();
		
		
			//move condition editor to underneat th button that requested it.
			preconditionEditor->move ( mapToGlobal (QPoint(displayConditionEditorWidget->x(),
				displayConditionEditorWidget->y() + displayConditionEditorWidget->height ())));
		
		
	}
}

void ActionWidget::probabilityChanged (DOMOutcome* outcome, double ) {
	
	//find outcome widget.
	list<OutcomeWidget*>::iterator it;
	for(it = outcomes.begin (); it != outcomes.end (); it++) {
		if( (*it)->getDOMOutcome () == outcome) {
			(*it)->probabilityChanged ();
		}
	}
}

