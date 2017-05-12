/*
 *  ActionListWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 6/06/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "ActionListWidget.h"
#include "../../model/impl/DOMFixedDuration.h"
#include "../../model/impl/DOMEffectSet.h"

#include <xercesc/dom/DOM.hpp>

ActionListWidget::ActionListWidget (DOMDomain& domain, QWidget* parent) 
	: QWidget (parent)
{
	this->domain = NULL;
	this->actionNames = NULL;
	
	newActionName = new QLineEdit (this);
	newActionButton = new QPushButton (QObject::tr ("New Action"));

	QHBoxLayout* newActionLayout = new QHBoxLayout ();
	newActionLayout->addWidget (newActionName);
	newActionLayout->addWidget (newActionButton);
	
	QVBoxLayout* thisLayout = new QVBoxLayout ();
	thisLayout->addLayout (newActionLayout);

	
	//Create a new action when the button is pressed or return is pressed
	//in the line edit.
	QObject::connect (newActionButton, SIGNAL (clicked (bool)), 
		this, SLOT (newAction()));
	QObject::connect (newActionName, SIGNAL (returnPressed ()), 
		this, SLOT (newAction()));
	
	
	this->deleteName = "delete";
	supportedNames.push_back (deleteName);
	
	//Create QActions.
	deleteAction = new QAction (QObject::tr ("Delete Action"), NULL);
	sortAction = new QAction (QObject::tr ("Sort Actions"), NULL);
	
	deleteAction->setEnabled (false);
	
	listWidgetContextMenu = new QMenu (actionNames);
	listWidgetContextMenu->addAction (deleteAction);
	listWidgetContextMenu->addAction (sortAction);
	
	setLayout (thisLayout);
	
	init (domain);
	
	QObject::connect (deleteAction, SIGNAL (triggered (bool)), 
		this, SLOT (deleteSelected ()));
	QObject::connect (sortAction, SIGNAL (triggered(bool)),
		this, SLOT (sortActions()));

	
	
}

void ActionListWidget::showListWidgetContextMenu (const QPoint& pos)
{
	
	//Position the menu sensibly.
	QPoint newPos = actionNames->mapToGlobal (pos);
	newPos.setX (newPos.x() - 1);
	newPos.setY (newPos.y() - 1);
	listWidgetContextMenu->popup(newPos);
	
}

void ActionListWidget::actionNameSelected (QListWidgetItem* actionItem)
{
	DOMAction* action = ((ActionListWidgetItem*)actionItem)->getAction ();
	if(action) emit openActionEditor (action);
}

void ActionListWidget::listWidgetSelectionChanged ()
{
	deleteAction->setEnabled(actionNames->selectedItems().size() != 0);
}

bool ActionListWidget::existingActionName (const QString& actionName)
{
	return actionNames->findItems (actionName, Qt::MatchExactly).size();
}

void ActionListWidget::newAction ()
{
	const QString& actionName = newActionName->text ();
	
	//If we have no data entered, return.
	if(!actionName.size()) return;
	
	//Add the action only if an action with that name does not
	//exist.
	if(!existingActionName (actionName)) {
		//Create the New Action.
		DOMDuration* fixedDuration = new DOMFixedDuration 
			(ACTION_DURATION, domain->getDOMElement()->getOwnerDocument());
		
		
		DOMAction* newAction  = new DOMAction 
			(actionName.toUtf8().data(), *domain);
		newAction->setBackgroundColour ("#98FB98");
		newAction->getEffectSet()->setDelay (fixedDuration);
		
		domain->addAction (newAction);
		
		newActionName->setText("");
	} else {
		newActionName->setSelection (0, actionName.size());
	}
}

void ActionListWidget::executeDelete ()
{
	QList<QListWidgetItem*> selectedActions = actionNames->selectedItems ();
	if(selectedActions.size()) {
		QList<QListWidgetItem*>::iterator it;
		for(it = selectedActions.begin(); it != selectedActions.end(); it++) {
			DOMAction* action = ((ActionListWidgetItem*)(*it))->getAction ();
			domain->removeAction (action);
		}
	}
}

bool ActionListWidget::canExecuteDelete ()
{
	return actionNames->selectedItems().size();
}

void ActionListWidget::execute (const QString& name)
{
	if(name == "delete") executeDelete();
}

bool ActionListWidget::canExecute (const QString& name) 
{
	return (name == "delete" && canExecuteDelete());
}

list<QString> ActionListWidget::getSupportedNames () 
{
	return supportedNames;
}

void ActionListWidget::sortActions ()
{
	actionNames->sortItems ();
}

void ActionListWidget::deleteSelected ()
{
	//I create a set of actions which I place the actions into delete.
	//If iterate and delete directly over the list, then the for-loop
	//index could potentially become out of sync.
	set<DOMAction*> deleteActions;
	
	for(int i = 0;  i < actionNames->count(); i++) {
		//Only delete the item if it's selected.
		if(!actionNames->isItemSelected (actionNames->item (i))) continue;
		
		DOMAction* action = ((ActionListWidgetItem*) 
			actionNames->item (i))->getAction();
		
		if(action) {
			deleteActions.insert (action);
		}
	}
	
	set<DOMAction*>::iterator it;
	for(it = deleteActions.begin(); it != deleteActions.end(); it++) {
		emit closeActionEditor ((*it));
		domain->removeAction (*it);
		delete (*it);
	}
}

void ActionListWidget::init (DOMDomain& domain) {
	//Remove myself from listener of preivous domain.
	if(this->domain) {
		this->domain->removeListener (this);
	}
	this->domain = &domain;
	this->domain->addListener (this);
	
	//delete actionNames list widget.
	if(this->actionNames) {
		layout()->removeWidget (actionNames);
		delete actionNames;
	}
	//Recreate actionNames
	actionNames = new QListWidget (this);
	actionNames->setContextMenuPolicy (Qt::CustomContextMenu);
	layout()->addWidget (actionNames);
	
	//Populate actionNames	
	list<DOMAction*> actions = domain.getActions ();
	list<DOMAction*>::iterator it;

	for(it = actions.begin(); it != actions.end(); it++) {
		new ActionListWidgetItem (*(*it), (*it)->getName (), 
			actionNames);
	}

	
	//Connect slots list widget slots.	
	QObject::connect (actionNames, SIGNAL (itemDoubleClicked (QListWidgetItem*)),
		this, SLOT (actionNameSelected (QListWidgetItem*)));

	QObject::connect (actionNames, SIGNAL 
		(customContextMenuRequested (const QPoint&)), this, SLOT 
		(showListWidgetContextMenu (const QPoint&)));

	QObject::connect (actionNames, SIGNAL (itemSelectionChanged()), 
		this, SLOT (listWidgetSelectionChanged()));

}


