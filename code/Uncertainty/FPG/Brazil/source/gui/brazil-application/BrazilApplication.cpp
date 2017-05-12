/*
 *  BrazilApplication.cpp
 *  
 *
 *  Created by Owen Thomas on 7/06/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "BrazilApplication.h"

#include <QList>
#include <QFileDialog>
#include <QMenuBar>
#include <QKeySequence>

#include "../action-editor/InternalConditionEditor.h"

BrazilApplication::BrazilApplication (DOMDomain& domain, 
	QMainWindow* actionEditorWindow, ActionWidgetFactory* actionWidgetFactory,
	QWidget* parent) : QWidget (parent)
{
	this->actionEditorWindow = actionEditorWindow;
	this->domain = &domain;
	this->actionWidgetFactory = actionWidgetFactory;
	this->openAction = NULL;
	
	QTabWidget* tabWidget = new QTabWidget (this);
	
	actionListWidget = new ActionListWidget (domain, NULL);
	predicates = new PredicateListWidget (*(domain.getProblem()), NULL);
	functions = new FunctionDefinitionWidget ( *(domain.getProblem ()), NULL);
	goal = new InternalConditionEditor ( *(domain.getProblem()->getGoal ()), domain, NULL);
	goal->hideDeleteButton ();
	
	QBoxLayout* centralWidgetLayout = new QVBoxLayout ();
	
	setLayout (centralWidgetLayout);
		
	tabWidget->addTab (actionListWidget, "Actions");
	tabWidget->addTab (predicates, "Predicates");
	tabWidget->addTab (functions, "Functions");
	tabWidget->addTab (goal, "Goal Condition");
	
	centralWidgetLayout->addWidget (tabWidget);
		
	QObject::connect (actionListWidget, SIGNAL (openActionEditor (DOMAction*)),
		this, SLOT (openActionEditor (DOMAction*)));
		
	QObject::connect (actionListWidget, SIGNAL (closeActionEditor (DOMAction*)),
		this, SLOT (closeActionEditor (DOMAction*)));
		
}

BrazilApplication::~BrazilApplication () {
}

void BrazilApplication::openActionEditor (DOMAction* action)
{
	if(!actionEditorWindow) return;
	
	actionEditorWindow->hide();
	QWidget* centralWidget = actionEditorWindow->centralWidget ();
	
	if(centralWidget) delete centralWidget;
	
	
	actionEditorWindow->setCentralWidget(actionWidgetFactory->createActionWidget 
		(domain,action));

	openAction = action;
	actionEditorWindow->show ();
}

void BrazilApplication::closeActionEditor (DOMAction* action)
{
	if(!actionEditorWindow) return;
	
	if(action == openAction) {
		QWidget* centralWidget = actionEditorWindow->centralWidget ();
		actionEditorWindow->hide ();
		if(centralWidget) delete centralWidget;
	}
}

void BrazilApplication::setDomain (DOMDomain& domain) {
	//We must do the following four things:
	
	//If we have an action editor open, delete it!
	if(actionEditorWindow) {
		actionEditorWindow->hide();
		QWidget* centralWidget = actionEditorWindow->centralWidget ();
	
		if(centralWidget) delete centralWidget;
	}
	
	//Set the action list widget to the new domain.
	actionListWidget->setDomain (domain);
	goal->setCondition (*(domain.getProblem()->getGoal()),domain);
	predicates->setProblem (*(domain.getProblem ()));
	functions->setProblem (*(domain.getProblem ()));
	
	this->actionWidgetFactory->setDomain (domain);
	this->domain = &domain;
	
}


