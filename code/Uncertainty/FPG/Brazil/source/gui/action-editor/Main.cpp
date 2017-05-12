/*
 *  Main.cpp
 *  
 *
 *  Created by Owen Thomas on 16/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>
#include <vector>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include <xercesc/framework/LocalFileFormatTarget.hpp>



#include <QApplication>
#include <QColor>
#include <QMainWindow>
#include <QToolBar>


#include <QIconEngine>
#include <QSize>
#include <QKeySequence>
#include <QMenu>
#include <QSvgWidget>
#include <QToolButton>
#include <QScrollArea>

#include "../BrazilPlanningWrapper.h"
#include "../brazil-application/BrazilMainWindow.h"
#include "../brazil-application/XMLBrazilConfiguration.h"
#include "../brazil-application/BrazilApplication.h"
#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMAction.h"
#include "../../model/impl/DOMConditionFactory.h"

#include "ConditionEditor.h"
#include "ActionWidget.h"
#include "ActionEffectWidget.h"
#include "ActionEffectWidgetFactory.h"
#include "OutcomeWidgetFactory.h"
#include "DelayedEffectWidgetFactory.h"
#include "BrazilController.h"

#include "../brazil-application/SideMenu.h"

using namespace std;
XERCES_CPP_NAMESPACE_USE
/*

void printPredicate (DOMPredicate* predicate)
{
	cout << "predicate: " << predicate->getName() << endl;
}

void printOutcome (DOMOutcome* outcome)
{
	cout << outcome->getLabel ()
		<< " " << outcome->getBackgroundColour()
		
		<< " " << outcome->getProbability() << endl;
}
void printAction (DOMAction* action)
{
	cout << "action: " << action->getName() << endl;
	list<DOMOutcome*> outcomes = action->getOutcomes ();
	cout << "outcomes" << endl;
	list<DOMOutcome*>::iterator it;
	for(it = outcomes.begin(); it != outcomes.end(); it++) {
		printOutcome (*it);
	}
}
*/
DOMDomain* printDomain (DOMElement* element)
{
	DOMDomain* domain = new DOMDomain (element);
	return domain;
	/*
	cout << domain->getName() << endl;
	
	cout << "has " << domain->getActions().size() << " actions." << endl;
	cout << "has " << domain->getPredicates().size() << " predicates." << endl;
	
	cout << "predicates: " << endl;
	set<DOMPredicate*>::iterator pIt;
	set<DOMPredicate*> predicates = domain->getPredicates();
	for(pIt = predicates.begin(); pIt != predicates.end(); pIt ++) {
		printPredicate ( (*pIt));
	}
	
	cout << "actions: " << endl;
	list<DOMAction*>::iterator aIt;
	list<DOMAction*> actions = domain->getActions();

	for(aIt = actions.begin(); aIt != actions.end(); aIt ++) {
		printAction ( (*aIt));
	}
	
	return domain;*/
}

void createBrazilApplication (DOMDomain* domain, XercesDOMParser* parser, QApplication* app)
{
	//Create Default Durations:
	QStringList durationNameList;
	durationNameList << "Fixed" << "Normal" << "Uniform" << "Exponential";

	
	//Create Atomic Effect Operator Display Names
	map<QString, QString> operatorDisplayNames;
	
	operatorDisplayNames [QString ("predicate")] = QString("set");
	operatorDisplayNames [QString ("not")] = QString ("remove");

	//Create Controller to interface with tool bars, menus.
	//and to control the action widget.
	BrazilControllerGroup* group = new BrazilControllerGroup ();

	//Create delete action.
	map <QString, DeleteModel*>deleters;
	
	DeleteDOMOutcome deleteDOMOutcome;
	
	deleters [QString ("DOMOutcome")] = &deleteDOMOutcome;
		DeleteAction* deleteAction = new DeleteAction(deleters);

	//Create new Action

	//First, parse prototypes.
	parser->parse ("gui/action-editor/new-prototypes/outcome.xml");
	
	DOMDocument* outcomeDocument = parser->getDocument ();
	DOMElement* outcomeElement = outcomeDocument->getDocumentElement ();
	
	parser->parse ("gui/action-editor/new-prototypes/effect.xml");
	
	//New Outcome Colours.
	std::vector<QColor> colours;
	colours.push_back( QColor ("#3366ff"));
	colours.push_back(QColor ("#33ff66"));
	colours.push_back(QColor ("#ffcc33"));
	colours.push_back(QColor ("#ff3366"));
	
	NewDOMOutcome newOutcome (*outcomeElement,  colours);
	
	//Mapping from model type to NewAction handler for that type.
	map <QString, NewModel*> newModels;
	newModels [QString ("DOMOutcome")] = &newOutcome;
	newModels [QString ("DOMAction")] = &newOutcome;
	
	//Create the new action using the handlers.
	NewAction* newAction = new NewAction(newModels);

	//Set action icons and short cuts
	QIcon deleteIcon ("delete.png");
	QIcon newIcon ("new.png");
	
	deleteAction->setIcon (deleteIcon);
	deleteAction->setShortcut (QKeySequence("Ctrl+D"));
	
	newAction->setShortcut (QKeySequence ("Ctrl+N"));
	newAction->setIcon (newIcon);

	
	//Create changing colour actions
	ChangeDOMOutcomeColourAction* changeColourAction = new ChangeDOMOutcomeColourAction ();
	ChangeDOMActionColourAction* changeActionColourAction = new ChangeDOMActionColourAction ();
	
	
	//Create the controllergroup
	group->addController (deleteAction);
	group->addController (newAction);
	group->addController (changeColourAction);
	group->addController (changeActionColourAction);

	//Create context menus
	QMenu* outcomeContextMenu = new QMenu ();
	outcomeContextMenu->addAction (changeColourAction);
	outcomeContextMenu->setDefaultAction (changeColourAction);
	
	QMenu* actionEffectContextMenu = new QMenu ();
	actionEffectContextMenu->addAction (changeActionColourAction);
	actionEffectContextMenu->setDefaultAction (changeActionColourAction);
	
	//Create ToolBar
	QToolButton* newButton = new QToolButton ();
	newButton->setDefaultAction (newAction);
		
	QToolButton* deleteButton = new QToolButton ();
	deleteButton->setDefaultAction (deleteAction);
	
	QToolButton* arrowButton = new QToolButton ();
	arrowButton->setArrowType (Qt::DownArrow);
	arrowButton->setPopupMode (QToolButton::InstantPopup);

	QMenu* arrowButtonMenu = new QMenu ();
	arrowButton->setMenu (arrowButtonMenu);
	arrowButton->setAutoRaise (true);
	
	QToolBar* toolBar = new QToolBar();
	toolBar->setMovable(false);
	toolBar->addWidget (deleteButton);
	toolBar->addWidget (newButton);
	toolBar->addWidget (arrowButton);
	
	

	QMainWindow* editorWindow = new QMainWindow ();
	
	//Create ActionWidget
	int effectHeight = 50;
	int outcomeHeight = 50;
	int barWidth = 25;
	int numDurationUnitsPerBar = 3600;
	
	int outcomeSpacing = 5;
	
	
	DelayedEffectWidgetFactory* dewFactory = 
		new DelayedEffectWidgetFactory (effectHeight, 
		 domain, group, &durationNameList);
	
	OutcomeWidgetFactory* owFactory = 
		new OutcomeWidgetFactory (outcomeHeight, group, dewFactory);
	
	ActionEffectWidgetFactory* aewFactory = 
		new ActionEffectWidgetFactory (outcomeHeight, group, dewFactory);

	ActionWidgetFactory* awFactory =
		new ActionWidgetFactory (outcomeSpacing, barWidth, numDurationUnitsPerBar,owFactory, 
			aewFactory, group, outcomeContextMenu, actionEffectContextMenu);
	
	
	editorWindow->addToolBar (toolBar);
	editorWindow->setGeometry (100, 100, 760, 340);

	XercesDOMParser* domainParser = new XercesDOMParser ();
//	domainParser->setDoNamespaces (true);
	//domainParser->setDoSchema (true);
	//XMLCh* loc = XMLString::transcode 
		//("../datamodel/schema/domain.xml");
//	domainParser->setExternalNoNamespaceSchemaLocation (loc);

	ErrorHandler* errHandler = (ErrorHandler*) new HandlerBase();
	domainParser->setErrorHandler(errHandler);


	QString configFileName = "config.xml";	

	parser->parse ("config.xml");
	DOMDocument* configDoc = parser->getDocument ();
	DOMElement* configElement = configDoc->getDocumentElement ();
	
	XMLBrazilConfiguration* config = new XMLBrazilConfiguration (*configElement);
	
	BrazilMainWindow* brazil = new BrazilMainWindow (*domain, 
		editorWindow, awFactory, domainParser, config,
		"gui/action-editor/new-prototypes/new-domain.xml");
	

	
	brazil->show ();
	app->exec ();
	
	config->save (configFileName);
	
}

#include <QListWidget>
/*
void createActionEditor (DOMAction* action, DOMDomain* domain, XercesDOMParser* parser,QApplication* app)
{
	//Create Default Durations:
	QStringList durationNameList;
	durationNameList << "Fixed" << "Normal" << "Uniform" << "Exponential";

	DOMDocument* doc = action->getDOMElement()->getOwnerDocument ();
	
	DOMFixedDuration* defaultFixed = new DOMFixedDuration(20, doc);
	DOMNormalDuration* defaultNormal = new DOMNormalDuration(10,5,doc);
	DOMUniformDuration* defaultUniform = new DOMUniformDuration (10, 20, doc);
	DOMExponentialDuration* defaultExponential = new DOMExponentialDuration(5, doc);
	
	map <QString, DOMDuration*> durations;
	
	durations [defaultFixed->getName()] = defaultFixed;
	durations [defaultNormal->getName()] = defaultNormal;
	durations [defaultUniform->getName()] = defaultUniform;
	durations [defaultExponential->getName()] = defaultExponential;

	//Create Atomic Effect Operator Display Names
	map<QString, QString> operatorDisplayNames;
	
	operatorDisplayNames [QString ("predicate")] = QString("set");
	operatorDisplayNames [QString ("not")] = QString ("remove");

	//Create Controller to interface with tool bars, menus.
	//and to control the action widget.
	BrazilControllerGroup* group = new BrazilControllerGroup ();

	//Create delete action.
	map <QString, DeleteModel*>deleters;
	
	DeleteDOMOutcome deleteDOMOutcome;
	DeleteDOMAtomicEffect deleteDOMAtomicEffect;
	
	deleters [QString ("DOMAtomicEffect")] = &deleteDOMAtomicEffect;
	deleters [QString ("DOMOutcome")] = &deleteDOMOutcome;
	
	DeleteAction* deleteAction = new DeleteAction(deleters);

	//Create new Action

	//First, parse prototypes.
	parser->parse ("gui/action-editor/new-prototypes/outcome.xml");
	
	DOMDocument* outcomeDocument = parser->getDocument ();
	DOMElement* outcomeElement = outcomeDocument->getDocumentElement ();
	
	parser->parse ("gui/action-editor/new-prototypes/effect.xml");
	DOMDocument* effectDocument = parser->getDocument ();
	DOMElement* effectElement = effectDocument->getDocumentElement ();
	
	//New Outcome Colours.
	vector<QColor> colours;
	colours.push_back( QColor ("#3366ff"));
	colours.push_back(QColor ("#33ff66"));
	colours.push_back(QColor ("#ffcc33"));
	colours.push_back(QColor ("#ff3366"));
	
	NewDOMOutcome newOutcome (*outcomeElement, doc, colours);
	NewDOMAtomicEffect newAtomicEffect (*effectElement,domain);
	NewDOMPredicateCondition newPredicateCondition (*domain);
	
	//Mapping from model type to NewAction handler for that type.
	map <QString, NewModel*> newModels;
	newModels [QString ("DOMOutcome")] = &newOutcome;
	newModels [QString ("DOMAtomicEffect")] = &newAtomicEffect;
	newModels [QString ("DOMAction")] = &newOutcome;
	newModels [QString ("DOMDelayedEffect")] = &newAtomicEffect;
	newModels [QString ("DOMCondition")] = &newPredicateCondition;
	
	//Create the new action using the handlers.
	NewAction* newAction = new NewAction(newModels);

	//Set action icons and short cuts
	QIcon deleteIcon ("delete.png");
	QIcon newIcon ("new.png");
	
	deleteAction->setIcon (deleteIcon);
	deleteAction->setShortcut (QKeySequence("Ctrl+D"));
	
	newAction->setShortcut (QKeySequence ("Ctrl+N"));
	newAction->setIcon (newIcon);

	NewConditionGroupAction* newConditionGroupAction = new NewConditionGroupAction ();
	newConditionGroupAction->setText ("New Condition Group");
	
	
	//Create changing colour actions
	ChangeDOMOutcomeColourAction* changeColourAction = new ChangeDOMOutcomeColourAction ();
	ChangeDOMActionColourAction* changeActionColourAction = new ChangeDOMActionColourAction ();
	
	
	//Create the controllergroup
	group->addController (deleteAction);
	group->addController (newAction);
	group->addController (changeColourAction);
	group->addController (changeActionColourAction);
	group->addController (newConditionGroupAction);
	
	//Create context menus
	QMenu* outcomeContextMenu = new QMenu ();
	outcomeContextMenu->addAction (changeColourAction);
	outcomeContextMenu->setDefaultAction (changeColourAction);
	
	QMenu* actionEffectContextMenu = new QMenu ();
	actionEffectContextMenu->addAction (changeActionColourAction);
	actionEffectContextMenu->setDefaultAction (changeActionColourAction);
	
	//Create ToolBar
	QToolButton* newButton = new QToolButton ();
	newButton->setDefaultAction (newAction);
		
	QToolButton* deleteButton = new QToolButton ();
	deleteButton->setDefaultAction (deleteAction);
	
	QToolButton* arrowButton = new QToolButton ();
	arrowButton->setArrowType (Qt::DownArrow);
	arrowButton->setPopupMode (QToolButton::InstantPopup);

	QMenu* arrowButtonMenu = new QMenu ();
	arrowButtonMenu->addAction (newConditionGroupAction);
	arrowButton->setMenu (arrowButtonMenu);
	arrowButton->setAutoRaise (true);
	
	QToolBar* toolBar = new QToolBar();
	toolBar->setMovable(false);
	toolBar->addWidget (deleteButton);
	toolBar->addWidget (newButton);
	toolBar->addWidget (arrowButton);
	
	//Create MainWindow
	QMainWindow* win = new QMainWindow();
	
	//Create ActionWidget
	int effectHeight = 50;
	int outcomeHeight = 50;
	int scale = 25;
	int outcomeSpacing = 5;
	
	cout << "create factories" << endl;
	DelayedEffectWidgetFactory* dewFactory = 
		new DelayedEffectWidgetFactory (effectHeight, operatorDisplayNames, 
		durations, domain, group, &durationNameList);
	
	OutcomeWidgetFactory* owFactory = 
		new OutcomeWidgetFactory (outcomeHeight, group, dewFactory);
	
	ActionEffectWidgetFactory* aewFactory = 
		new ActionEffectWidgetFactory (outcomeHeight, group, dewFactory);
	
	ActionEffectWidget* actionEffectWidget = 
		aewFactory->createActionEffectWidget (action, NULL, 25 / 3600);
	
	ActionWidget* widget = 
		new ActionWidget (NULL, action,outcomeSpacing, 25, 3600, domain, 
		actionEffectWidget, owFactory, group);
	
	widget->setBackgroundRole(QPalette::Base);
	widget->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
	QColor a1 ("#FFE6CC");
	QColor a2 ("#FFFFCC");
	widget->setAlternatingBackgroundColours (a1,a2);
	widget->setHotAreaColour (QColor("yellow"));
	widget->setHotAreaBorderColour (QColor ("black"));
	
	widget->setOutcomeContextMenu (outcomeContextMenu);
	widget->setActionEffectContextMenu (actionEffectContextMenu);
	
	win->setCentralWidget (widget);
	win->addToolBar (toolBar);
	win->setGeometry (100, 100, 760, 340);
	cout << "win show" << endl;
	win->show ();
	cout << "app exec" << endl;
	app->exec ();
	cout << "print domain" << endl;
	printDomain (domain->getDOMElement ());
}
*/



int main(int argc, char** argv)
{

	QApplication app(argc, argv);
	
	XMLPlatformUtils::Initialize();
	
	XercesDOMParser* parser = new XercesDOMParser();
	
	ErrorHandler* errHandler = (ErrorHandler*) new HandlerBase();
	parser->setErrorHandler(errHandler);
	
    try {
		parser->parse("gui/action-editor/new-prototypes/new-domain.xml");
	}
	catch (const XMLException& toCatch) {
		char* message = XMLString::transcode(toCatch.getMessage());
		cout << "Exception message is: \n"
			<< message << "\n";
		XMLString::release(&message);
		return -1;
	}
	catch (const DOMException& toCatch) {
		char* message = XMLString::transcode(toCatch.msg);
		cout << "Exception message is: \n"
			<< message << "\n";
		XMLString::release(&message);
		return -1;
	}
	
	DOMDocument* doc = parser->getDocument ();
	DOMElement* element = doc->getDocumentElement ();
	
	
	DOMDomain* domain = printDomain (element);
	
				
	list<DOMAction*> actions = domain->getActions();
	/*
	SideMenu* testMenu = new SideMenu ();
	
	QListWidget* testWidget = new QListWidget ();
	testWidget->addItem ("blkah");
	testWidget->addItem ("badf");
	
	testMenu->addItem ("Definition", testWidget);
	testMenu->addItem ("Overview", testWidget);
	testMenu->addItem ("Gantt Chart", testWidget);
	testMenu->addItem ("Bookmarks", testWidget);
	((QBoxLayout*)testMenu->layout())->addStretch (10);
	testMenu->show ();*/
	createBrazilApplication ( domain,parser, &app);
		
	XMLPlatformUtils::Terminate();
	//app.exec ();

}

