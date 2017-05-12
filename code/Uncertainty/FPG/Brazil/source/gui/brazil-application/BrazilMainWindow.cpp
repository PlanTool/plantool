/*
 *  BrazilMainWindow.cpp
 *  
 *
 *  Created by Owen Thomas on 6/09/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "BrazilMainWindow.h"
#include <QFileDialog>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QObject>
#include <QKeySequence>
#include "InitialResultBar.h"
#include <QDockWidget>
#include "SimpleErrorHandler.h"
#include "BrazilApplication.h"
#include <QString>
#include <QSplitter>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include <QSizePolicy>
#include <QScrollArea>

#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "../action-editor/ActionWidgetFactory.h"
#include "../../model/impl/DOMDomain.h"
#include "BrazilPlanningWrapperImpl.h"



#include <QSize>
  
BrazilMainWindow::BrazilMainWindow (DOMDomain& domain, QMainWindow* actionEditorWindow, 
	ActionWidgetFactory* awFactory, XercesDOMParser* parser, BrazilConfiguration* config,
	QString newDomainFileName) {

	//XML parser used to read in domain files the user wants to open.
	this->parser = parser;
	this->domain = &domain;
	this->config = config;
	this->newDomainFileName = newDomainFileName;
	
	planner = new BrazilPlanningWrapperImpl (domain);
	medianStates = new BrazilStateList (1000); //change to use property value.
	stochasticStates = new BrazilStateList (domain.getProperties()->getPropertyInt("max states in stats view")); //change to use property value.
	
	//Main Menu.
	QMenuBar* menu = new QMenuBar ();
	setMenuBar (menu);
	QMenu* fileMenu = new QMenu ("File");
	
	//I assume that fileMenu takes controls of these actions and will delete them.
	QAction* newDomainAction = fileMenu->addAction (QObject::tr ("New"));
	QAction* openDomainAction = fileMenu->addAction (QObject::tr ("Open"));
	QAction* saveDomainAction = fileMenu->addAction (QObject::tr ("Save"));
	QAction* saveAsDomainAction = fileMenu->addAction (QObject::tr ("Save As"));
	
	newDomainAction->setShortcut (QKeySequence ("Ctrl+N"));
	openDomainAction->setShortcut (QKeySequence ("Ctrl+O"));
	saveDomainAction->setShortcut (QKeySequence ("Ctrl+S"));
	
	menu->addMenu (fileMenu);

	//ResultBar.
	resultBar = new InitialResultBar (domain, NULL);
	QDockWidget* resultBarDock = new QDockWidget ();
	resultBarDock->setFloating (false);
	
	
	//This displays the current widget [gantt bar, brazil definition]
	QWidget* rightSplitterWidget = new QWidget ();
	
	//Stacked layout because only one is visible at a time.
	rightSplitterLayout = new QStackedLayout ();
	rightSplitterWidget->setLayout (rightSplitterLayout);
	
	QWidget* centralWidget = new QWidget ();
	centralWidget->setLayout (new QVBoxLayout ());
	
	QSplitter* splitter = new QSplitter (centralWidget);

	//1. BrazilApplication.
	brazilDefinition = new BrazilApplication (domain, actionEditorWindow, awFactory);
	
	//Gantt chart
	this->ganttChart = new GanttChart (*medianStates,domain,  rightSplitterWidget);
	
	// Statistics
	this->statsWidget = new StatsWidget(rightSplitterWidget, &domain);
	
	ganttChart->setVisible (true);
	
	//Side menu
	this->sideMenu = new SideMenu();
	
	sideMenu->addItem ("Problem", brazilDefinition);
	sideMenu->addItem ("Statistics", statsWidget);
	sideMenu->addItem ("Gantt Chart", ganttChart);
	sideMenu->addItem ("Bookmarks", NULL);
	
	
	rightSplitterWidget->layout()->addWidget (brazilDefinition);
	rightSplitterWidget->layout()->addWidget (ganttChart);
	rightSplitterWidget->layout()->addWidget (statsWidget);
	
	
	splitter->addWidget (sideMenu);
	splitter->addWidget (rightSplitterWidget);
	
	
	centralWidget->layout()->addWidget (splitter);
	centralWidget->layout()->addWidget (resultBar);
		
	((QBoxLayout*)centralWidget->layout())->setStretchFactor(resultBar, 0);
	
	((QBoxLayout*)centralWidget->layout())->setStretchFactor(splitter, 10);
	
	setCentralWidget (centralWidget);
	
	//Connect action slots.
	QObject::connect (openDomainAction, SIGNAL (triggered (bool)), this, 
		SLOT (openDomain ()));
		
	QObject::connect (saveDomainAction, SIGNAL (triggered (bool)), this,
		SLOT (saveDomain ()));
	
	QObject::connect (saveAsDomainAction, SIGNAL (triggered (bool)), this,
		SLOT (saveAsDomain ()));
		
	QObject::connect (newDomainAction, SIGNAL (triggered (bool)), this,
		SLOT (newDomain ()));
	
	//Connect menu slots
	QObject::connect (sideMenu, SIGNAL (selectedWidgetChanged (QWidget*)),
		rightSplitterLayout, SLOT (setCurrentWidget (QWidget*)));
		
	//Connect planning slots
	QObject::connect (resultBar, SIGNAL (plan ()), planner, SLOT (start ()));
	QObject::connect (resultBar, SIGNAL (pause()), planner, SLOT (stop ()));
	
	//Toggle editing of gantt chart when paused or planning
	QObject::connect (resultBar, SIGNAL (plan ()), ganttChart, 
		SLOT (disableEditing ()));

	QObject::connect (resultBar, SIGNAL (pause()), ganttChart,
		SLOT (enableEditing ()));

	// [daa] Connect stats widget to Gantt chart
	QObject::connect (statsWidget, SIGNAL (viewState (BrazilState*)),
		medianStates, SLOT (addState(BrazilState*)));


	// Connect up the statsWidget to be informed of new states
	QObject::connect (stochasticStates, SIGNAL (stateAdded (BrazilState*)),
			  statsWidget, SLOT(newState(BrazilState*)));
	
	// Connect up the statswidget to  be informed of states being deleted
	QObject::connect (stochasticStates, SIGNAL (aboutToDelete(BrazilState*)),
			  statsWidget, SLOT(removeState(BrazilState*)));
	
	//Connect planning wrapper to median states and stochastic states.
	QObject::connect (planner, SIGNAL (newMedianState (BrazilState*)),
		medianStates, SLOT (addState(BrazilState*)));
	
	QObject::connect (planner, SIGNAL (newState(BrazilState*)),
			 stochasticStates, SLOT(addState(BrazilState*)));
	
	QObject::connect (ganttChart, SIGNAL (requestSimulationFrom (BrazilState*)),
		planner, SLOT (medianPlan (BrazilState*)));
}


void BrazilMainWindow::openDomain () {
	QString domainFileName = QFileDialog::getOpenFileName(
                    this,
                    "Choose a file",
                    config->getDefaultDir (),
                    "Domains (*.xml)");
					
	
	if(!domainFileName.trimmed().size()) return;
	
	SimpleErrorHandler* handler = new SimpleErrorHandler ();
	parser->setErrorHandler (handler);

	try {
		//parser->setValidationScheme (AbstractDOMParser::Val_Always);
		//parser->setValidationConstraintFatal (true);
		//parser->setExitOnFirstFatalError (true);
		parser->parse (domainFileName.toUtf8().data());
		
	} catch (const SAXParseException& ex) {
		cout << XMLString::transcode (ex.getMessage ()) << endl;
	}

	list<char*> messages = handler->getMessages ();
	list<char*>::iterator it;
	for(it = messages.begin (); it != messages.end(); it++) {
		cout << *it << endl;
	}
	
	DOMDocument* doc = parser->getDocument ();
	DOMElement* element = doc->getDocumentElement ();
	
	DOMDomain* nextDomain = new DOMDomain (element);
	
	setDomain (*nextDomain);
	
	this->openFileName = domainFileName;
	config->setDefaultDir (openFileName);
}


void BrazilMainWindow::saveDomain () {
	if(!openFileName.trimmed().size())  {
		saveAsDomain ();
		return;
	}
	
	DOMImplementation* impl  = DOMImplementation::getImplementation ();
	DOMWriter* writer = impl->createDOMWriter ();
 	// [daa] Format the output nicely
	writer->setFeature(XMLString::transcode("format-pretty-print"), true);
	
	XMLFormatTarget* fileTarget = new LocalFileFormatTarget (openFileName.toUtf8 ().data ());
	
	DOMNode* node = domain->getDOMElement ();
	
	writer->writeNode (fileTarget, *node);
	
	delete writer;
	delete fileTarget;

}

void BrazilMainWindow::newDomain () {
	openFileName = "";	
	parser->parse (newDomainFileName.toUtf8().data ());
	
	DOMDocument* doc = parser->getDocument ();
	DOMElement* element = doc->getDocumentElement ();
	
	DOMDomain* nextDomain 
		= new DOMDomain (element);
	setDomain (*nextDomain);
}

void BrazilMainWindow::saveAsDomain () {
	QString defaultName;
	if(openFileName.trimmed().size()) {
		defaultName = openFileName;
	} else {
		defaultName = "domain.xml";
	}
	QString saveFileName = QFileDialog::getSaveFileName (
							this,
							"Save Domain",
							defaultName,
							"Domains (*.xml)");
							
	if(!saveFileName.trimmed().size()) return;
	if(!defaultName.trimmed().size()) return;
	
	
	//Piggy back on the saveDomain () method.
	openFileName = saveFileName;
	
	saveDomain ();

}
void BrazilMainWindow::setDomain (DOMDomain& newDomain) {
	DOMProblem* problem = newDomain.getProblem ();
	set<DOMPredicate*> predicates = problem->initiallyValidPredicates();
	set<DOMPredicate*>::iterator it;
	
	medianStates->clear();
	stochasticStates->clear();

	this->planner->setDomain (newDomain);
	this->brazilDefinition->setDomain (newDomain);
	this->ganttChart->setDomain (newDomain);
	this->resultBar->setDomain (newDomain);
	this->statsWidget->setDomain (&newDomain); // [daa]
	
	delete this->domain;
	this->domain = &newDomain;
	//More to come..
}

