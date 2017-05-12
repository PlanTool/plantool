/*
 * $Id$
 *
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 *
 */

#include<iostream>

#include <QPainter>
#include <QRect>
#include"DOMPropertySet.h"

#include"StatsWidget.h"




/**
 * Create StatsWidget
 */
StatsWidget::StatsWidget(QWidget* parent, DOMDomain* d) : QFrame(parent), domain(d) {
    
    setFocusPolicy(Qt::ClickFocus);

    setFrameShape(QFrame::Box);

    props = domain->getProperties();
    double axisPad = props->getPropertyDouble("gui stats axis pad");

    successColor = QColor(props->getPropertyString("gui stats success colour"));
    failureColor= QColor(props->getPropertyString("gui stats failure colour"));
    
    // Create area for data points.
    graph = new QWidget(this);

    // Resize. 2*axisPad leaves room for ComboBox at top.    
    // Move it inside axes
    graph->resize((int)((1.0-axisPad)*geometry().width()), (int)((1.0 - 2.0*axisPad)*geometry().height()));
    graph->move((int)(axisPad*geometry().width()), (int)(axisPad*geometry().height()));    

    graph->raise();
    graph->show();

    // Drop down box with function names
    functionList = new QComboBox(this);
    repopulateFunctionList();
    functionList->raise();
    functionList->show();


    // Connect list box to stats widget 
    QObject::connect(functionList, SIGNAL(currentIndexChanged (const QString&)), 
		     this, SLOT(changeFunction(const QString&)));


    maxTime=1;
    yMax=1.0;
    yMin=0;




}


/**
 * Remove all points. Frees memory associated with points.
 */
StatsWidget::~StatsWidget() {
    
    for (Points::iterator it = points.begin(); it != points.end(); it++) {
	removeState(it->first);
    }

}


/**
 * Slot for adding a new state. Checks if the state really is new. If yes,
 * add it to the map and create a new graphical point.
 * Ask for resize of axis if necessary.
 */
void StatsWidget::newState(BrazilState* s) {
	
    // Only add the point if it really is new.
    if (points.find(s) == points.end()) {
	int radius = props->getPropertyInt("gui HotAreaWidget radius");
	HotAreaWidget* point = new HotAreaWidget(radius, 
						 s->isSuccessful()?successColor:failureColor, 
						 s->isSuccessful()?true:false,
						 true, 
						 graph);
	points[s] = point;
	point->raise();
	point->show();	
	// Locate point will return true if axes changed.

	// Create communication between clicked points back to statsWidget
	QObject::connect(point, SIGNAL(doubleClicked(HotAreaWidget*)),
			 this, SLOT(pointChosen(HotAreaWidget*)));

	if (locatePoint(s, point)) {
	    repositionAll();
        }

	update();

    }

}


/**
 * Put a particular point where it is supposed to be given current axes lengths.
 * If a point exceeds current axis lengths, axis length will be increased to
 * compensate. An increase will return true, which should in turn cause all
 * points to be shifted appropriately.
 */
bool StatsWidget::locatePoint(BrazilState* s, HotAreaWidget* p) {
    
    bool axisChange = false;

    int radius = props->getPropertyInt("gui HotAreaWidget radius");

    DOMFunction* targetFunc = NULL;
    if (functionMap.size() > 0 && functionList->currentText().length() > 0) {
									
	targetFunc = functionMap[functionList->currentText().toStdString().c_str()];
    }

    size_t time = s->getTime();

    if (time > maxTime) {
	maxTime = time;
	axisChange = true;
    }
    double x = s->getTime()/(double)maxTime;

    double val = (targetFunc!=NULL)?s->getResourceLevel(targetFunc):0.0;
    if (val > yMax) {
	yMax = val;
	axisChange = true;
    }
    else if (val < yMin) {
	yMin = val;
	axisChange = true;
    }
    double y = (double)(val - yMin)/(double)(yMax - yMin);

    int height = graph->geometry().height();
    int width = graph->geometry().width();

    p->move((int)(width*x - radius),(int)(height*(1.0-y) - radius));

    return axisChange;
}


/**
 * Called if adding a point has shifted an axis. Moves all points to the correct
 * co-ordinates in the graph object.
 */
void StatsWidget::repositionAll() {

    for (Points::const_iterator it = points.begin(); it != points.end(); it++) {
	locatePoint(it->first, it->second);
    }

}


/**
 * A state has become too old, or otherwise not wanted for  plotting.
 * This is a slot
 */
void StatsWidget::removeState(BrazilState* s) {

    Points::iterator p = points.find(s);

    if (p != points.end()) {
	// Not sure if something needs to be done here to 
	// cleanly remove a widget
	HotAreaWidget* dot=p->second;

	// Disconnect communication between clicked points back to statsWidget
	QObject::disconnect(dot, SIGNAL(doubleClicked(HotAreaWidget*)),
			 this, SLOT(pointChosen(HotAreaWidget*)));

	points.erase(p);
	delete dot;	
    }
    
}


/**
 * Redraw this widget
 */
void StatsWidget::paintEvent(QPaintEvent* event) {

    int width = event->rect().width();
    int height = event->rect().height();

    assert(width > 0 && height > 0);

    QPainter painter(this);

    int lw = props->getPropertyInt("gui stats axis width");

    QPen axisPen(QColor("black"), 
		 lw,
		 Qt::SolidLine,
		 Qt::RoundCap,
		 Qt::MiterJoin);

    painter.setPen(axisPen);

    double axisPad = props->getPropertyDouble("gui stats axis pad");

    // Draw X axis
    painter.drawLine(0, (int)((1.0-axisPad)*height), width, (int)((1.0-axisPad)*height));

    // Draw Y axis
    painter.drawLine((int)(axisPad*width), (int)(axisPad*height), (int)(axisPad*width), height);
    
    // Draw Y ticks
    int ticks = props->getPropertyInt("gui stats axis y-ticks");
    for (int i=0; i <= ticks; i++) {
	QString tickTxt = QString::number((yMax - yMin)*i/ticks + yMin);
	painter.drawText((int)(axisPad*width)-41, (int)(((ticks - i)/(double)ticks*(1.0 - 2*axisPad) + axisPad)*height) , 40, 20, Qt::AlignRight, tickTxt); 
    }

}


/**
 * Update the function map and repopulate the combo box
 * with the names of functions
 */
void StatsWidget::repopulateFunctionList() {

    functionMap = domain->getFunctionMap();
    functionList->clear();
    for (DOMDomain::FunctionMapCIt it = functionMap.begin();
	 it != functionMap.end();
	 it++) {
	functionList->addItem(QString(it->first));
    }
}

/**
 * The user is about to see the StatsWidget, so update the list of
 * available function names
 */
void StatsWidget::showEvent (QShowEvent *) {

    repopulateFunctionList();
    cout<<"Showing statswidget\n";

}

   
/**
 * Stats widget just got focus. What do we do about it.
 */
void StatsWidget::resizeEvent (QResizeEvent*) {

    double axisPad = props->getPropertyDouble("gui stats axis pad");

    graph->resize((int)((1.0-axisPad)*geometry().width()), (int)((1.0 - 2.0*axisPad)*geometry().height()));
    graph->move((int)(axisPad*geometry().width()), (int)(axisPad*geometry().height()));

    repositionAll();

}




/** 
 * User has loaded a new domain. Tell the statswidget this.
 */
void StatsWidget::setDomain(DOMDomain* domai) {

    domain = domai;
    props = domain->getProperties();

	cout << "populate function list." << endl;
    // Refresh list of avaiable functions.
    repopulateFunctionList();
	cout << "populate done." << endl;

}


/**
 * A slot used by all the hot area widgets to inform Stats view that a
 * point was double clicked.
 */
void StatsWidget::pointChosen(HotAreaWidget* p) {

    // Do inverse map lookup to find corresponding state
    
    for (Points::iterator it = points.begin(); it != points.end(); it++) {
	if (it->second == p) {
	    std::cout<<"Rendering state:"<<*(it->first)<<endl;
	    emit(viewState(it->first));
	    return;
	}
    }

    throw std::runtime_error("StatsWidget::pointChosen() Clicked unknown data point");

}


void StatsWidget::changeFunction(const QString&) {

    repositionAll();
    update();

}
