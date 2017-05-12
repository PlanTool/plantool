/*
 *  GanttBarContainer.cpp
 *  
 *
 *  Created by Owen Thomas on 15/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "GanttBarContainer.h"
#define minTime(a,b) (((a)<(b))?(a):(b))
#define maxTime(a,b) (((a)>(b))?(a):(b))
#include <limits>

#include "../model/impl/DOMDuration.h"
#include "../model/impl/DOMEffectSet.h"

GanttBarContainer::GanttBarContainer (BrazilEvent* startAction,
				      int timeScaling, GanttChart* chart, QWidget* parent) : QWidget (parent) {
	
    this->startAction = startAction;
    this->timeScaling = timeScaling;
    this->chart = chart;
	
    barHeight = 25;
    vMargin = 2;
	
    endAction = NULL;
    startOutcome = NULL;
    endOutcome = NULL;
	
    startActionEffect = NULL;
    endActionEffect = NULL;
    startOutcomeEffect = NULL;
    endOutcomeEffect = NULL;
	
    allEvents.insert (startAction);
	
    actionBar = NULL;
    outcomeBar = NULL;
	
    actionLeftResizer = actionRightResizer = outcomeRightResizer = NULL;
	
    drawTopLine = true;
    drawBottomLine = false;
    drawRightLine = true;
    drawLeftLine = false;
	
    //Gantt Bar Containers are editable unless otherwise instructed.
    //see setEditable
    editable = true;
}

/**
 * GanttBar has received a mouseEvent event, set the appropriate
 * reziers to be framed.
 */
void GanttBarContainer::enter (GanttBar* bar) {
    if(bar == actionBar) {
	actionLeftResizer->setFrameStyle(QFrame::Box | QFrame::Plain);
	actionRightResizer->setFrameStyle(QFrame::Box | QFrame::Plain);
    } else if (bar == outcomeBar) {
	outcomeRightResizer->setFrameStyle(QFrame::Box | QFrame::Plain);
    }
}

/**
 * The opposite of enter () (above), the mouse has left the GanttBar.
 * set the appropriate resizer to be unframed.
 */
void GanttBarContainer::leave (GanttBar* bar) {
    if(bar == actionBar) {
	actionLeftResizer->setFrameStyle (QFrame::NoFrame);
	actionRightResizer->setFrameStyle (QFrame::NoFrame);
    } else if (bar == outcomeBar) {
	outcomeRightResizer->setFrameStyle (QFrame::NoFrame);
    }
}
 
void GanttBarContainer::paintEvent (QPaintEvent* event) {
    QPainter painter(this);
			
    if(drawTopLine) {
	painter.drawLine (0, 0, width(), 0);
    }
    if(drawBottomLine) {
	painter.drawLine (0, height()-1, width(), height()-1);
    }
    if(drawLeftLine) {
	painter.drawLine (0, 0, 0, height());
    }
    if(drawRightLine) {
	painter.drawLine (width()-1, 0, width()-1, height());
    }
	
    QWidget::paintEvent (event);
}

/**
 * Called when resizer's position has changed, updates the gantt bar(s) 
 * positions.
 */
void GanttBarContainer::resizerPositionModified 
(GanttBarResizer* resizer) {
	
	
    if(resizer == actionLeftResizer) {
	int d = actionBar->x() - (actionLeftResizer->x() + actionLeftResizer->width());

	actionBar->setGeometry (actionBar->x() - d, actionBar->y(), 
				actionBar->width () + d, actionBar->height ());
    }
    if(resizer == actionRightResizer) {
		 
	actionBar->setGeometry (actionBar->x(), actionBar->y(), 
				actionRightResizer->x() - actionBar->x(), actionBar->height ());
    }
    if(resizer == outcomeRightResizer) {
		 
	outcomeBar->setGeometry (outcomeBar->x(), outcomeBar->y(), 
				 outcomeRightResizer->x() - outcomeBar->x() , outcomeBar->height ());
    }
}

/**
 * Similar to the above, except a gantt bar has moved.
 */
void GanttBarContainer::barPositionModified (GanttBar* bar) {
    if(bar == actionBar) {
	actionLeftResizer->move (actionBar->x() - actionLeftResizer->width(), actionLeftResizer->y());
	if(outcomeBar == NULL) {
	    actionRightResizer->move (actionBar->x() + actionBar->width(), 
				      actionLeftResizer->y());
	}
	else {
	    outcomeBar->move (actionBar->x() + actionBar->width(), actionLeftResizer->y());
	    outcomeRightResizer->move (outcomeBar->x() + outcomeBar->width(), 
				       actionLeftResizer->y());
	}
		
    }
    else {
	actionLeftResizer->move (outcomeBar->x() - 
				 (actionBar->width() + actionLeftResizer->width()), actionLeftResizer->y());
	actionBar->move (actionLeftResizer->x() + actionLeftResizer->width(), actionLeftResizer->y());
	outcomeRightResizer->move (outcomeBar->x() + outcomeBar->width(),
				   actionLeftResizer->y());
    }
}


/**
 * Update states based on positions of gantt bars.
 * If the underlying state has changed this triggers a call to
 * positionChanged in the GanttChart.
 */
void  GanttBarContainer::commitModification () { 
	
    int leftInvalidTime = std::numeric_limits<int>::max ();
    BrazilEvent* triggerEvent = NULL;

    bool invalidTimeFound = false;
	
    // I think the order that these could be called in are important.
    if(startActionModified () ) {
	cout << "start action mod." << endl;
	int newTime = minTime(startAction->timeToOccur, actionBar->x() * timeScaling);
	if (leftInvalidTime > newTime) {
	    leftInvalidTime = newTime;
	    triggerEvent = startAction;
	}
	
	assert(leftInvalidTime >= 0);
		
	invalidTimeFound = true;
	startAction->timeToOccur = actionBar->x() * timeScaling;
	if(startActionEffect) startActionEffect->timeToOccur = startAction->timeToOccur;
    }
	
    if(endActionModified ()) {
	cout << "end action mod." << endl;

	int newTime = minTime(endAction->timeToOccur, 
			      timeScaling * actionBar->x() + timeScaling * actionBar->width());
	
	if (leftInvalidTime > newTime) {
	    leftInvalidTime = newTime;
	    triggerEvent = endAction;
	}


	assert(leftInvalidTime >= 0);

	invalidTimeFound = true;
	endAction->timeToOccur = timeScaling * actionBar->x() + timeScaling * actionBar->width ();
		
	if(endActionEffect) endActionEffect->timeToOccur = endAction->timeToOccur;
    }
	
    if(startOutcomeModified ())  {
	cout << "start outcome mod." << endl;
		
	int newTime = minTime(startOutcome->timeToOccur, outcomeBar->x() * timeScaling);
	if (leftInvalidTime > newTime) {
	    leftInvalidTime = newTime;
	    triggerEvent = startOutcome;
	}	

	invalidTimeFound = true;
	startOutcome->timeToOccur = (outcomeBar->x()) * timeScaling;
	if(startOutcomeEffect) startOutcomeEffect->timeToOccur 
				   = startOutcome->timeToOccur;
    }
	
    if(endOutcomeModified ())  {
	cout << "end outcome mod." << endl;
		
	int newTime = minTime(endOutcome->timeToOccur, 
			      timeScaling * outcomeBar->x() + timeScaling * outcomeBar->width ());
	if (leftInvalidTime > newTime) {
	    leftInvalidTime = newTime;
	    triggerEvent = endOutcome;
	}

	assert(leftInvalidTime >= 0);		

		
	invalidTimeFound = true;
	endOutcome->timeToOccur = timeScaling * outcomeBar->x() + 
	    timeScaling * outcomeBar->width ();
		
	if(endOutcomeEffect) {
	    endOutcomeEffect->timeToOccur = endOutcome->timeToOccur;
	}
    }
	
    if (invalidTimeFound) {
	chart->positionChanged (*this, leftInvalidTime, triggerEvent);
    }
}


/**
 * Call nextOutcome on the GanttChart this container is 
 * displayed within.
 */
void GanttBarContainer::nextOutcome () {

    // startOutcome is an event type, but nextOutcome is an Outcome DOM element.
    if (!startOutcome) return;
    DOMProbabilistic* probabilistic = ((DOMOutcome*)startOutcome->element)->getProbabilistic ();	
    DOMOutcome* nextOutcome = (DOMOutcome*)startOutcome->element;
	
	
    /**
     * Find the next outcome (sequentially).
     */
    bool foundCurrentOutcome = false;
	
    map<DOMOutcome*, double>::iterator it;
	
    for(it = probabilistic->getOutcomes ().begin (); 
	it != probabilistic->getOutcomes().end(); it++) {
	
	if(it->first == startOutcome->element) foundCurrentOutcome = true;
	else if (foundCurrentOutcome) {
	    nextOutcome = it->first;
	    break;
	}
    }
	
    //the currently displayed outcome is the last, circle around and display the first.
    if(nextOutcome == startOutcome->element) nextOutcome = probabilistic->getOutcomes ().begin ()->first;
	
    startOutcome->element = nextOutcome;

    //We can just use the standard position changed call to render
    //the gannt chart.
    chart->positionChanged (*this, startOutcome->timeToOccur, startOutcome);

}


/**
 * The current endAction event must not be defined and 
 * endAction must be of type END_ACTION.
 *
 * This will cause a GanttBar to be rendered in the container
 * representing the duration of the action.
 *
 */
void GanttBarContainer::setEndAction (BrazilEvent* endAction) {
	
    assert (this->endAction == NULL);
    assert (endAction->type == BrazilEvent::END_ACTION);
	
    this->endAction = endAction;
    allEvents.insert (endAction);
	
    QColor backgroundColour 
	( ((DOMAction*)endAction->element)->getBackgroundColour ());
	
    actionBar = new GanttBar (backgroundColour, this);
		
    int actionBarX = startAction->timeToOccur / timeScaling;
    int actionBarY = vMargin;
    int scaledWidth = 
	endAction->timeToOccur / timeScaling - startAction->timeToOccur / timeScaling;
		
    actionBar->setVisible (true);
    actionBar->setGeometry (actionBarX, actionBarY, scaledWidth, barHeight);
    actionBar->setEditable (editable);
	
    actionLeftResizer = new GanttBarResizer (this, GanttBarResizer::LEFT, 0, actionBar->x() + actionBar->width ());
    actionRightResizer = new GanttBarResizer (this, GanttBarResizer::RIGHT,actionBar->x());

    actionLeftResizer->setVisible (editable);
    if(endOutcome != NULL) actionRightResizer->setVisible (editable);
	
	
    actionLeftResizer->move (actionBarX - actionLeftResizer->width(), vMargin);
    if(endOutcome != NULL) actionRightResizer->move (actionBarX + scaledWidth, vMargin);
	
	
    QObject::connect (actionBar, SIGNAL (moved (GanttBar*)), this,
		      SLOT (barPositionModified (GanttBar*)));
	
    QObject::connect (actionLeftResizer, SIGNAL (moved (GanttBarResizer*)), this,
		      SLOT (resizerPositionModified (GanttBarResizer*)));
		
    QObject::connect (actionRightResizer, SIGNAL (moved (GanttBarResizer*)), this,
		      SLOT (resizerPositionModified (GanttBarResizer*)));
	
    setMaximumHeight (2 * vMargin + actionBar->height());
    setMinimumHeight (maximumHeight());
	
    setWidth ();
}

void GanttBarContainer::setStartOutcome (BrazilEvent* startOutcome) {
    assert(this->startOutcome == NULL);
    assert(startOutcome->type == BrazilEvent::START_OUTCOME);
    this->startOutcome = startOutcome;
    allEvents.insert (startOutcome);
}

void GanttBarContainer::setEndOutcome (BrazilEvent* endOutcome) {
    assert(this->endOutcome == NULL);
    assert(endOutcome->type == BrazilEvent::END_OUTCOME);
	
    this->endOutcome = endOutcome;
    allEvents.insert (endOutcome);
	
    QColor backgroundColour  
	( ((DOMOutcome*)endOutcome->element)->getBackgroundColour ());
	
    outcomeBar = new GanttBar (backgroundColour, this);
		
    if(actionRightResizer != NULL) actionRightResizer->setVisible (false);
	
    outcomeBar->setVisible (true);
    outcomeBar->setEditable (editable);
	
    int outcomeBarX = startOutcome->timeToOccur / timeScaling;
    int outcomeBarY = vMargin;
    int scaledWidth = 
	endOutcome->timeToOccur / timeScaling - startOutcome->timeToOccur / timeScaling;
	
    outcomeBar->setGeometry (outcomeBarX, outcomeBarY, 
			     scaledWidth, barHeight);

    outcomeRightResizer = new GanttBarResizer (this, GanttBarResizer::RIGHT, outcomeBar->x());

    outcomeRightResizer->setVisible (editable);
	
    outcomeRightResizer->move (outcomeBarX + scaledWidth, vMargin);
	
    QObject::connect (outcomeBar, SIGNAL (moved (GanttBar*)), this,
		      SLOT (barPositionModified (GanttBar*)));
		
    QObject::connect (outcomeRightResizer, SIGNAL (moved (GanttBarResizer*)), this,
		      SLOT (resizerPositionModified (GanttBarResizer*)));
	
    setWidth ();
}

void GanttBarContainer::setStartActionEffect (BrazilEvent* startActionEffect) {
    assert (startActionEffect->timeToOccur == startAction->timeToOccur);
    this->startActionEffect = startActionEffect;
    allEvents.insert (startActionEffect);
}

void GanttBarContainer::setEndActionEffect (BrazilEvent* endActionEffect) {
    if(endAction)assert (endActionEffect->timeToOccur == endAction->timeToOccur);
    this->endActionEffect = endActionEffect;
    allEvents.insert (endActionEffect);
}

void GanttBarContainer::setStartOutcomeEffect (BrazilEvent* startOutcomeEffect) { 
    assert(startOutcome->timeToOccur == startOutcomeEffect->timeToOccur);
    this->startOutcomeEffect = startOutcomeEffect;
    allEvents.insert (startOutcomeEffect);
}

void GanttBarContainer::setEndOutcomeEffect (BrazilEvent* endOutcomeEffect) {
    if(endOutcome)	assert (endOutcome->timeToOccur == endOutcomeEffect->timeToOccur);
    this->endOutcomeEffect = endOutcomeEffect;
    allEvents.insert (endOutcomeEffect);
}

bool GanttBarContainer::operator<(const GanttBarContainer& other) {
    return this->startAction->timeToOccur < other.startAction->timeToOccur;
}

bool GanttBarContainer::startActionModified () {
	
    if(actionBar->x() * timeScaling < startAction->timeToOccur - timeScaling 
       || actionBar->x() * timeScaling > startAction->timeToOccur + timeScaling ) return true;
	
    return false;
}

bool GanttBarContainer::endActionModified () {
    if(NULL == endAction) return false;
	
    if(actionBar->x() * timeScaling + actionBar->width() * timeScaling < endAction->timeToOccur - timeScaling
       || actionBar->x() * timeScaling + actionBar->width() * timeScaling > endAction->timeToOccur + timeScaling) return true;
		
    return false;
}

bool GanttBarContainer::startOutcomeModified () {
    if(NULL == startOutcome) return false;
	
    if(outcomeBar->x() * timeScaling < startOutcome->timeToOccur - timeScaling 
       || outcomeBar->x() * timeScaling > startOutcome->timeToOccur + timeScaling ) return true;
	
    return false;
}

bool GanttBarContainer::endOutcomeModified () {
    cout << "in end outcome modified, is endOutcome NULL: " << endOutcome << endl;
	
    if(NULL == endOutcome) return false;
	
    if(outcomeBar->x() * timeScaling + outcomeBar->width() * timeScaling < endOutcome->timeToOccur - timeScaling
       || outcomeBar->x() * timeScaling + outcomeBar->width() * timeScaling > endOutcome->timeToOccur + timeScaling) return true;
	
    return false;
}

time_t GanttBarContainer::getEndTime () {
    time_t endTime = 0;
    set<BrazilEvent*>::iterator it;
    for(it = allEvents.begin(); it != allEvents.end(); it++) {
	if( (*it)->timeToOccur > endTime) endTime = (*it)->timeToOccur;
    }
    return endTime;
}

