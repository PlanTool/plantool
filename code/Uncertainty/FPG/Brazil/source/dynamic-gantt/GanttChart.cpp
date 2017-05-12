/*
 *  GanttChart.cpp
 *  
 *
 *  Created by Owen Thomas on 15/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "GanttChart.h"
#include "GanttNameArea.h"
#include "GanttBarArea.h"
#include "GanttBarContainer.h"
#include <QScrollArea>

#include "../model/impl/DOMProbabilistic.h"
#include "../model/impl/DOMOutcome.h"

GanttChart::GanttChart (BrazilStateList& medianStates, 
			DOMDomain& domain,  QWidget* parent, int scaling) : QWidget (parent) {
	
    this->domain = &domain;	//Initial change to the new BSL, change in future to having this
    //passed in.
    this->medianStates = &medianStates;
    this->drawnStates = new BrazilStateList (40);
    this->scaling = scaling;
    this->isDrawingState = false;
	
    //Not editable by default.
    this->editable = false;
	
    //Gantt bars are arranged vertically, but this layout contains
    //the name area (on the left) and the widget containing gantt bars
    //on right.
    QHBoxLayout* hlayout = new QHBoxLayout ();
    setLayout (hlayout);
	
    QScrollArea* scrollArea = new QScrollArea (this);
    hlayout->addWidget (scrollArea);
	
    childContainer = new QWidget (scrollArea);
	
    chlayout = new QHBoxLayout ();
    chlayout->setSizeConstraint (QLayout::SetMinAndMaxSize);
    childContainer->setLayout (chlayout);
	
    ganttNameArea = new GanttNameArea (childContainer);
    ganttBarArea = new GanttBarArea (childContainer);
	
    chlayout->addWidget (ganttNameArea);
    chlayout->addWidget (ganttBarArea);
	
    ganttNameArea->setVisible (true);
    ganttBarArea->setVisible (true);
	
    scrollArea->setWidget (childContainer);
	
    childContainer->setVisible (true);
	
    scrollArea->setVisible (true);
	
    setVisible (true);
    setAutoFillBackground (true);
	
    //Listen on BrazilStateList signal.
    QObject::connect (&medianStates, SIGNAL (stateAdded (BrazilState*)),
		      this, SLOT (handleNewMedianState (BrazilState*)));
}


void GanttChart::setDomain (DOMDomain& domain) {
    this->domain = &domain;
    //Clear undo / redo.
    //Clear current state.
    //Stop planning - or fail if planning. I think fail if planning,
    //could be too complicated otherwise. 
	
    //Clear gantt name and gantt bar areas.
    //domain already set on planning wrapper.
	
    //What if the domain already has parameters...
}


/**
 * Draw the gantt chart from state, only considering events
 * that occur at or after time.
 */
void GanttChart::drawFromState (BrazilState* state, int time)
{
    //Go through each past and future event that occurs
    //after time and draw it (through calls to visit ()).
	
    //past events..
    for(BrazilState::EventsCIt it = state->getPast().begin(); 
	it != state->getPast().end(); it++) {
	if(it->first->timeToOccur >= time )
	    visit ( it->second);
    }
	
    //future events..
    for(BrazilState::EventsCIt it = state->getFuture().begin(); 
	it != state->getFuture().end(); it++) {
	if(it->first->timeToOccur >= time && NULL != it->second)
	    visit ( it->second);
    }
}

//
//BrazilEventVisitor implementation..
//
//Used to actually draw the gantt chart.
//
//These are called from within the visit () method of 
//BrazilEventVisitor
//drawFromState () calls visit with each Event it wants the
//gantt chart to display.
//


void GanttChart::visitStartAction (BrazilEvent* event, DOMAction*) {

    // The planner ignores start actions that don't meet a 
    // second check of their preconditions. There will ne no
    // future events scheduled for this event->actionCopy
    if (event->ignored) return;

    //Create a new container for action.
    GanttBarContainer* container = new GanttBarContainer (event,
							  100, this, ganttBarArea);
	

    container->setEditable (editable);
    //
    //Insert into the containers map
    //
    //The containers map associates actions with their GanttBarContainers
    //As we do planning, not scheduling, an Action can appear many times 
    //in the plan. Hence we need to map each Action to a map of keys to
    //GanttBarContainers.
    //
    //The keys are read from the event->actionCopy field. This lets us
    //find the correct GanttBarContainer when we receive an action end
    //event.
    //
	
	
    //If we have no existing containers for the action then
    //create a new copy map and add the container as the first entry.
    containers [event->actionCopy] = container;
}

void GanttChart::visitEndAction (BrazilEvent* event, DOMAction*) {
	
    //Get right container using actionCopy Key for this event.
    GanttBarContainer* container = containers [event->actionCopy];
	
    //Position the end of the action.
    container->setEndAction (event);
	
    //Add to ganttBarArea after the x position h as been set.
    //Adding after the call to positionActionStart will ensure it
    //is added to the correct vertical position.
    ganttBarArea->addGanttBarContainer (*container);
	
    //Add to gantt name area.
    ganttNameArea->addGanttBarContainer (*container);
	
    container->setVisible (true);
}

void GanttChart::visitStartOutcome (BrazilEvent* event, DOMOutcome*) {
    //Find the container for the action
    GanttBarContainer* container =  containers  [event->actionCopy];
	
    //pass the start outcome event to the container.
    container->setStartOutcome(event);
}

void GanttChart::visitEndOutcome (BrazilEvent* event, DOMOutcome*) {
    //as above..
    cout<<"GanttChart::visitEndOutcome() actionCopy="<<event->actionCopy<<endl;
    GanttBarContainer* container = containers [event->actionCopy];

    container->setEndOutcome (event);
}

void GanttChart::visitEffect (BrazilEvent* event, DOMEffectSet* effect) {
	
    //This effect might not be associated with an action.
    if(!containers.count (event->actionCopy)) return;
	
    //Find the container
    GanttBarContainer* container = containers [event->actionCopy];
	
    //Find whether the effect is associated with an action or outcome
    if(((DOMAction*) container->getStartAction()->
	element)->getEffectSet () == effect) {
		
	container->setEndActionEffect (event);
    }
		
    else if(container->getStartOutcome() != NULL
	    && ((DOMOutcome*)container->getStartOutcome ()->element)
	    ->getEffects().front() == effect) {
	container->setEndOutcomeEffect (event);
    }
		
    //To-Do: More when we have instant effects.
}

/**
 * Some or all of the elements within container have changed position and
 * any event in the state that is queud at or after invalidTime must be
 * removed. The resulting state has its effects replayed and is passed to
 * the planner as an initial state. The planner is then requested to create
 * a new medianPlan, which will be drawn by handleNewMedianState.
 */
void GanttChart::positionChanged (GanttBarContainer& changedContainer, int leftInvalidTime, BrazilEvent* trigger)
{
    cout << "** Position Changed." << endl;
    cout << "Left Invalid Time: " << leftInvalidTime << endl;

    BrazilStateUpdater* newState = new BrazilStateUpdater(this->domain,  domain->getProperties()->getPropertyInt("planner max makespan"));
		
    //Populate with events from current DRAWN state.
    BrazilState* currentState = drawnStates->getStates().back();
	
    cout<<"Current state before copy:" << *currentState<<endl;

    moveEvents(currentState->getPast(), newState, leftInvalidTime);
    moveEvents(currentState->getFuture(), newState, leftInvalidTime);
	
    cout << "position changed, init state: " << *newState << endl;
    cout<<"Trigger event:"<<*trigger<<endl;

    // Reinsert a copy of the trigger event if it's a start action
    if (trigger->type == BrazilEvent::START_ACTION) newState->queueEvent(new BrazilEvent(*trigger));
	
    // Replay the newly queued effects.
    newState->replayEffects(leftInvalidTime);

    cout << "after replay effects: " << *newState << endl;

    // This doesn't block
    emit requestSimulationFrom(newState);
    
    // Can't delete this until requestSimulationFrom has used it.
    //delete newState;

}


/**
 * When user modifies state a newstate is constructed by copying over all the events
 * queued up to the invalid time. Events are copied into the future queue
 * So that BrazilStateUpdater::replayEffects() can work. There are some tricky bits to get
 * when events are moved forward in time (have to remove their end action events).
 * @param src the current state
 * @param dest the new state
 * @param invalidTime the time up to which we copy events over.
 * @param futureStarts those actions that were moved forward to a point after invalidTime
 * @author daa
 */
void GanttChart::moveEvents(const BrazilState::Events& src, 
			    BrazilState* newState, 
			    time_t invalidTime)
{
    
    cout << "move events, invalid time: " << invalidTime << endl;
    
    //Copy the events from past and future, creating new events and queueing them
    //into new state.
    for (BrazilState::EventsCIt eventIterator = src.begin (); 
	 eventIterator != src.end(); 
	 eventIterator++) {
	
	BrazilEvent* event=eventIterator->second;

	// Here I do not copy if the event is queued AT OR after invalid time, 
	// or if the event was ignored due to failed second check of preconds.
	if (event->queuedAt >= invalidTime || event->ignored) { 
			
	    cout << "!!! Event queued at or after invalid time: " << *event << endl;
	    continue;
	}
	else {
	    cout << "Event queued  before invalid time: " << *event << endl;
	}


	BrazilEvent* copiedEvent = new BrazilEvent ( *(eventIterator->second) );
	newState->queueEvent (copiedEvent);

    }
}



void GanttChart::handleNewMedianState (BrazilState* state) {
    BrazilState* currentDrawnState = NULL;
    if(drawnStates->getStates().size ()) 
	currentDrawnState = drawnStates->getStates ().back();
	
    //Also only draw if it the new state is going to produce a different gantt chart
    //to the last one drawn: !isVisuallyEquivalent ()
    if(!currentDrawnState || !isVisuallyEquivalent (currentDrawnState, state)) {
	if(!isDrawingState) {
	    drawnStates->addState(state);
	    //If we get another state whilst we are drawing one
	    //then we want to finish the state we're currently
	    //on.
	    isDrawingState = true;
	    cout << "Handle New Median State: " << *state << endl;
	    buildGanttChart (drawnStates->getStates ().back());
	    isDrawingState = false;
	}
    }
}

/**
 * Deletes the gantt bar containers and creates a new represention from
 * state. This method relies on drawFromState
 */
void GanttChart::buildGanttChart (BrazilState* state) {
	
    /*
      Although this appears to be horribly inefficient - just deleting
      the whole gantt chart and redrawing it from scratch, it actually
      works fine. 
	 
      If we start experiencing performance problems [with larger gantt
      charts] then this would the place to start refactoring.
	 
    */
	
    // [daa] Swapped order of deletes here.
    containers.clear ();
	
    chlayout->removeWidget (ganttNameArea);
    chlayout->removeWidget (ganttBarArea);

    delete ganttNameArea;
    delete ganttBarArea;
	
	
    ganttNameArea = new GanttNameArea (childContainer);
    chlayout->addWidget (ganttNameArea);
	
    ganttBarArea = new GanttBarArea (childContainer);
    chlayout->addWidget (ganttBarArea);
	
    ganttNameArea->setVisible (false);
    ganttBarArea->setVisible (false);
	
    //This method call does the guts of the drawing - 
    //it goes through all the events in the state and
    //using the BrazilEventVisitor implementation - it
    //draws the gantt chart from each event.
    drawFromState (state, 0);
	
    //Tidy up.
    //Find gantt bars that have  not been ended.
    map<int, GanttBarContainer*>::iterator it;
    for(it = containers.begin (); it != containers.end(); it++) {
		
	if(it->second->getEndAction() == NULL) {

	    //No end action event, so create one.
	    BrazilEvent* endActionEvent = new BrazilEvent;
	    endActionEvent->type = BrazilEvent::END_ACTION;
	    endActionEvent->actionCopy = it->second->getStartAction ()->actionCopy;
	    endActionEvent->element = it->second->getStartAction()->element;
	    endActionEvent->queuedAt = it->second->getStartAction()->queuedAt;
	    endActionEvent->timeToOccur = 100000000;	//fixed.
			
	    it->second->setEndAction (endActionEvent);
	}
	else if (it->second->getStartOutcome () && 
		 it->second->getEndOutcome () == NULL) {
			
		    
	    //No end outcome event, so create one.
	    BrazilEvent* endOutcomeEvent = new BrazilEvent;
	    endOutcomeEvent->type = BrazilEvent::END_OUTCOME;
	    endOutcomeEvent->actionCopy = it->second->getStartOutcome ()->actionCopy;
	    endOutcomeEvent->element = it->second->getStartOutcome()->element;
	    endOutcomeEvent->queuedAt = it->second->getStartOutcome()->queuedAt;
	    endOutcomeEvent->timeToOccur = 100000000;	//fixed.
		    
	    it->second->setEndOutcome (endOutcomeEvent);
	}
    }
	
    ganttNameArea->setVisible (true);
    ganttBarArea->setVisible (true);
	
}

void GanttChart::setEditable (bool editable) {
    this->editable = editable;
	
    //Mark each container as editable.
    map<int, GanttBarContainer*>::iterator it;
    for( it = containers.begin (); it != containers.end(); it++) {
	it->second->setEditable (editable);
    }
}

