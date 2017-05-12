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

#include"BrazilEventVisitor.h"
#include"PlanningException.h"


/** 
 * Sort out what type of event this is and call appropriate handler
 * Not completely sure why this additional level of abstraction is useful in this case since it comes
 * down to a switch statement anyway.
 *
 * It would have been best to have this method inside the BrazilEvent itself, but 
 * I did not want to change that class when I wrote this some time ago.
 *
 * I think this is useful because if you have multiple classes wanting this functionality
 * then placing the switch in one location helps avoid redundant code.
 */
void BrazilEventVisitor::visit (BrazilEvent* event) { 
    
    switch (event->type) {
	
    case BrazilEvent::START_ACTION:			
	visitStartAction (event, (DOMAction*) event->element);
	break;
	
    case BrazilEvent::END_ACTION:
	visitEndAction (event, (DOMAction*) event->element);
	break;
	
    case BrazilEvent::START_OUTCOME:
	visitStartOutcome (event, (DOMOutcome*) event->element);
	break;
	
    case BrazilEvent::END_OUTCOME:
	visitEndOutcome (event, (DOMOutcome*) event->element);
	break;
	
    case BrazilEvent::EFFECT:
	visitEffect (event, (DOMEffectSet*) event->element);
	break;
	
    case BrazilEvent::TERMINAL:
	break;

    default:
	throw PlanningException("Unknown event type in brazilEventVisitor::visit()");
    }
}
