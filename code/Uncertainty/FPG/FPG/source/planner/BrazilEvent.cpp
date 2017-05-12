/*
 * $Id: BrazilEvent.cpp 281 2007-06-29 07:02:24Z daa $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code.
 */

#include"DOMWrapper.h"
#include"DOMAction.h"
#include"DOMOutcome.h"
#include"DOMEffectSet.h"
#include"BrazilSetup.h"

//DBG_MODULE(DBG_EVENTS);

#include"BrazilEvent.h"


using namespace std;

string BrazilEvent::typeNames[] = {"start action", "end action", "start outcome", "end outcome", "effect", "terminal state"};


/**
 * Doesn't do anything except set the processed flag to false
 */
BrazilEvent::BrazilEvent() {
    actionCopy = 0;
    ignored = false;
}

/**
 * Print information about the event.
 */
ostream& BrazilEvent::print(ostream& o) {
    
    string name;

    switch(type) {
        case START_ACTION:
        case END_ACTION:
	    name = ((DOMAction*)element)->getName();
	    break;
        case START_OUTCOME:
        case END_OUTCOME:
	    name = ((DOMOutcome*)element)->getLabel();
	    break;
        case EFFECT:
	    name = ((DOMEffectSet*)element)->getTypeName();
	    break;
        case TERMINAL:
	    name = "Plan ends";
	    break;
        default:
	    name = "unkown type";
    }

    o<<"Event "
     <<typeNames[type];
    o<<" ("
     <<actionCopy
     <<")";
    o<<": '"
     <<name;
    o<<"' @ "
     <<timeToOccur
     <<" queued at "
     <<queuedAt
     <<" pointing to ";
    o<< element;
	
	o<< " (" << actionCopy << ")"
     << endl;
    
    if (type==TERMINAL) {
	o<<"\t\t\tTerminal state type  = '";
	switch (terminalType) {
	case GOAL: 
	    o<<"Goal reached'\n";
	    break;
	case DEAD_END:
	    o<<"Dead end'\n";
	    break;
	case OUT_OF_TIME:
	    o<<"Out of time'";
	    break;
	case PREDICTED_DEAD_END:
	    o<<"Predicted dead end'";
	    break;
	default:
	    o<<"UNKNOWN'";
	}	
    }

    return o;
}


/**
 * Wrapper around BrazilEvent::print()
 */
ostream& operator<<(ostream& o, BrazilEvent& e) {

    return e.print(o);
}


/** 
 * Test for equality of two events. Events are considered equal if 
 * The of the same type, at the same time, and operating on the
 * same element (action/outcome/effect)
 */
bool operator==(BrazilEvent& lhs, BrazilEvent& rhs) {

    if (lhs.type != rhs.type) return false;
    if (lhs.timeToOccur != rhs.timeToOccur) return false;
    if (lhs.element != rhs.element) return false;
    return true;
}
