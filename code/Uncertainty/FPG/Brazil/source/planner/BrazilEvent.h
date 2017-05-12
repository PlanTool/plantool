/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * $Id: BrazilEvent.h 270 2007-01-25 09:50:38Z daa $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code.
 */

#ifndef _BrazilEvent__h
#define _BrazilEvent__h

#include"DOMWrapper.h"

/**
 * The eventqueue for simulating a plan execution is made up of a list
 * of these events. This is just a data structure (except for printing
 * methods) so everything is public.
 */
class BrazilEvent {

public:

    /**
     * The fundamental event types. An action will typically consist
     * of several events including a start and end task, start and end
     * outcome, plus a series of effects.
     */
    enum EventType { START_ACTION, END_ACTION, START_OUTCOME, END_OUTCOME, EFFECT,  TERMINAL };


    /**
     * The different classes of termination.
     */
    enum TermType { GOAL, DEAD_END, OUT_OF_TIME, PREDICTED_DEAD_END };

    static std::string typeNames[];

    /**
     * Does main ordering on time, and subordering on whether it's an event.
     * This ensures that effects are always processed before start
     * actions/end actions.
     * Implement the comparison operator for ordering elements in the
     * event queues.
     */
    struct BrazilEventOrder {
	bool operator()(const BrazilEvent* lhs, const BrazilEvent* rhs) const {
	    if (lhs->timeToOccur < rhs->timeToOccur) return true;
	    if (lhs->timeToOccur > rhs->timeToOccur) return false;
	    // Times are the same, make effects earlier.
	    if (lhs->type == EFFECT) return true;
	    return false;

	}
    };

    BrazilEvent();
 
    /**
     * The class of event. In a sense this is redundant because the
     * DOM element should always be able to tell us what the event type
     * is, but there may be times when we need to cast the element 
     * below to something easier to work with.
     */
    EventType type;
    
    
    /**
     * For a TERMINAL event, it means planning has ended. This
     * variable encodes the reason. Mostly so the GUI can render the
     * reason for ending and figure out what time the plan really ends
     */
    TermType terminalType;

    /**
     * If the event concerns an action, which copy of the action does
     * it refer to.  The number is unique for every action started.
     */
     int actionCopy;
    
  
    /**
     * A pointer to whatever part of the XML specification 
     * tells us how to implement an event.
     */
    DOMWrapper* element;
    
    /*
     * When was this event queued. Useful if we want to "unwind" the
     * plan execution to an old decision point.
     */
    time_t queuedAt;

    /**
     * When will the event happen. This is used as the key for 
     * the Brazil
     */
    time_t timeToOccur; 


    /**
     * Particularly with start action events the planner may choose to
     * ignore the request if the actions pre conditions are no longer met
     * This flag indicates that that happened. The GUI should ignore
     * this event. It's left on the queue for debugging/stats purposes.
     */
    bool ignored;

    /**
     * Print the current event.
     */
    ostream& print(ostream& o);
    friend ostream& operator<<(ostream& output, BrazilEvent& s);
    friend bool operator==(BrazilEvent& lhs, BrazilEvent& rhs);

};

#endif
