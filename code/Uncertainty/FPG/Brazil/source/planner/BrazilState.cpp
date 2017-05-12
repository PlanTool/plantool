/*
 * $Id: BrazilState.cpp 280 2007-06-22 04:50:13Z daa $ 
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

#include"BrazilSetup.h"

DBG_MODULE(DBG_STATE);

#include"DOMPredicate.h"
#include"DOMFunction.h"
#include"DOMProblem.h"
#include"BrazilEvent.h"
#include"BrazilState.h"
#include"PlanningException.h"

using namespace std;



/**
 * Default constructor for initial state. Assumes no events on queue.
 */
BrazilState::BrazilState (DOMProblem* problem) {
    this->startTime = 0;
    this->time = 0;
    this->predicates = problem->initiallyValidPredicates();
    this->resources = problem->initialFunctionValues();
    success = false;
    validState = true;
    totalActions = 0;
}


BrazilState::~BrazilState() {

    freeQueues();

}


/**
 * Get the current value of the clock. Using seconds from start.
 */
time_t BrazilState::getTime() const {
    return time;
}


void BrazilState::setTime(time_t newTime) {
    assert(newTime >= time);
    time = newTime;
}


/**
 * Determine if predicate is currently true.
 * @param predicate predicate to test
 */
bool BrazilState::getPredicateValue(DOMPredicate* predicate) {
    return (predicates.end() != predicates.find(predicate));
}


/**
 * Change a predicate value. Setting to true means inserting. Because
 * we use a normal set, this means that if the predicate is
 * already true (present) then there will be no change.
 * If the predicate is false (absent) it will be inserted.
 * Vice versa for negating predicates.
 * @param predicate the predicate to change
 * @param val new value.
 */
void BrazilState::setPredicateValue(DOMPredicate* predicate, bool val) {

    if (DBG(3)) BOUT<<" predicate "<<predicate->getName()<<"="<<val<<endl;

    if (val) predicates.insert(predicate);
    else predicates.erase(predicate); 

}


/**
 * Determine a resource level. System must guarantee all resources are 
 * in map, even if resource level is 0.
 * @param resource Pointer into XML tree for the resource definition
 */
double BrazilState::getResourceLevel(DOMFunction* resource) {
   ResourceList::iterator i;
   i = resources.find(resource);
   assert(i != resources.end());
   return i->second;
}


/**
 * Change a resource level
 * @param resource the resource to change
 * @param the new value
 */
void BrazilState::setResourceLevel(DOMFunction* resource, double val) {
    resources[resource] = val;
}

/**
 * We've reached the end of events (for the moment). In 
 * in addition if there's no eligible tasks, we're in 
 * in a failure state.
 */
bool BrazilState::endOfEvents() {

    return (future.begin() == future.end());

}


/**
 * peek at the time of the next event. Useful for operations that
 * process all events at the same clock tick.
 */
time_t BrazilState::nextEventTime() {  

    return future.begin()->first->timeToOccur;

}


/**
 * Get the current event on the queue and increment the 
 * iterator to the next event (which might be events.end()) queue.
 *  Does not remove it.  I think this
 * is safe because the STL map containers have the property that
 * inserting/deleting events does not invalidate current iterators.
 * Make sure you always call isLastEvent() first, otherwise you'll end
 * up with an invalid iterator.
 */
BrazilEvent* BrazilState::nextEvent() {  

    // Get next event
    BrazilEvent* e = future.begin()->second;

    // Store on "past" event queue
    past.insert(past.end(), pair<BrazilEvent*, BrazilEvent*>(e, e));

    // Delete from future queue. Safe because we don't lose the pointer
    future.erase(future.begin());
    
    if (DBG(3)) BOUT<<" nextEvent()="<<*e<<endl;
    return e;

}


/**
 * Add an event to the queue.The current time will be added to the event
 * as the queuedAt time. The key will be extracted from the events timeToOccur
 * @param event pointer with all elements except queuedAt.
 */
void BrazilState::queueEvent(BrazilEvent* e) {
    future.insert(pair<BrazilEvent*, BrazilEvent*>(e, e));
    if (DBGM(3, DBG_EVENTS|DBG_STATE)) BOUT<<"Added event:"<<*e<<endl;
    
}



/**
 * More convenient version of queueEvent. Automatically populates
 * queuedAt field with current state time 
 * @see #queueEvent(BrazilEvent)
 * @param type type of the event
 * @param element a pointer into the Domain that tells us where to
 * look for how to execute this event
 * @param time the event should occur
 * @param terminalType Used only for TERMINAL type events. Defaults to Goal. Has no impact
 * on planner but could be used in GUI to note
 * oddities such a task failing to start due to a conflict, or why an
 * execution failed
 */
void BrazilState::queueEvent(BrazilEvent::EventType type, DOMWrapper* element, time_t timeToOccur, int actionCopy, BrazilEvent::TermType terminalType) {


    BrazilEvent* e = new BrazilEvent();
    e->type = type;
    e->element = element;
    e->queuedAt = getTime();
    e->timeToOccur = timeToOccur;
    e->terminalType = terminalType;

    if (type == BrazilEvent::START_ACTION) {
	totalActions++;
	e->actionCopy = totalActions;
    } 
    else e->actionCopy = actionCopy;
    

    queueEvent(e);

}


/**
 * Not the normal C++ copy.
 * Before performing the copy all events in the current queues
 * will be freed. This is because they are dynamically 
 * allocated within the BrazilState, but the pointers will
 * be lost when the multimaps are overriden in the shallow copy.
 * Over time this would result in a bad memory leak that
 * builds up over planning.
 * 
 * Also, a deep copy of the source event queues is performed for
 * the same reason.
 */
void BrazilState::copy(const BrazilState& srcState) {

    // Delete both queues and free memory associated with their
    // events.  Better make sure no one else has refs to these
    // pointers!
    freeQueues();

    // Perform deep copy of srcState queue
    copyQueue(srcState.getPast(), past);
    copyQueue(srcState.getFuture(), future);

    // Copy other important stuff
    time = srcState.getTime();
    startTime = srcState.startTime;
    resources = srcState.resources;
    predicates = srcState.predicates;
    success = srcState.success;
    validState = srcState.validState;
    totalActions = srcState.totalActions;
    queuedToStart = srcState.queuedToStart;

}


BrazilState& BrazilState::operator=(const BrazilState& srcState) { 

    if (&srcState != this) copy(srcState);
    else throw PlanningException("trying to copy self in BrazilState\n");
    return *this;
}


/** 
 * Do a deep copy of one queue to another.
 * Starts from beginning of src and end of dst so you can do 
 * a concatenate if necessary.
 */
void BrazilState::copyQueue(const Events& src, Events& dst) {

    for (Events::const_iterator it=src.begin(); it != src.end(); it++) {
	// Create new copy of the event and insert that in destination
	BrazilEvent* eventCopy = new  BrazilEvent(*(it->second));
	dst.insert(pair<BrazilEvent*, BrazilEvent*>(eventCopy, eventCopy));
    }
}


/**
 * Because the queues just hold pointers to events, we need
 * to ensure the allocated events all get propely 
 * freed at the end of each simulated execution (after
 * passing this info to PlanningListeners that make their
 * own copies of events.
 */
void BrazilState::freeQueues() {

    freeQueue(past);
    freeQueue(future);

}


/**
 * Free all the events in a particular queue, then clear the queue since
 * none of the events it points to are valid.
 */
void BrazilState::freeQueue(Events& q) {
    for (Events::iterator e=q.begin(); e != q.end(); e++) {
	delete e->second;
	if (DBG(4)) BOUT<<"Deleted event\n";
    }
    q.clear();
}


/**
 * Print the current state. 
 */
ostream& BrazilState::print(ostream& o) {

    o<<"State @"
     <<time<<endl;
    if (success) o<<"\t* Goal reached *"<<endl;
    if (!validState) o<<"\t- Invalid state -"<<endl;

    o<<"\tTrue predicates:"<<endl;
    for (DOMPredicate::PredicateCIt it = predicates.begin(); it != predicates.end(); it++) {
	o<<"\t\t"<<(*it)->getName()<<endl;
    }
    
    o<<"\tResource levels:"<<endl;
    for (ResourceList::const_iterator it = resources.begin(); it != resources.end(); it++) {
	o<<"\t\t"<<it->first->getName()<<"="<<it->second<<endl;
    }

    o<<"\tPast event queue:"<<endl;
    for (Events::iterator e=past.begin(); e != past.end(); e++) {
	o<<"\t\t"<<*(e->second);
    }

    o<<"\tFuture event queue:"<<endl;
    for (Events::iterator e=future.begin(); e != future.end(); e++) {
	o<<"\t\t"<<*(e->second);
    }

    return o;
}
	


/**
 * Operator version of BrazilState::print()
 */
ostream& operator<<(ostream& o, BrazilState& s) {
    
    return s.print(o);
    
}



