/*
 * $Id: BrazilState.h 273 2007-03-15 06:00:18Z owen $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should not be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 * 
 */

#ifndef _BrazilState__h
#define _BrazilState__h

#include"DOMProblem.h"
#include"BrazilEvent.h"


/**
 * The BrazilState captures all information about the current point in 
 * the simulated execution of a plan. It contains the event queue, which
 * also records past events to facilitate production of results.
 * It does NOT have routines that assist in progression of the 
 * the state except to add events to the queue, just representation.
 * 
 */
class BrazilState {

 public:

    /**
     * Event queue type
     */
    typedef multimap<BrazilEvent*, BrazilEvent*, BrazilEvent::BrazilEventOrder> Events;
    typedef Events::iterator EventsIt;
    typedef Events::const_iterator EventsCIt;

 protected:

    /**
     * Absolute time, in seconds although this is fairly arbitrary.
     */
    time_t time;

    /**
     * Time that the plan started.
     */
    time_t startTime; 

    /**
     * The map of resource values
     */    
    typedef map<DOMFunction*, double> ResourceList;
    ResourceList resources;
    
    /** 
     * The currently true predicates. Abscence means false.
     */
    DOMPredicate::PredicateSet predicates;


    /**
     * event queue proper, all future events in time ordered sequence
     */
    Events future;

    /**
     * Keep track of all that has passed
     */
    Events past;

  
    /**
     * Flag set by BrazilStateHelpers::isGoal() to indicate status of
     * last call to isGoal().
     */
    bool success;


    /** 
     * Flag set by BrazilStateUpdaters::replay() to indicate if this state
     * represents an impossible situation according to the model.
     * This can happen if the user overrides the state. It's up to 
     * the gui to decide if the user is allowed to create impossible states.
     * From an impossible state the planner will just try and plan anyway.
     */
    bool validState;


    /**
     * In some situations, namely when the user manaully schedules
     * an action to begin sometime in the future, we need to prevent
     * the planner from choosing that action, at least until
     * the scheduled action starts.
     * Pretty much just for the GANTT chart interaction at the moment.
     */
    set<DOMAction*> queuedToStart;

    /**
     * Count how many actions have been, or will be started. Just nice to know
     * and also used to give every action on the event queue a unique id.
     */
    int totalActions;



 public:
    
    /**
     * Default constructor initialises itself from the problem data
     */
    BrazilState(DOMProblem* problem); 

    /**
     * Desctructor will free BrazilEvents in queues
     */
    virtual ~BrazilState();

    /**
     * Copy and assignement stuff with deep copy of queues
     */
    BrazilState(const BrazilState& srcState) { copy(srcState); }
    virtual void copy(const BrazilState& srcState);
    BrazilState& operator=(const BrazilState& srcState);

	// see .cpp for doco
    void freeQueues();
    void freeQueue(Events& q);

    
    void setTime(time_t time);
    time_t getTime() const;    


    /**
     * Just a boolean flag to indicate if a sub-class of state
     * has decided that this state represents successful resolution
     * of the planning problem. Much faster than evaluating the
     * goal condition.
     */
    bool isSuccessful() { return success; }


    /** 
     * Does this state
     * represents an impossible situation according to the model.
     * This can happen if the user overrides the state. It's up to 
     * the gui to decide if the user is allowed to create impossible states.
     * From an impossible state the planner will just try and plan anyway.
     * Can only be set by BrazilStateUpdater#replay and constructor.
     */
    bool isValid() { return validState; }

    bool getPredicateValue(DOMPredicate* predicate);    
    void setPredicateValue(DOMPredicate* predicate, bool val);

    double getResourceLevel(DOMFunction* resource);
    void setResourceLevel(DOMFunction* predicate, double val);

    /**
     * Get the total number of actions that were queued to start.
     * Note that this does not mean they actually started.
     */
    int getActionsStarted() { return totalActions; }

    // Creation and processing of events
    bool endOfEvents();
    time_t nextEventTime();
    BrazilEvent* nextEvent();
    void queueEvent(BrazilEvent* e);
    void queueEvent(BrazilEvent::EventType type, DOMWrapper* element, time_t timeToOccur, int actionCopy, BrazilEvent::TermType terminalType = BrazilEvent::GOAL);

    // Events administration
    
    /**
     * Check out copyQueue or copy() if you want
     * your own deep copy of queue that you can
     * play with at leisure.
     * post const is a contract that getPast won't screw with anything
     * @see #copyQueue(Events&, Events&) 
     */
    const Events& getPast() const { return past; }
    

    /**
     * Direct access to future events 
     * Check out copyQueue or copy() if you want
     * your own deep copy of queue that you can
     * play with at leisure.
     * @see #copyQueue(Events&, Events&) 
    */
    const Events& getFuture() const { return future; }
    
    void copyQueue(const Events& src, Events& dst);

    ostream& print(ostream& o);
    friend ostream& operator<<(ostream&o, BrazilState& s);
    
};

#endif
