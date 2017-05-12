/*
 * $Id: BrazilState.cpp 107 2006-07-27 03:31:12Z owen $ 
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

DBG_MODULE(DBG_UPDATERS);

#include"DOMWrapper.h"
#include"DOMPredicate.h"
#include"DOMFunction.h"
#include"DOMProblem.h"
#include"DOMAction.h"
#include"DOMOutcome.h"
#include"DOMEffectSet.h"
#include"DOMAtomicEffect.h"
#include"DOMPredicateEffect.h"
#include"DOMPredicate.h"
#include"DOMFunctionEffect.h"
#include"DOMFunction.h"

#include"BrazilState.h"
#include"BrazilStateHelpers.h"
#include"BrazilStateUpdater.h"

#include"PlanningException.h"

using namespace std;

/**
 * Create new state based on domain problem spec. 
 * Immediately figure out what actions are eligible
 * @param md Maximum permitted duration of the plan
 */
BrazilStateUpdater::BrazilStateUpdater(DOMDomain* d, time_t md) : 
    BrazilStateHelpers(d->getProblem()), 
    domain(d), 
    maxDuration(md), 
    effectVisitor(this) {
    updateEligible();
}


/**
 * Put a start action event on the queue.  No validity checks done
 * here. That will happen when the start action events are processed.
 * @param a action to start
 */
void BrazilStateUpdater::startAction(DOMAction* a) {

    // Mark that this action is scheduled to start
    queuedToStart.insert(a);

    // Event should happen in this clock tick
    queueEvent(BrazilEvent::START_ACTION, a, getTime(), 0);
    
}


/**
 * Scan through all actions and determine if they are eligible
 * @return true if any eligibile task was found, false otherwise
 */
bool BrazilStateUpdater::updateEligible() {

    eligible.clear();
    int a=0;
    for (DOMAction::ActionCIt i = domain->getActions().begin(); i != domain->getActions().end(); i++, a++) {
	// Is this state variable/predicate satisfied
	if (isActionEligible(*i)) eligible[a] = *i;
    }

    // Nothing eligible!
    if (eligible.size() == 0) return false;
    return true;
}



/**
 * A real workhorse of the system. Will process events until a
 * decision point is found, a successful state is found or we run out
 * of events and there's no eligible tasks.Also enforces the behaviour that
 * a decision point cannot occur at current state time.
 * This is really ugly. Need to try again when I'm more with it.
 * @param median Should we turn off stochasticty and always 
 *               sample most likely outcome/duration?
 * @return true if a decision point has been reached, false otherwise. 
 * In the false case, goal can be distinguished
 * from failure by checking the value of isSuccessful()
 */
bool BrazilStateUpdater::processEvents(bool median) {

    if (DBG(3))	cout<<"Process Events." << endl;
	
    // Always process all events at current time first
    if (processBatchOfEvents(median)) return false;  // return false if goal found

    // We promise to make time move forward to avoid infinite loops, 
    // even if there were some events processed at the current time.
    if (endOfEvents()) setTime(getTime() + 1);
    
	if (time > maxDuration) {
		queueEvent(BrazilEvent::TERMINAL, NULL, time, 0, BrazilEvent::OUT_OF_TIME);
		return false;
    }
    
    // Now we've handled special case of events at current clock tick,
    // go into general case of moving forward in time
    do {
		
		// Failure case: No more events, and nothing eligible Slightly
		// inneficient to potentially call updateEligible twice in a
		// row like this. Should be rare though.
		if (endOfEvents() && !updateEligible()) {
			queueEvent(BrazilEvent::TERMINAL, NULL, time, 0,  BrazilEvent::DEAD_END);
			return false;
		}

		if (!endOfEvents()) setTime(nextEventTime()); // Increment time
		if (time > maxDuration) {
			queueEvent(BrazilEvent::TERMINAL, NULL, time, 0, BrazilEvent::OUT_OF_TIME);
			return false;
		}

		if (processBatchOfEvents(median)) return false;  // return false if goal found

    } while(!updateEligible());

    return true;  // we've found a new decision point
    
}


/** 
 * Process the next batch of events. Processing will
 * consume all events at the current time point only.
 * On return, the next event will be the first
 * event in the future. Will evaluate goal conditions
 * after processing a batch.
 * @return true if a goal was reached
 */
bool BrazilStateUpdater::processBatchOfEvents(bool median) {

    time_t currTime = getTime();

    if (DBG(5)) BOUT<<"Process Batch of Events."<<endl;

    // Process  all events in next time batch
    while(!endOfEvents() && nextEventTime()==currTime) {
		processEvent(nextEvent(), median);
    } 

    return isGoal();

}


/**
 * Generic event processor. Just a giant switch to other handlers
 * @param e event to process
 * @param median are we ignoring probabilities?
 */
void BrazilStateUpdater::processEvent(BrazilEvent* e, bool median) {

    if (DBG(4)) BOUT<<"\t Process Event: "<<*e<<endl;

    switch(e->type) {
        case BrazilEvent::START_ACTION:
	    if (!processStartAction((DOMAction*)(e->element), e->actionCopy, median)) {
		// The action could not be started because a condition has been violated since
		// the action was  first queued. To keep the history clean, we remove
		// the event from the past queue. (It should always be the tail)
		// This is important otherwise it 

		e->ignored = true;
	    }
	    break;
        case BrazilEvent::END_ACTION:
	    processEndAction((DOMAction*)(e->element), e->actionCopy, median);
	    break;
        case BrazilEvent::START_OUTCOME:
	    processStartOutcome((DOMOutcome*)(e->element), e->actionCopy, median);
	    break;
        case BrazilEvent::END_OUTCOME:
	    processEndOutcome((DOMOutcome*)(e->element));
	    break;
        case BrazilEvent::EFFECT:
	    processEffect((DOMEffectSet*)(e->element));
	    break;
        case BrazilEvent::TERMINAL:
	    // Do nothing
	    break;
        default:
	    throw PlanningException("Unknown event type in BrazilStateUpdater::processEvent()");
    }

}


/**
 * Sample the duration of this action and schedule it's end for that time. 
 * Double check the action is eligible, and if it's not, put a NOTE in the event queue
 * @param a the action to start
 * @param median true if we're not sampling
 * @param actionCopy which action are we running 
 * @return false if the action could not be started 
 */
bool BrazilStateUpdater::processStartAction(DOMAction* a, int actionCopy, bool median) {
 
    if (DBG(3)) BOUT<<"processing action="<<a->getName()<<endl;

    // De-mark action as queued but not started (or eligibility test will fail)
    queuedToStart.erase(a);

    // Do a double check because policy gradient approach does not guarantee
    // no conflicting actions will be chosen concurrently
    if (!isActionEligible(a)) return false;

	
    // Queue instant effects. Due to ordering on event queue, these
    // events should happen *before* other start events already
    // queued.
    DOMEffectSet* atStartEffects = a->getAtStartEffectSet();
    if (atStartEffects->getNumAtomicEffects() > 0) {
        time_t effectTime =  sampleTime(atStartEffects->getDelay(), median);
	if (DBG(4)) BOUT<<"Queueing start effect for "<<effectTime<<endl;
        queueEvent(BrazilEvent::EFFECT, atStartEffects, effectTime, actionCopy);
    }

    // Now the fun part. Figure out when to schedule end of event.
    queueEvent(BrazilEvent::END_ACTION, a, sampleTime(a->getEffectSet()->getDelay(), median), actionCopy);

    return true;

}


/**
 * Queue deterministic end effects
 * Toss die for outcomes
 * Queue outcome start
 * @param a the action to start
 * @param median true if we're not sampling
 * @param which action are we running
 */
void BrazilStateUpdater::processEndAction(DOMAction* a, int actionCopy, bool median) {

    DOMEffectSet* effects = a->getEffectSet();
    // Doco says effects set never Null
    assert(effects != NULL);
    // Queue deterministic end effects
    if (effects->getNumAtomicEffects() > 0) {
	queueEvent(BrazilEvent::EFFECT, effects, getTime(), actionCopy);
    }

    // Does this action have any outcomes?
    if (a->getProbabilisticChildren().front()->getNumberOfOutcomes() > 0) {

	// Compute an outcome and schedule it's start.
	DOMOutcome* o;
	if (median) o = a->getProbabilisticChildren().front()->getMostLikelyOutcome();
	else o = a->getProbabilisticChildren().front()->sampleOutcome();
	
	assert(o != NULL);

	// The start of an outcome is always in the next step
	queueEvent(BrazilEvent::START_OUTCOME, o, getTime()+1, actionCopy); 
    }
}


/**
 * Queue outcome end effects.
 * Seems to be possible to have multiple sets of outcome effects.
 * As normal, this has weirdness when it comes to defining the end of 
 * an action. Fortunately, there's an indirect path from outcome back to parent
 * action.
 * Toss die for outcomes
 * Queue outcome start
 * @param a the action to start
 * @param actionCopy which action gneerated this outcome
 * @param median true if we're not sampling
 */
void BrazilStateUpdater::processStartOutcome(DOMOutcome* o, int actionCopy, bool median) {

    time_t maxDelay = getTime();  // Init for empty outcome case
     
    // Queue start effects (which might actually have a delay anyway)
    DOMEffectSet* atStartEffects = o->getAtStartEffects();

    // The effect set might be empty
    if (atStartEffects->getNumAtomicEffects() > 0) {
        time_t delay =  sampleTime(atStartEffects->getDelay(), median);
	maxDelay = max(maxDelay, delay);
	if (DBG(4)) BOUT<<"Queueing outcome start effect for "<<delay<<endl;
	// Due to ordering on queue, any effects scheduled for this time will
	// be implemented before any other kind of event.
	else queueEvent(BrazilEvent::EFFECT, atStartEffects, delay, actionCopy);

    }


    // Queue different end effects
    DOMOutcome::Effects effects = o->getEffects();

    for (DOMOutcome::EffectsCIt it = effects.begin(); it != effects.end(); it++) {
	time_t delay =  sampleTime((*it)->getDelay(), median);
        maxDelay = max(maxDelay, delay);
	queueEvent(BrazilEvent::EFFECT, *it, delay, actionCopy);
    }

    queueEvent(BrazilEvent::END_OUTCOME, o, maxDelay, actionCopy);
    
}



/**
 * Nothing to do. Its just useful for the GUI to see it in the event
 * queue.
 */
void BrazilStateUpdater::processEndOutcome(DOMOutcome*) {}



/**
 * Process effects, which can be effects on functions or predicates. 
 */
void BrazilStateUpdater::processEffect(DOMEffectSet* e) {

    DOMEffectSet::AtomicEffects ae = e->getAtomicEffects();

    for (DOMEffectSet::AtomicEffectsCIt it=ae.begin(); it != ae.end(); it++) {
	(*it)->visit(&effectVisitor);
    }

}


/**
 * Finally, a function that actually updates the state variables!
 */
bool BrazilStateUpdater::EffectVisitor::visitPredicateEffect(DOMPredicateEffect* pe) {
    state->setPredicateValue(pe->getPredicate(), !pe->isNegated());    
    return true;
}


/**
 * Finally, a function that actually updates the resources!
 */
bool BrazilStateUpdater::EffectVisitor::visitFunctionEffect(DOMFunctionEffect* fe) {
    
    double newLevel = state->getResourceLevel(fe->getFunction());
    double expressionResult = fe->getExpression()->visit(state->expressionEvaluator);

    switch(fe->getOperator()) {
	case DOMFunctionEffect::scaleUp:
	    newLevel *= expressionResult;
	    break;
	case DOMFunctionEffect::scaleDown:
	    newLevel /= expressionResult;
	    break;
	case DOMFunctionEffect::increase:
	    newLevel += expressionResult;
	    break;
	case DOMFunctionEffect::decrease:
	    newLevel -= expressionResult;
	    break;
	case DOMFunctionEffect::assign:
	    newLevel = expressionResult;
	    break;
        default:
	    throw PlanningException("Unknown operator in BrazilStateUpdater::visitFunctionEffect()");
    }

    state->setResourceLevel(fe->getFunction(), newLevel);
    
    return true;
}


/** 
 * Due to sligly odd teired arrangement of BrazilState and its
 * subclasses we need a way to copy a base level state object
 * (perhaps passed in by GUI), to high level BrazilStateUpdater.
 * This has to update the set of eligible tasks too, since
 * state just changed.
 */
BrazilStateUpdater& BrazilStateUpdater::operator=(const BrazilState& srcState) { 
    copy(srcState); 
    updateEligible(); 
    return *this; 
}




/**
 * When we want to recreate an intermediate state up to a particular
 * time point you create a fresh BrazilStateUpdater, copy the events
 * into the future queue up to the time you want to recreate, then you
 * call this method. It will replay all the effects and time
 * increments up to the last event in the future queue or the
 * specified time. It will also leave the eligible list up to date. It
 * guarantees NOT to add new events.
 * 
 * Addition: replayEffects() now re-checks preconditions for every task
 * start. This covers the possibility that a user has dragged a 
 * task start prior to some effect it depends on being created.
 * This is not considered a fatal error, but will result in
 * the state as being flagged "invalid". State will be returned
 * to valid the next time replayEffects is called on a valid sequence. 
 *
 * Addition 2: replayEffects() now guarantees a complete reconstruction of the 
 * state, not just effects. This really means the totalActions count and
 * the queudToStart fields are updated too.
 *
 * @see #isValid
 * @param upTo Time to stop processing events (not queueing time).
 * @return true if state valid
 */
bool BrazilStateUpdater::replayEffects(time_t upTo) {

    BrazilEvent* e = NULL;

    validState = true; // Assume valid

    // Process  all effects events in future queue
    while(!endOfEvents()) {

	// Order of next two important
	if (nextEventTime() != time) {
	    // Don't process at or past the upTo time point to avoid 
	    // processing stuff that was queued prior to
	    // change time point, but happens after change time point.
	    if (nextEventTime() >= upTo) break; 
	    setTime(nextEventTime());
	}
	   
	e = nextEvent(); // This moves event to past queue
	
	// Check conditions still hold
	if (e->type == BrazilEvent::START_ACTION) {

	    // Actions must still be eligble after user moves things around
	    if (!isActionEligible((DOMAction*)e->element)) {
		// This can be erroneously triggered if an action's start effect invalidates
		// one of its own preconditions, which is commonly used to stop multiple
		// instance of an action co-occuring.
		validState=false;
		BOUT<<"Action preconds not met for "<<((DOMAction*)e->element)->getName()<<endl;
	    }
	    
	    // Just track the number of actions started so far
	    if (e->actionCopy > totalActions) totalActions = e->actionCopy;
	    
	}
	
	// Only process effects
	if (e->type == BrazilEvent::EFFECT) processEvent(e, true); // true means median
    } 

    
    // We also need to do a quick pass through the future events to
    // make sure that totalActions and the queuedToStart structures
    // are correct
    for (EventsCIt i = future.begin(); i != future.end(); i++) {
	if (i->second->type == BrazilEvent::START_ACTION) {

	    // Okay, this this action is already queued in the future.
	    // Better let the state know that so that commencing
	    // simulation from this point in time doesn't
	    // immediately requeue this action.
	    queuedToStart.insert((DOMAction*)i->second->element);

	    // Just track the number of actions started so far
	    if (e->actionCopy > totalActions) totalActions = e->actionCopy;

	}
    }


    setTime(upTo);
    updateEligible();
    isGoal(); // User change might lead us to a goal state immediately.
    return validState;
}
