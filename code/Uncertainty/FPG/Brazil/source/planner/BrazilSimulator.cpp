/*
 * $Id: BrazilSimulator.cpp 280 2007-06-22 04:50:13Z daa $ 
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
#include"PGBasics.hh"

#include"DOMPredicate.h"
#include"DOMFunction.h"
#include"DOMAction.h"
#include"DOMDomain.h"

#include"BrazilSetup.h"

DBG_MODULE(DBG_SIMULATOR);

#include"BrazilEvent.h"
#include"BrazilState.h"
#include"BrazilStateHelpers.h"
#include"BrazilStateUpdater.h"
#include"PlanningListener.h"
#include"BrazilSimulator.h"


using namespace std;
using namespace libpg;


/**
 * Just remember the domain, and compute how many tasks and predicated there are
 * TODO: insert max duration from properties
 */
BrazilSimulator::BrazilSimulator(DOMDomain* domain) : 
    state(domain, domain->getProperties()->getPropertyInt("planner max makespan")), 
    initState(state), lastTerminalState(state) {

    props = domain->getProperties();
    this->domain = domain;
    numActions = domain->getActions().size();
    numPredicates = domain->getPredicates().size();
    planningListener = NULL;
    mode = SAMPLED_MODE;
    avgMS=0;

    episodes=0;

    if (DBG(1)) BOUT<<"Inital state: "<<initState<<endl;
}


void BrazilSimulator::setInitialState(BrazilState* newInitialState) {
    initState = *newInitialState;
}


BrazilState* BrazilSimulator::getCurrentState() {
    return &state;
}



/**
 * Returns the number of state variables in the system plus one for 
 * linear approixmator bias.
 */
int BrazilSimulator::getObsRows() { 
    return numPredicates + 1;
}


/**
 * All agents get same observation, so just one column
 */
int BrazilSimulator::getObsCols() {
    return 1;
}


/**
 * returns the number of tasks
 */
int BrazilSimulator::getAgents() {
    return numActions;
}


int BrazilSimulator::getActionDim() {
    return numActions;
}


int BrazilSimulator::getRewardDim() {
    return 1;
}


/**
 * Currently just returns negative of plan time
 * This should reinforce prob of success followed by duration
 */
void BrazilSimulator::getReward(Vector& rewards) {

    assert(rewards.size() == 1);
    rewards[0] = nextReward;
}


/**
 * in BrazilContext we just fill in the eligible vector with all the
 * previsously computed eligible tasks, then fill in the features
 * using the true predicates.  In the future this should be a much
 * richer function, building in any combination of features the user
 * selects.
 */
void BrazilSimulator::getObservation(Observation& obs) {

    assert(obs.getFeatures().size1() == (size_t)(numPredicates + 1));
    assert(obs.getFeatures().size2() == 1);

    // Fill in eligible actions
    Vector eligible = obs.getEligible();
    eligible.clear();
    for (BrazilStateUpdater::EligibleCIt i = state.getEligible().begin(); i != state.getEligible().end(); i++) {
	// Can this action run?
	eligible[i->first] = 1.0;
    }

    // Find out which state variables are true
    Matrix& features = obs.getFeatures();
    int f=0;
    for (DOMPredicate::PredicateIt it = domain->getPredicates().begin(); it != domain->getPredicates().end(); it++, f++) {
	// Is this state variable/predicate satisfied
	features(f,0) = (double)state.getPredicateValue(*it);
    }
    // Bias term
    features(f, 0) = 1.0;

}


/**
 * In the context of Brazil, doAction() launches actions selected by
 * the policy, then proceeds to process events until we a decision
 * point is reached.
 * @param actions Boost vector with 1 for actions to start, 0 otherwise
 */
int BrazilSimulator::doAction(Vector& actions) {

    if (props->findProperty("planner naive policy")) {
	// We want to follow a naive policy. Ignore the planner and set all eligible actions to run
	// Good test of mutex resolution :)
	for (BrazilStateUpdater::EligibleCIt i = state.getEligible().begin(); i != state.getEligible().end(); i++) {
	    actions[i->first] = 1.0;
	}
    }

    startActions(actions);
    return nextDecisionPoint();
}
 

/**
 * Process all events at current time, then incrementally process
 * events at future times until a terminal state is reached, or a
 * decision point is reached. 
 * @return 1 if terminal state, 0 otherwise
 */
int BrazilSimulator::nextDecisionPoint() {

    nextReward = 0;

    if (!state.processEvents(mode==MEDIAN_MODE?true:false)) {

	// Reached a terminal state, either with a success or failure
	if (state.isSuccessful()) nextReward = 1;
	// This is the case where a dead end was found. Penalise explicitly?
	else if (state.getTime() < props->getPropertyInt("planner max makespan")) nextReward = 0;

	if (planningListener != NULL) {
	    // Let the GUI know about the most recent completed execution.
	    if (mode==MEDIAN_MODE)  planningListener->medianState(&state);
	    else if (episodes%props->getPropertyInt("planner stochastic notify period")==0) {
		planningListener->stochasticState(&state);
	    }
	}

	if (DBG(2)) cout<<"Terminal state:"<<state<<endl;

	avgMS = (episodes*avgMS + state.getTime())/++episodes;
	lastTerminalState = state;
	reset(); // back to initial state
	return 1; // Goal

    }

    if (DBG(4)) BOUT<<" decision point found\n";

    return 0; // Normal decision point

}


/** 
 * Take us back to the initial state 
 */
void BrazilSimulator::reset() {
    // Will automatically free memory associated with old event queues
    state  = initState;
} 


/**
 * Go through the list of eligible actions and start the ones
 * indicated by policy. Note that some combinations of actions will be
 * illegal. At the moment the simulator will put a note on the event
 * queue rather than start the conflicting task. This means the order
 * of tasks in the domain becomes important. Preferred tasks should be
 * earlier.
 */
void BrazilSimulator::startActions(Vector& actions) {

    assert(actions.size() == (size_t)numActions);

    for (BrazilStateUpdater::EligibleCIt i = state.getEligible().begin(); i != state.getEligible().end(); i++) {
	if ((bool)actions[i->first]) state.startAction(i->second);
    }

}


int BrazilSimulator::getNumActions() {

    return numActions;

}



