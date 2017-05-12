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

#ifndef _BrazilSimulator__h
#define _BrazilSimulator__h


#include"PGBasics.hh"

#include"DOMDomain.h"

#include"BrazilStateUpdater.h"
#include"PlanningListener.h"

using namespace libpg;


/**
 * Implementation of Simulator class for the PG library Really the
 * driver of the simulation process, implementing chosen actions,
 * updating the state, and returning the next state description
 * vector. Covers translation of BrazilState information into Vector
 * format for the PG library.
 */
class BrazilSimulator : public Simulator {

 public:
    typedef enum {MEDIAN_MODE, SAMPLED_MODE} Mode;
    
 private:

    // Effectively the current state
    BrazilStateUpdater state;
    
    // The initial state can be overrided
    BrazilState initState;
    
    // Remember the last terminal state, for debugging and regression test mode
    BrazilState lastTerminalState;

    DOMDomain* domain;

    DOMPropertySet* props;

    double avgMS;

    /** 
     *	Optional (may be NULL). If not null, every time a trial ends
     *  the final state will
     *  be sent to this listener. The listener must make a copy if needed.
     */
    PlanningListener* planningListener;

    /** 
     * Most likely, or stochastic
     */
    Mode mode;

    /**
     * How many PDDL actions?
     */
    int numActions;

    /** 
     * How many predicate, i.e, state variables
     */
    int numPredicates;


    /**
     * What is the next reward going to be
     */
    double nextReward;

    /**
     * How many completed episodes
     */
    int episodes;

    void startActions(Vector& actions);

    int nextDecisionPoint();

 public:
    
    
    /**
     * Simulator just needs to know about the domain.
     * The initial state will be constructed from the Problem instance within
     * this domain, but may be later overriden by setInitialState()
     */
    BrazilSimulator(DOMDomain* domain);

    virtual ~BrazilSimulator() { }

    /** 
     * Override the default initial state in the domain
     */
    void setInitialState(BrazilState* newInitialState);

    void reset();
    
    /**
     * This will be the most recent state. Useful for returning goal
     * states to a Planning Listener
     */
    BrazilState* getCurrentState();


    /**
     * Returns the last terminal state encountered. Useful for regression
     * testing mode.
     */
    BrazilState& getLastTerminalState() { return lastTerminalState; }

    /**
     * Someone wants to know each time we end a trial.
     * Set to NULL to stop listener being triggered.
     */
    void setPlanningListener(PlanningListener* pl) { planningListener = pl; }
    PlanningListener* getPlanningListener() { return planningListener; }

    /**
     * Set whether simulation happens in average mode.  In average
     * mode, continuous CDFs are sampled, and the most probable
     * outcome is chosen
     */
    void setMode(Mode mode) { this->mode = mode; }

    /** 
     * get current mode
     * @see setMedianMode()
     */
    Mode getMode() { return mode; }


    /**
     * Get the average makespan
     */
    double getAvgMS() { return avgMS; }

    //
    // Interface for PG library.
    //
    
    virtual int getObsRows();
    virtual int getObsCols();
    virtual int getAgents();
    virtual int getActionDim();
    virtual int getRewardDim();
    virtual void getReward(Vector& rewards);
    virtual void getObservation(Observation& obs);
    virtual int  doAction(Vector& action);
    virtual int  getNumActions();

};

#endif
