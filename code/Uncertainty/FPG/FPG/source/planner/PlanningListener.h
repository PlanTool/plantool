/*
 * $Id: BrazilState.cpp 80 2006-06-14 05:18:04Z daa $ 
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

#ifndef _PlanningListener__h
#define _PlanningListener__h

/**
 * A collection of messages to pass back to the GUI about events *during* simulatation of many plan trials.
 * At the moment just used to pass the completed State at the end of each trial. Used to render the GUI and
 * gather statistics.
 */
class PlanningListener {

 public:

    /**
     * Stop compiler whinging
     */
    virtual ~PlanningListener() {};

    /**
     * Planner has computed the "median" state for GANTT chart display
     * State will be corrupted after return, so listener must be
     * finished with it by return.
     */
    virtual void medianState(BrazilState* state) = 0;

    /**
     * Planner has computed one stochastic execution of the plan, and
     * passes that plan to the listener for it to compute whatever
     * statistics it desires.  State will be corrupted after return,
     * so listener must take a copy or extract relevant statistics
     * immediately.
     */
    virtual void stochasticState(BrazilState* state) = 0;

    /**
     * This will be called by the planner to occasionally (every few
     * minutes) notify the GUI (or anything else) that it should make
     * a copy of the parameters. This is so that ifa long planning
     * run is interrupted, all the effort is not lost. 
     */
    virtual void parameterBackup(std::vector<double> params) = 0;
    
};
#endif
