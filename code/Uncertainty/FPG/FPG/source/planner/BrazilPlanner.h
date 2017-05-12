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

#ifndef _BrazilPlanner__h
#define _BrazilPlanner__h

#include"DOMDomain.h"

#include"PGBasics.hh"
#include"FactoredController.hh"
#include"OLPomdp.hh"
#include"GOALPomdp.hh"
#include"BrazilSimulator.h"
#include"BrazilAlg.h"

using namespace libpg;

/**
 * Top level of planning code except for main() file. Serves as
 * interface GUI uses to talk to planner.
 */
class BrazilPlanner {

 private: 

    // Top level domain and problem, although we may choose to override initial state
    DOMDomain* domain;

    // The simulator where most of the real work gets done
    BrazilSimulator* simulator;

    // The FPG planning algorithm
    BrazilAlg* planner;

    // Keep a record of all the controllers for final deletion. (This is a vector<controller*>)
    FactoredController::Controllers controllers;
    Controller* controller;

    Controller* buildControllers();

    // Sanity check variable to make sure median plan is never called during planning.
    // Maybe take this out when things have been debugged a bit.
    bool planning;

 public:
    
    BrazilPlanner(DOMDomain* d);
    ~BrazilPlanner();
    
    void setInitialState(BrazilState* newInitState);

    void setPlanningListener(PlanningListener* pl);
    
    void run(bool plan);
    void requestInterrupt();

    void regressionTest();
    void medianPlan();
    
    void setParameters(std::vector<double>& params);
    void getParameters(std::vector<double>& params);
    
};

#endif
