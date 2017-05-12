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
