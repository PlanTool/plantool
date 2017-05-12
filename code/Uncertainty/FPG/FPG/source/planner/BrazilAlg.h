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

#ifndef _BrazilAlg_h
#define _BrazilAlg_h

#include<PGBasics.hh>
#include<RLAlg.hh>
#include<OLPomdp.hh>
#include<GOALPomdp.hh>
#include"PlanningListener.h"

/**
 * BrazilAlg is essentially the GOALPomdp alg from the libpg library,
 * but we need one small change. If there's a PlanningListener we need
 * to get a most likely plan every so often during optimisation.
 */
class BrazilAlg : public GOALPomdp {

 public:

    BrazilAlg(Controller* controller, Simulator* simulator, double discount, double stepSize) :
	GOALPomdp(controller, simulator, discount, stepSize) {};

    virtual ~BrazilAlg() {};

    virtual double learnCore(int stepsPerEpoch, int& totalSteps);
    virtual void printPerformanceInfo(bool printTitles, 
				      int steps, 
				      double avgReward, 
				      double maxParam, 
				      int seconds);
};
#endif
