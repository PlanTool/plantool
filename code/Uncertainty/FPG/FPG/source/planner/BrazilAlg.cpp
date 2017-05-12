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



#include<time.h>
#include<iomanip>

#include"BrazilSetup.h"
#include"BrazilState.h"
#include"BrazilSimulator.h"
#include"BrazilAlg.h"

using namespace std;


/**
 * This is overriden from GOALPomdp in libpg. Only mod is that if 
 * there is a Planninglistener it will get a median plan  and give
 * it to the planning listener before each optimisation burst.
 * By default this is very simple, call doSteps for stepsPerEpoch times,
 * leaving online learning to doSteps. In other BrazilAlgs this might 
 * call doSteps to build up a batch estimate and then do a line search.
 */
double BrazilAlg::learnCore(int stepsPerEpoch, int& totalSteps) {

    double lastEpochVal;
    Vector totalRewards(simulator->getRewardDim());
    totalRewards.clear();

    BrazilSimulator* s = (BrazilSimulator*)simulator;
    // The new bit. If there's a planning listener, generate a median state
    // for it
    if (s->getPlanningListener() != NULL) {
	s->setMode(BrazilSimulator::MEDIAN_MODE);
	s->reset();
	doSingleEpisode(totalRewards, false); // Simulator will call planningListener
	s->setMode(BrazilSimulator::SAMPLED_MODE);
	totalRewards.clear();
    }


    // Learn
    lastEpochVal = doSteps(totalRewards, stepsPerEpoch, true);
    totalSteps += stepsPerEpoch;
    if (useAutoBaseline) baseline.assign(totalRewards);
   

    return lastEpochVal;

}



    /**
     * Print a line of performance information
     * @param  are we only printing the headers?
     * @param  total number of steps so far
     * @param  average reward over last epoch
     * @param  maximum parameter
     * @param  time elapsed so far.
     * This overloaded method from GOALAlg additionally prints prob(success) and avg. makespan
     */
    void BrazilAlg::printPerformanceInfo(bool printTitles, 
					 int steps, 
					 double avgReward, 
					 double maxParam, 
					 int seconds) {
	cout<<setprecision(PRECISION);
	if(printTitles) {
	    cout<<"       "
		<<setw(CWIDTH)<<"Steps"
		<<setw(CWIDTH)<<"R"
		<<setw(CWIDTH)<<"max(Param)"
		<<setw(CWIDTH)<<"Seconds"
		<<setw(CWIDTH)<<"Episodes"
		<<setw(CWIDTH)<<"Succ%"
		<<setw(CWIDTH)<<"Avg.MS"<<endl;
	}
	cout<<"Trial: "
	    <<setw(CWIDTH)<<steps
	    <<setw(CWIDTH)<<avgReward
	    <<setw(CWIDTH)<<maxParam
	    <<setw(CWIDTH)<<seconds
	    <<setw(CWIDTH)<<totalEpisodes
	    <<setw(CWIDTH)<<successes/(double)episodes
	    <<setw(CWIDTH)<<((BrazilSimulator*)simulator)->getAvgMS()<<endl;
    }

