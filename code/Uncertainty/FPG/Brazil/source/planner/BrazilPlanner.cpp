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

/**
 * Implictly set a static variable describing what debug
 * category is default for this file. Must be here if DBG
 * macro is used. Can or together different debug modes.
 * Should be done after declaring BrazilSetup, but before
 * other includes for planner.
 */
DBG_MODULE(DBG_PLANNER);

#include"PlanningException.h"
#include"DOMDomain.h"
#include"DOMProblem.h"

#include"PGBasics.hh"
#include"OLPomdp.hh"
#include"GOALPomdp.hh"
#include"NeuralNet.hh"
#include"BasicController.hh"
#include"BinaryController.hh"
#include"FactoredController.hh"

#include"BrazilEvent.h"
#include"BrazilState.h"
#include"BrazilStateHelpers.h"
#include"BrazilStateUpdater.h"
#include"PlanningListener.h"
#include"BrazilSimulator.h"
#include"BrazilPlanner.h"
#include"BrazilAlg.h"

using namespace std;
using namespace libpg;


/** 
 * Yes, ugly, ugly global.. but so convenient for global debugging
 * The higher the number, the higher the verbosity
 */
int brazilPlannerDebugLevel = 0; // Default is no debugging

/** 
 * What is the global list of modules we are interested
 * in debugging? By default, nothing.
 */
unsigned int brazilDebugModule = 0;


/**
 * Create a new Brazil planning engine from domain description.
 * @param Domain from GUI or loaded from a file
 */
BrazilPlanner::BrazilPlanner(DOMDomain* d) : domain(d)  {

    simulator = new BrazilSimulator(domain);
    controller = buildControllers();

    double discount;
    double stepSize;
    try {
	discount = domain->getProperties()->getPropertyDouble("planner discount");
	if (DBG(1)) BOUT<<"Discount factor: "<<discount<<endl;
	stepSize = domain->getProperties()->getPropertyDouble("planner step size");
	if (DBG(1)) BOUT<<"Step size: "<<stepSize<<endl;
    } catch(DOMPropertySet::PropertyException &pe) {
	BOUT<<"Missing critical property: "<<pe.what()<<endl;
	terminate();
    }
    planner = new BrazilAlg(controller, simulator, discount, stepSize); 
    planner->setUseAutoBaseline(false);
    planning = false;
}


/**
 * Help the constructor to build all the controllers for this instance
 * of the planner
 */
Controller* BrazilPlanner::buildControllers() {
       
    assert(simulator != NULL);

    // Simulator now allows us to get a precise description of
    // required controllers and their inputs
    for (int c=0; c < simulator->getAgents(); c++) {
	// Always single output
	
	controllers.push_back((Controller*)new BinaryController(new NeuralNet(simulator->getObsRows(), 1)));
    }
 
    // No obs splitting or local rewards
    return new FactoredController(controllers, false, false);
    
}


/**
 * Someone wants to listen in on the inner workings of the planner. Typically the GUI.
 */
void BrazilPlanner::setPlanningListener(PlanningListener* pl) {
    simulator->setPlanningListener(pl);
}


/**
 * Override the default initial state specified in the domain. This
 * might mean a user is playing with the GANTT chart, as a prelude to
 * replan from the state they have on their screen, or maybe they just
 * want to unfold the plan from the state they are in given the
 * current set of parameters.  A copy of this state will be made.
 */
void BrazilPlanner::setInitialState(BrazilState* newInitState) {
    simulator->setInitialState(newInitState);
}


/**
 * Return the median plan given current initial state and current
 * parameters.  This must not be called during planning. During
 * planning the planner will automatically generate medianPlan
 * messages as progress updates. Calling this will also generate a
 * message to any PlanningListeners.  This routine CAN be called
 * during non-planning statistics gathering.  Median plan means: mean
 * is returned for CDFs Highest prob outcome is returned for discrete
 * outcomes.  So this isn't really a median of the distributions, but
 * neither is it a mean.  Should find a more rigourous description.
 * @return pointer to a Brazil state that will change the next time
 * medianPlan() is called.
 * @exception Planning Exception if currently planning
 */
void BrazilPlanner::medianPlan() {

    Vector totalRewards(1);
    
    if (planning) throw PlanningException("BrazilPlanner::medianPlan() called while planning\n");
    simulator->reset();
    simulator->setMode(BrazilSimulator::MEDIAN_MODE);
    planner->doSingleEpisode(totalRewards, false);
    simulator->setMode(BrazilSimulator::SAMPLED_MODE);
	
}


/**
 * Run a single episode in median mode and print the resulting state.
 * Then quit.
 * Console output can be compared to a baseline
 */
void BrazilPlanner::regressionTest() {

    cout<<"Starting regression test...\n";
    medianPlan();
    cout<<"Regression output:"<<endl<<simulator->getLastTerminalState()<<endl;
	
}


/**
 * Request termination of planning or statistics gathering. This
 * should be called from the GUI thread since the planning thread is
 * presumably still executing in run(). This will not happen
 * instantaneously, but rather at the end of the next planning epoch
 * (a few seconds at most). The end of planning (but not statistics)
 * will guarantee that the final median plan is generated.  The GUI
 * will know that the interrupt has happened because
 * BrazilPlanner::run() will exit.
 */
void  BrazilPlanner::requestInterrupt() {
    // This will stop planning at the end of the next epoch.
    planner->setMaxSteps(1);
}



/**
 * Provide planner with some new parameters (a.k.a, the actual "plan")
 * that will completely wipe the last lot so make sure you do a
 * getParameters if you don't want to loose the last plan.
 * @exception throws PlanningException if param size mismatch
 */
void BrazilPlanner::setParameters(std::vector<double>& params) {

    if (controller->getNumParams() != (int)params.size()) {
	throw PlanningException("In BrazilPlanner::setParameters() params.size() != controller->getNumParams()");
    }

    Vector v(params.size());
    for (unsigned int p=0; p < v.size(); p++) {
	v[p] = params[p];
    }

    controller->scatter(v, Approximator::PARAMS);

}


/**
 * Return whatever the controllers currently have for their parameters
 * @param provide an empty double vector into which to put all the parameters
 */
void  BrazilPlanner::getParameters(std::vector<double>& params ) {
    
    Vector v(controller->getNumParams());
    controller->reduce(v, Approximator::PARAMS);
    for (unsigned int p=0; p < v.size(); p++) {
	params[p] = v[p];
    }
    
}



/**
 * Do the planning!!! 
 * Returns when planning is complete or interrupted, or out of time.
 * @returns final median state
 */
void BrazilPlanner::run(bool plan) {
    if (DBG(1)) BOUT<<"planning starts" << endl;
    planning = true;

    // Stochastic and changing params.
    if (plan) planner->learn(
      domain->getProperties()->getPropertyInt("planner epoch steps"),
      domain->getProperties()->getPropertyInt("planner max time"));

    else {
	// Does not change params. Still stochastic.
	planner->evaluateAndPrint(domain->getProperties()->getPropertyInt("planner stochastic evaluation steps"), 0); 
    }
    planning = false;
    if (DBG(1)) BOUT<<"planning ends" << endl;
    medianPlan();
}
