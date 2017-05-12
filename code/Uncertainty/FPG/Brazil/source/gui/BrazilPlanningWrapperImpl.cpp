/*
 *  BrazilPlanningWrapper.cpp
 *  
 *
 *  Created by Owen Thomas on 8/09/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "BrazilPlanningWrapperImpl.h"
#include "../planner/BrazilPlanner.h"

BrazilPlanningWrapperImpl::BrazilPlanningWrapperImpl (DOMDomain& domain) {
	this->domain = &domain;
	
	planner = NULL;	
	planningThread = NULL;
	initState = NULL;
}

void BrazilPlanningWrapperImpl::start (bool plan) {
	planning = plan;
	simulating = !plan;
	
	planner = new BrazilPlanner (domain);
	
	if(initState) {
		planner->setInitialState (initState);
	}
	
	planner->setPlanningListener (this);
	
	//create thread.
	planningThread = new BrazilPlanningThread (planner, plan);
	
	//start thread
	planningThread->start ();
	
	emit started (plan);
	
}

void BrazilPlanningWrapperImpl::stop () {
	
	emit stopped ();

	if(!planningThread) return;
	
	//Interrupt
	planner->requestInterrupt ();
	planning = simulating = false;
}

void BrazilPlanningWrapperImpl::setInitialState (BrazilState* state) {
	initState = state;
	planner->setInitialState (state);
}

/**
 * [daa] We should be in the planning thread here 
 */
void BrazilPlanningWrapperImpl::medianPlan (BrazilState* init) {
	if(!planner) return;
	setInitialState (init);
	
	bool initSimValue = simulating;
	bool initPlanValue = planning;
	
	simulating = true;
	planning = false;
	
	planner->medianPlan ();
	
	simulating = initSimValue;
	planning = initPlanValue;
}




