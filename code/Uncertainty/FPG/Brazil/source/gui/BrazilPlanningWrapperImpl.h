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
 *  BrazilPlaningWrapper.h
 *  
 *
 *  Created by Owen Thomas on 26/07/06.
 *  
 *
 */

#ifndef brazil_planning_wrapper_impl
#define brazil_planning_wrapper_impl

class DOMDomain;
class DOMProblem;
class BrazilState;
class BrazilPlanner;

using namespace std;

#include <QObject>
#include <QThread>

#include "BrazilPlanningWrapper.h"

/**
 * A generic brazil simulation wrapper that during simulation/planning emits
 * newState signals.
 */
class BrazilPlanningWrapperImpl : public BrazilPlanningWrapper {
	
	
	private:
	
		//The current domain, this can be changed through set domain
		//A brazil planning wrapper will always have a domain.
		DOMDomain* domain;
		
		//The current planning thread. Created with start (bool).
		BrazilPlanningThread* planningThread;
		
		//The planner, passed to the planning thread. The planner is
		//consistent and is created in the constructor.
		BrazilPlanner* planner;
		
		BrazilState* initState;
		
		/*
		 * Indicate whether the planner is currently planning or 
		 * simulating. If both are false then the planner is not
		 * currently doing either. 
		 *
		 * These are both set to false when the request to stop 
		 * the planner is received, not when the planner actually
		 * stops planning. The planner will generate a new
		 * state after a call to stop ().
		 *
		 */
		bool planning, simulating;
			
	public slots:
		
		/**
		 * Creates a new planning thread,starts the planning thread and
		 * emits the started signal.
		 */
		virtual void start (bool plan = true);
		
		/**
		 * Stops the planning thread, this is currently not fully implemented.
		 * It should also delete the planning thread too.
		 */
		virtual void stop ();
		 
		virtual void medianPlan (BrazilState* withInitialState = NULL);

	public:
		
		//Planner and planning thread controlled in an instance of this class.
		BrazilPlanningWrapperImpl (DOMDomain& domain);
		
		virtual ~BrazilPlanningWrapperImpl () { 
		}
				
		virtual void setInitialState (BrazilState* init);
		
		virtual bool isPlanning () { 
			return planning;
		}
		
		virtual bool isSimulating () { 
			return simulating;
		}
		
		virtual void setParameters (std::vector<double> params) {
			cout << "NYI" << &params << endl;
		 }
		
		virtual std::vector<double>  getParameters (){ 
			cout << "NYI" << endl;
			std::vector<double> pList;
			
			return pList;
		}
		
		virtual void setDomain (DOMDomain& domain) { 
			this->domain = &domain;
		}
		
		//Override methods in BrazilPlanningWrapper
		
		virtual void stochasticState  (BrazilState* state) {
			if(planning || simulating) emit newState (new BrazilState(*state));
		}
		
		virtual void medianState (BrazilState* state) {
			if(planning || simulating) emit newMedianState (new BrazilState (*state));
		}
};

#endif


