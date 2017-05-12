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
 *  BrazilPlanningThread.h
 *  
 *
 *  Created by Owen Thomas on 8/09/06.
 *  
 *
 */

#include <QThread>
#include "PGBasics.hh"
#include "OLPomdp.hh"
#include "GOALPomdp.hh"
#include "FactoredController.hh"
//#include "Controller.hh"


#include "../model/impl/DOMWrapper.h"
#include "../model/impl/DOMRootCondition.h"
#include "../model/impl/DOMFunction.h"
#include "../model/impl/DOMPredicate.h"
#include "../model/impl/DOMOutcome.h"

#include "../planner/BrazilState.h"
#include "../planner/BrazilStateHelpers.h"
#include "../planner/BrazilStateUpdater.h"
#include "../planner/BrazilSimulator.h"
#include "../planner/BrazilPlanner.h"

#include <iostream>
using namespace std;

class BrazilPlanningThread : public QThread{

	private:
		BrazilPlanner* planner;
		bool plan;
	public:
		
		BrazilPlanningThread (BrazilPlanner* planner, bool plan) {
			this->planner = planner;
			this->plan = plan;
		}
		
		virtual void run () {
			//simply call planner.run.
			planner->run (plan);
			
			//this will return at approp. time.
			
			//I probably don't need to call exec as the planner will not
			//generate any signals etc.
			//exec ();
			
			//But I need to have a test so that I know when it's finished. 
			//Actually this test must happen in the planner.
			//planner->run (plan);

			//This means I will need some other method in this class to pass
			//a param to the planner, telling it to stop.
		}
		
	};


