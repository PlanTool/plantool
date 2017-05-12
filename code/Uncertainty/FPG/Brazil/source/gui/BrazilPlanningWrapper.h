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
 *  AbstractBrazilPlanningWrapper.h
 *  
 *
 *  Created by Owen Thomas on 21/09/06.
 *  
 *
 */

#ifndef brazil_planning_wrapper
#define brazil_planning_wrapper

class BrazilState;
using namespace std;
#include <vector>
#include "../planner/PlanningListener.h"

#include "BrazilPlanningThread.h"
#include <iostream>

#include <QObject>

#include "../model/impl/DOMWrapper.h"

#include <vector>

class BrazilPlanningWrapper : public QObject, public PlanningListener {
	Q_OBJECT
	
	signals:
		/**
		 * The contract here is that after the newState signal is emitted
		 * state may be deleted. If any receivers of this signal wish to
		 * "hold on" to state, they must make a copy.
		 */
		void newState (BrazilState* state);
		
		/**
		 * After this signal is emitted the state may be deleted.
		 * If receives of this signal wish to "hold on" to state, they
		 * must make a copy.
		 */
		void newMedianState (BrazilState* state);
		
		//
		//emitted when the wrapper starts and stops planning
		//
		
		void started (bool isPlanning);
		void stopped ();

	public slots:
		
		
		
		virtual void start (bool plan) = 0;
		
		virtual void start () {
			start (true);
		} //default params don't work well with slots.
		
		virtual void stop () = 0;
		virtual void medianPlan (BrazilState* withInitialState = NULL) = 0;
		
	public:
		virtual ~BrazilPlanningWrapper ( ) { }
		virtual bool isPlanning () = 0;
		virtual bool isSimulating () = 0;
		
		
		virtual void setInitialState (BrazilState*) = 0;
		virtual void setParameters (std::vector<double>) = 0;
		virtual std::vector<double> getParameters () = 0;
		
		virtual void setDomain (DOMDomain&) = 0;
		
		
		//
		//PlanningListener impl
		//
		virtual void stochasticState (BrazilState* state) {
		    emit newState (new BrazilState(*state));
		}
		
		virtual void medianState (BrazilState* state) {
		    emit newMedianState (new BrazilState(*state));
		}
		
		virtual void parameterBackup (std::vector<double> ) {
			cout << "NYI" << endl;
		}
};

#endif




