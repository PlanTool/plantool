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
 *  Probabilistic.h
 *  
 *  An interface representing a grouping of probabilities. The
 *  sum of all Outcomes within this probabilistic group must sum
 *  to be 1.0. A Probabilistic instance can manipulate the probabilities
 *  of its child DOMOutcomes to ensure this.
 *
 *  Currently this is implemented by DOMAction.
 *
 *  Created by Owen Thomas on 13/03/06.
 *  
 *
 */

#ifndef inc_probabilistic
#define inc_probabilistic

#include "ProbabilisticListener.h"
#include <map>

class DOMOutcome;
class DOMAction;

class Probabilistic 
{
	
	public:
		
		virtual ~Probabilistic() { }
		
		//add, remove Outcomes of this Probabilistic.
		
		virtual map<DOMOutcome*, double>& getOutcomes () = 0;
		

		virtual void setOutcomes ( map<DOMOutcome*, double> ) = 0;


		
		virtual int getNumberOfOutcomes () = 0;
		
		virtual void setProbabilisticListener (ProbabilisticListener*) = 0;
		
		virtual DOMAction* getParent () = 0;

		/**
		 * Generic way of sampling from an outcome
		 * @return pointer to sampled outcome
		 * @author daa
		 */
		virtual DOMOutcome* sampleOutcome() = 0;

		/** 
		 * Get most likey outcome
		 * @author daa
		 */
		virtual DOMOutcome* getMostLikelyOutcome() = 0;
};
#endif
