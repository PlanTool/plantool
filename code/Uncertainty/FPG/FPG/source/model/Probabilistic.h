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
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
