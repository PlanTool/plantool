/*
 *  DOMProbabilistic.h
 *  
 *
 *  Created by Owen Thomas on 18/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_probabilistic
#define dom_probabilistic

#include <map>
#include <xercesc/dom/DOM.hpp>
#include "DOMWrapper.h"
#include "../Probabilistic.h"
#include "../ProbabilisticListener.h"
#include "DOMOutcome.h"

#include <stdexcept>

class DOMAction;


class DOMProbabilistic : public DOMWrapper, public Probabilistic
{
	private:
		//Document used to create the element this model represents.
		DOMDocument* doc;
		
		//An object listening to changes to the probabilistic component
		//of the action (e.g. adding /removing outcomes).
		ProbabilisticListener* listener;
		
		DOMAction* parent;

		// [daa] Keep track of most likely outcome for fast
		// response in most likely outcome mode of planner.
		DOMOutcome* mostLikelyOutcome;

		map <DOMOutcome*, double> outcomeProbabilities;
		
	protected:
	
		/**
		 * Scan through all outcomes for most likely.
		 * Not sensitive to cummulative prob.
		 * Side effect is to set class var mostLikelyOutcome
		 * @author daa
		 */
		DOMOutcome* findMostLikelyOutcome();

	public:
		
		DOMProbabilistic (DOMElement* element, DOMAction* parent);
		
		DOMProbabilistic (DOMDocument* doc, DOMAction* parent);
		
		virtual ~DOMProbabilistic () { }
				
		virtual int getNumberOfOutcomes ()
		{
			return this->outcomeProbabilities.size();
		}
		
		virtual void setProbabilisticListener (ProbabilisticListener* listener)
		{
			this->listener = listener;
		}
		

		

		virtual map<DOMOutcome*, double>& getOutcomes ()

		{
			return outcomeProbabilities;
		}
		
		virtual DOMAction* getParent () {
			return parent;
		}

		/**
		 * A generic method for setting the DOMOutcomes within this 
		 * probabilistic or adjusting the probabilities of existing
		 * DOMOutcomes.
		 *
		 */
		virtual void setOutcomes (map <DOMOutcome*, double> probabilities);		


		/** 
		 * Assumes probabilities sum to 1.0
		 * @author daa
		 */
		virtual DOMOutcome* sampleOutcome();


		/**
		 * Return result of watching all probs and prob
		 * changes for most likely outcome.
		 * @author daa 
		*/
		virtual DOMOutcome* getMostLikelyOutcome() {
		    assert(mostLikelyOutcome != NULL);
		    return mostLikelyOutcome;
		}

};

#endif
