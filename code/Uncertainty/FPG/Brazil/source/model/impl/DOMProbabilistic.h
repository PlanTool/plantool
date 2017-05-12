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
 *  DOMProbabilistic.h
 *  
 *
 *  Created by Owen Thomas on 18/05/06.
 *  
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
