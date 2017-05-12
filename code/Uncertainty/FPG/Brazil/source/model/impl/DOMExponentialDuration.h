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
 *  DOMExponentialDuration.h
 *  
 *  Created by Owen Thomas on 3/03/06.
 *  
 *
 */
 
#ifndef inc_dom_exponential_duration
#define inc_dom_exponential_duration
 
#include <math.h>
#include "DOMDuration.h"

/**
 * A concrete implementation of DOMDuration that represents
 * an exponentially distributed duration.
 *
 */
class DOMExponentialDuration : public DOMDuration
{
	public:
		DOMExponentialDuration (DOMElement *duration)
		{
			this->node = duration;
		}
		
		/**
		 * Creates a DOMExponentialDuration with the passed lambda
		 * value.
		 *
		 * The internal DOMElement is created from the passed
		 * DOMDocument pointer.
		 *
		 * @throw invalid_argument if lambda <= 0.
		 */
		DOMExponentialDuration (double lambda, DOMDocument *doc);
				
		

		DOMDuration* copy ()
		{
			return new DOMExponentialDuration ((DOMElement*)node->cloneNode(true));
		}
		
		~DOMExponentialDuration()
		{
			if(node->getParentNode() == NULL)
			{
				node->release();
			}
		}
		
		/**
		 * Returns "Exponential"
		 * @return 'Exponential'
		 */
		virtual char* getName() 
		{
			return "Exponential";
		}
		
		/**
		 * Self-use: called by Constructor.
		 *
		 * @throw invalid_argument if lambda <= 0.
		 */
		void setLambda (double lambda)
		{
			if(lambda <= 0) throw std::invalid_argument ("lambda <= 0");
			setAttribute ("lambda", lambda);
		}
		
		/**
		 * Convienience method, this is equivalent to
		 * calling setLambda (1.0 / mean).
		 */
		void setMean (time_t mean)
		{
			if(mean <= 0)
				throw std::invalid_argument ("mean <= 0");
			
			setAttribute ("lambda", 1.0 / mean);
		}
		
		double getLambda ()
		{
			return getAttributeAsDouble ("lambda");
		}
		
		virtual time_t getMean ()
		{
			return (unsigned int) (1.0 / getAttributeAsDouble ("lambda"));
		}

		/**
		 * @author daa
                 * Sample an exponential distribution. Trick is to
		 * sample from a uniform distribution then do inverse
		 * transform sampling.
		 */
		virtual time_t getSample () 
		{
		    return (time_t)(-log(random()/(double)RAND_MAX)/getAttributeAsDouble ("lambda"));
		}

};

#endif
