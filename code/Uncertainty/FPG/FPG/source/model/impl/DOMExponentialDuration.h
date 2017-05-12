/*
 *  DOMExponentialDuration.h
 *  
 *  Created by Owen Thomas on 3/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
