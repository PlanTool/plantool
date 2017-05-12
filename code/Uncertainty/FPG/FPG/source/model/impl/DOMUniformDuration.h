/*
 *  DOMUniformDuration.h
 *  

 *  Created by Owen Thomas on 6/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef inc_dom_uniform_duration
#define inc_dom_uniform_duration

#include "DOMDuration.h"

XERCES_CPP_NAMESPACE_USE

/**
 *  A concrete implementation of DOMDuration that specifies a duration
 *  that's uniformly distributed over a range.
 */
class DOMUniformDuration : public DOMDuration
{
	public:
	
		DOMUniformDuration (DOMElement *duration)
		{
			this->node = duration;
		}
		
		/**
		 * Creates a DOMUniformDuration with the passed minimum and maximum durations.
		 * The internal DOMElement is created using the passed DOMDocument pointer.
		 *
		 * @throws invalid_argument if minDuration or maxDuration < 0.
		 * @throws invalid_argument if minDuration > maxDuration.
		 */
		DOMUniformDuration (time_t minDuration, time_t maxDuration, DOMDocument *doc);
		
		/**
		 * Returns a copy of this DOMUniformDuration, with a deep
		 * copy of the underlying DOMElement.
		 *
		 * @return a copy of this DOMUniformDuration.
		 */
		DOMDuration* copy ()
		{
			return new DOMUniformDuration ((DOMElement*)node->cloneNode(true));
		}
		
		~DOMUniformDuration()
		{
			if(node->getParentNode() == NULL)
			{
				node->release();
			}
		}
		
		/**
		 * Returns "Uniform".
		 *
		 * @return 'Uniform'
		 */
		virtual char* getName() 
		{
			return "Uniform";
		}
		
		
		time_t getMaxDuration ()
		{
			return getAttributeAsTime("max");
		}
		
		time_t getMinDuration ()
		{
			return getAttributeAsTime ("min");
		}



		/**
		 * @author daa
		 * For uniform duration this is ((max - min) + 1)/2.
		 * @return the fixed time.
		 */
		virtual time_t getMean ()
		{
		    time_t max = getAttributeAsTime("max");
		    time_t min = getAttributeAsTime("min");
		    return (max - min)/2;
		}
		

		/**
		 * @author daa
		 * For fixed duration this is just the fixed time.
		 * @return the fixed time.
		 */
		virtual time_t getSample ()
		{
		    time_t max = getAttributeAsTime("max");
		    time_t min = getAttributeAsTime("min");
		    return random()%(max - min) + min;
		}
		
		/**
		 * Sets the maximum duration value to the passed
		 * double value.
		 *
		 * \throws invalid_argument if duration < 0.
		 * \throws invalid_argument if duration < getMinDuration().
		 *
		 * Self use: called by constructor.
		 */
		
		void setMaxDuration (time_t duration);
				
		/**
		 * Sets the minimum duration value to the passed
		 * double value.
		 *
		 * \throws invalid_argument if duration < 0.
		 * \throws invalid_argument if duration > getMaxDuration().
		 *
		 * Self use: called by constructor.
		 */
		void setMinDuration (time_t duration);
};

#endif
