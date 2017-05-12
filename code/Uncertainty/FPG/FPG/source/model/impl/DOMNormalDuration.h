/*
 *  DOMNormalDuration.h
 *  
 *  Created by Owen Thomas on 2/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef inc_dom_normal_duration
#define inc_dom_normal_duration

#include "DOMDuration.h"

/**
 *  A concrete implementation of DOMDuration that represents
 *  a normally distributed duration.
 *
 */
class DOMNormalDuration : public DOMDuration
{
	public:
		DOMNormalDuration (DOMElement *duration)
		{
			this->node = duration;
		}
		
		/**
		 * Creates a DOMNormalDuration with the passed mean and standard
		 * deviation.
		 *
		 * The internal DOMElement is created from the passed DOMDocument
		 * pointer.
		 *
		 * 
		 * \throws invalid_argument if stdDev or mean < 0.
		 *
		 */
		DOMNormalDuration (time_t mean, time_t stdDev, DOMDocument *document);
		
		
		
	/**
	 * Returns a copy of this DOMNormalDuration, with a deep
	 * copy of the underlying DOMElement.
	 *
	 * @return a copy of this DOMNormalDuration.
	 */
	DOMDuration* copy ()
	{
		return new DOMNormalDuration ((DOMElement*)node->cloneNode(true));
	}
	
	
	~DOMNormalDuration()
	{
		if(node->getParentNode() == NULL)
		{
			node->release();
		}
	}
		
		/**
		 * Returns "Normal"
		 * @return 'Normal'
		 */
		
		virtual char* getName() 
		{
			return "Normal";
		}
		
		/**
		 * Self-use: Called by constructor.
		 */
		void setMean (time_t mean)
		{
			setAttribute ("mean", mean);
		}

		/**
		 * Self-use: Called by constructor.
		 */
		void setStandardDeviation (time_t stdDev)
		{
			setAttribute ("standardDeviation", stdDev);
		}
		
		virtual time_t getMean()
		{
			return getAttributeAsTime ("mean");
		}

		virtual time_t getSample();		

		time_t getStandardDeviation()
		{
			return getAttributeAsTime("standardDeviation");
		}

};

#endif
