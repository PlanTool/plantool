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
 *  DOMNormalDuration.h
 *  
 *  Created by Owen Thomas on 2/03/06.
 *  
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
