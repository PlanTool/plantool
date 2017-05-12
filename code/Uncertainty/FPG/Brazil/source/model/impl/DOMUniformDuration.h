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
 *  DOMUniformDuration.h
 *  

 *  Created by Owen Thomas on 6/03/06.
 *  
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
