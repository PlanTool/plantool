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
 *  DOMFixedDuration.h
 *  
 *
 *  Created by Owen Thomas on 2/03/06.
 *  
 *
 */
 
#ifndef inc_dom_fixed_duration
#define inc_dom_fixed_duration

#include <xercesc/dom/DOM.hpp>

#include "DOMDuration.h"

/**
 *  Fixed Duration implementation of DOMDuration.
 *  A Fixed Duration has a positive double fixed value.
 */
class DOMFixedDuration : public DOMDuration
{
	public:
	
		/**
		 * Creates a DOMFixedDuration over the specified DOMElement.
		 * A Fixed duration has a single, fixed value that the duration
		 * will always take.
		 */
		DOMFixedDuration (DOMElement *duration)
		{
			this->node = duration;
		}
		
		/**
		 * Creates a DOMFixedDuration with the specified duration.
		 * 
		 * Creates a DOMElement from the specified DOMDocument.
		 */
		DOMFixedDuration (time_t duration, DOMDocument *document);
		
				/**
		 * Returns a copy of this DOMFixedDuration, with a deep
		 * copy of the underlying DOMElement.
		 *
		 * @return a copy of this DOMFixedDuration.
		 */
		DOMDuration* copy ()
		{
			return new DOMFixedDuration ((DOMElement*)node->cloneNode(true));
		}
		
		/**
		 * Returns "Fixed".
		 * @return 'Fixed'.
		 */
		virtual char* getName() 
		{
			return "Fixed";
		}
		
		/**
		 * This will release the internal DOMElement
		 * if it has no parent node.
		 */
		~DOMFixedDuration()
		{
			if(node->getParentNode() == NULL)
			{
				node->release();
			}
		}

		/**
		 * Returns the fixed duration.
		 * @return the fixed duration.
		 */
		time_t getDuration ()
		{
			return getAttributeAsTime ("duration");
		}


		/**
		 * @author daa
		 * For fixed duration this is just the fixed time.
		 * @return the fixed time.
		 */
		virtual time_t getMean ()
		{
		    return getDuration();
		}
		

		/**
		 * @author daa
		 * For fixed duration this is just the fixed time.
		 * @return the fixed time.
		 */
		virtual time_t getSample ()
		{
		    return getDuration();
		}
		

		/**
		 * Sets the duration to the specified double value.
		 *
		 * Self use: called by Constructor
		 */
		void setDuration(time_t duration)
		{
			setAttribute ("duration", duration);
		}
};

#endif
