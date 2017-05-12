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
 *  DOMDuration.h
 *  
 *  Base duration class, each  *
 *  Created by Owen Thomas on 9/03/06.
 *  
 *
 */
#ifndef inc_dom_duration
#define inc_dom_duration


#include "DOMWrapper.h"
#include <xercesc/dom/DOM.hpp>

/**
 * An abstract Brazilian duration.
 * 
 * Concrete durations must override this and provide an implementation for
 *  getName() and copy.
 */
class DOMDuration : public DOMWrapper
{

	public:
	
	virtual ~DOMDuration() { }
    
	/**
	 * Returns the name of the Duration.
	 * @return The name of the Duration.
	 */
	virtual char* getName() = 0;
	
	/**
	 * @author daa 
         * Every probability distribution has a mean. This
	 * helps planner code treat distributions in a uniform <sic>
	 * way.
	 * @return non-negative mean
	 */
	virtual time_t getMean() = 0;
	

	/**
	 * @author daa Every probability distribution can also be
         * sampled.  @return a sampled time from this
         * distribution. Guaranteed to be non-negative.
	 */
	virtual time_t getSample() = 0;
	
	/**
	 * Returns a pointer to a copy of this
	 * DOMDuration.
	 *
	 * @return a copy of this DOMDuration.
	 *
	 */
	virtual DOMDuration* copy() = 0;
	
	static DOMDuration* create (DOMElement* element);
	
	static bool canRead (DOMElement* element);
};

#endif
