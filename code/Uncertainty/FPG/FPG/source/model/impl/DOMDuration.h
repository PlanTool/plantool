/*
 *  DOMDuration.h
 *  
 *  Base duration class, each  *
 *  Created by Owen Thomas on 9/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
