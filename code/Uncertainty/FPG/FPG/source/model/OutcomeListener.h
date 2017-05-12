/*
 *  OutcomeListener.h
 *  
 *  A Class can implement OutcomeListener and set itself to be the
 *  listener for a DOMOutcome if it wants to receive notifications about
 *  changes to the outcome.
 
 *  Created by Owen Thomas on 21/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef outcome_listener
#define outcome_listener
class OutcomeListener
{
	public:
		
		virtual ~OutcomeListener () { }
			
		/**
		 * Signals that the label of the Outcome has changed.
		 */
		virtual void labelChanged () = 0;
		
		/**
		 * Signals that the background colour has changed.
		 */
		 virtual void backgroundColourChanged () = 0;
		 
};
#endif
