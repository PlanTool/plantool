/*
 *  ProbabilisticListener.h
 *  
 *  A Class can implement ProbabilisticListener and set itself to be the
 *  listener for a class implementing Probabilistic if it wants to receive notifications about
 *  changes to the probabilistic group.
 *
 *  Created by Owen Thomas on 23/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef probabilistic_listener
#define probabilistic_listener

class DOMOutcome;

class ProbabilisticListener
{
	public:
		virtual ~ProbabilisticListener () {} 
		
		virtual void outcomeAdded(DOMOutcome*) = 0;
		virtual void outcomeRemoved(DOMOutcome*) = 0;
		virtual void probabilityChanged (DOMOutcome*,double) = 0;
};
#endif
