/*
 *  ActionListener.h
 *  
 *  A Class can implement ActionListener and set itself to be the
 *  listener for a DOMAction if it wants to receive notifications about
 *  changes to the action.
 *
 *  Created by Owen Thomas on 23/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 */
 

#ifndef action_listener
#define action_listener

class DOMProbabilistic;

class ActionListener
{
	public:
		virtual ~ActionListener () {}
		
		/**
		 * Signals that the Action's name
		 * has been changed.
		 */
		virtual void nameChanged() = 0;
		
		/**
		 * Signals that the Action's, deterministic
		 * effect, background colour has changed.
		 * 
		 */
		virtual void backgroundColourChanged () = 0;
		
		/**
		 * Signals that the Action has had a Probabilistic
		 * child added.
		 */
		virtual void probabilisticAdded (DOMProbabilistic*) = 0;
		
		virtual void probabilisticRemoved (DOMProbabilistic*) = 0;
};

#endif
