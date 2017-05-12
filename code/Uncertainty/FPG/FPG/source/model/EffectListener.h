/*
 *  EffectListener.h
 *  
 *   A Class can implement EffectListener and set itself to be the
 *  listener for a DOMDelayedEffect if it wants to receive notifications about
 *  changes to the effect.
 
 *  Created by Owen Thomas on 21/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef effect_listener
#define effect_listener

class DOMAtomicEffect;


class EffectListener 
{
	public:
		
		virtual ~EffectListener() { }
		
		virtual void effectAdded (DOMAtomicEffect* effect) = 0;
		
		virtual void effectRemoved (DOMAtomicEffect* effect) = 0;
		/**
		 * Signals that the duration has been changed.
		 * i.e. the duration has been replaced with another
		 * duration, not that the duration value has changed.
		 */
		virtual void durationChanged () = 0;
		
};

#endif
