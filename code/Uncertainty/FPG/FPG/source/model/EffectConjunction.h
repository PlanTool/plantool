/*
 *  EffectConjunction.h
 *  
 *
 *  Created by Owen Thomas on 18/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef effect_conjunction
#define effect_conjunction

class DOMPredicateEffect;
class DOMFunctionEffect;
class EffectConjunction 
{
	public:
		
		virtual ~EffectConjunction () { }
		/**
		 * If this node already exists, this DOMDelayedEffect
		 * is unchanged.
		 */

		virtual void addChild (DOMPredicateEffect* effect) = 0;

/**
		 * If a child of this DOMDelayedEffect, the atomic effect is
		 * removed.
		 */

		virtual void removeChild (DOMPredicateEffect* effect) = 0;
		
		
		virtual void addChild (DOMFunctionEffect* effect) = 0;
		virtual void removeChild (DOMFunctionEffect* effect) = 0;
};

#endif
