/*
 *  DOMEffectSet.h
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_effect_set
#define dom_effect_set

using namespace std;

#include <list>
#include <xercesc/dom/DOM.hpp>
#include "DOMWrapper.h"

class DOMDuration;
class DOMAtomicEffect;
class EffectListener;
class DOMDomain;

/** 
 * A collection of DOM Atomic Effects.
 *
 * This collection may potentially be empty.
 * This collection may have a delay associated with it.
 */
class DOMEffectSet : public DOMWrapper {
	private:
		DOMDuration* delay;
		list<DOMAtomicEffect*> children;
		list<EffectListener*> listeners;
		DOMDomain* domain;
			public:


		/**
		 * Shortcut types for doug
		 * @author doug
		 */
		typedef list<DOMAtomicEffect*> AtomicEffects;
		typedef AtomicEffects::iterator AtomicEffectsIt;
		typedef AtomicEffects::const_iterator AtomicEffectsCIt;

		/**
		 * Construct the effect set from the specified element
		 * and drawing it's predicate and function definitions 
		 * from domain.
		 *
		 * domain's dom element must be an ancestor of element.
		 *
		 */
		DOMEffectSet (DOMElement& element, DOMDomain& domain);
		
		//DOMEffectSet (DOMDomain& domain, DOMDuration* duration = NULL);
		
		/**
		 * Remove any listeners and delete itself and all children,
		 * releasing the associated dom node.
		 */
		virtual ~DOMEffectSet ();
		
		/**
		 * Return the number of atomic effects.
		 */
		virtual int getNumAtomicEffects ();
		
		virtual list <DOMAtomicEffect*>& getAtomicEffects ();
		
		/**
		 * Equivalenet to calling getDelay () != NULL.
		 */
		virtual bool isDelayed ();
		
		/**
		 * Returns the delay (possibly NULL) associated with this
		 * effect.
		 */
		virtual DOMDuration* getDelay ();
		
		/**
		 * Sets the delay to delay, if a delay is already
		 * specified the current delay is deleted.
		 */
		virtual void setDelay (DOMDuration* delay);
		
		virtual void addListener (EffectListener* listener);
		
		virtual void removeListener (EffectListener* listener);
		
		virtual void addAtomicEffect (DOMAtomicEffect*);
		
		virtual void removeAtomicEffect (DOMAtomicEffect*);
		
		virtual DOMDomain* getDomain() {
			return domain;
		}
		
		
		virtual char* getTypeName () {
		        // [daa] added strdup
		        return strdup("DOMEffectSet");
		}
};

#endif
