/*
 *  DOMAtoimicEffectFactory.h
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_atomic_effect_factory
#define dom_atomic_effect_factory

#include "DOMDomain.h"
#include "DOMAtomicEffect.h"
#include "DOMEffectSet.h"

#include <xercesc/dom/DOM.hpp>

class DOMAtomicEffectFactory {

	public:
		
		static DOMAtomicEffect* create 
			(DOMElement& element, DOMDomain& domain, DOMEffectSet& parent);
		
		static bool canRead (DOMElement& element);
};

#endif
