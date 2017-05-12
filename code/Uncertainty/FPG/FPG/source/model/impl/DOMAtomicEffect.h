/*
 *  DOMAtoimicEffect.h
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_atomic_effect
#define dom_atomic_effect
#include "DOMAtomicEffectVisitor.h"
#include "DOMWrapper.h"

class DOMEffectSet;
class DOMAtomicEffect : public DOMWrapper {

		
	protected:
		DOMAtomicEffect () {}
	public:
		
		virtual ~DOMAtomicEffect () { }
		virtual bool visit (DOMAtomicEffectVisitor* visitor) = 0;
};

#endif
