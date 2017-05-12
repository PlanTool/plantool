/*
 *  DOMAtomicEffectFactory.cpp
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMAtomicEffectFactory.h"

#include "DOMPredicateEffect.h"
#include "DOMFunctionEffect.h"
DOMAtomicEffect* DOMAtomicEffectFactory::create 
	(DOMElement& element, DOMDomain& domain, DOMEffectSet& parent) {
	
	if(DOMPredicateEffect::canRead (&element)) {
		return new DOMPredicateEffect (element, domain);
	}
	else if (DOMFunctionEffect::canRead (&element)) {
		return new DOMFunctionEffect (element, domain);
	}
	return NULL;
}

bool DOMAtomicEffectFactory::canRead (DOMElement& element) {

	return DOMPredicateEffect::canRead (&element) || DOMFunctionEffect::canRead (&element);
}

