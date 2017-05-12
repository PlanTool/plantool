/*
 *  DOMAtomicEffectVisitor.h
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
#ifndef _DOMAtomicEffectVisitor__h
#define _DOMAtomicEffectVisitor__h

class DOMFunctionEffect;
class DOMPredicateEffect;

class DOMAtomicEffectVisitor {

	public:
		virtual ~DOMAtomicEffectVisitor () { }
		/**
                 * [daa] code for how to handle a functional effect
		 * Return value allows code that calls this to
		 * determine if the effect really does something.
		 * It doesn't have to be used or generated, but is used
		 * in the simulator 
		 */
		virtual bool visitFunctionEffect (DOMFunctionEffect*) = 0;

		/**
                 * [daa] code for how to handle a predicate effect
		 * Return value allows code that calls this to
		 * determine if the effect really does something.
		 * It doesn't have to be used or generated, but is used
		 * in the simulator 
		 */
		virtual bool visitPredicateEffect (DOMPredicateEffect*) = 0;
};

#endif
