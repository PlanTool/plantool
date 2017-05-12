/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  DOMAtomicEffectVisitor.h
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  
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
