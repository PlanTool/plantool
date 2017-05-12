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
 *  DOMPredicateEffect.h
 *  
 *
 *  Created by Owen Thomas on 6/07/06.
 *  
 *
 */
#ifndef dom_predicate_effect
#define dom_predicate_effect

#include "DOMWrapper.h"
#include "DOMPredicate.h"
#include "DOMAtomicEffect.h"

#include <xercesc/dom/DOM.hpp>

class DOMDelayedEffect;
class DOMDomain;

class DOMPredicateEffect : public DOMAtomicEffect
{
	private:
		DOMPredicate* predicate;
		bool negated;
		
	public:
		DOMPredicateEffect (DOMPredicate& predicate,
			DOMDocument& doc, bool negated);
		
		DOMPredicateEffect (DOMElement& element, 
			DOMDomain& domain);
		
		virtual ~DOMPredicateEffect () { }
		
		virtual bool visit (DOMAtomicEffectVisitor* visitor) {
			return visitor->visitPredicateEffect (this);
		}
		void setNegated (bool value);
		bool isNegated ();
		
		DOMPredicate* getPredicate ();
		void setPredicate (DOMPredicate* predicate);

		
		virtual char* getTypeName () {
			return "DOMPredicateEffect";
		}
		
		static bool canRead (DOMElement* element) {
			static XMLCh* nameString = XMLString::transcode ("name");
			static XMLCh* negatedString = XMLString::transcode ("negated");
			static XMLCh* falseString = XMLString::transcode ("false");
			static XMLCh* trueString = XMLString::transcode ("true");
			static XMLCh* oneString = XMLString::transcode ("1");
			static XMLCh* zeroString = XMLString::transcode ("0");
			
			return element->hasAttribute (nameString) && element->hasAttribute (negatedString)
				&& (	XMLString::equals(element->getAttribute (negatedString), falseString) 
						|| XMLString::equals(element->getAttribute (negatedString), trueString)
						|| XMLString::equals(element->getAttribute (negatedString), oneString)
						|| XMLString::equals(element->getAttribute (negatedString), zeroString) );
		}
};

#endif
