/*
 *  DOMPredicateEffect.h
 *  
 *
 *  Created by Owen Thomas on 6/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
