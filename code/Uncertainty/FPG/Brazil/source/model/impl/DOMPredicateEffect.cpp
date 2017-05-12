/*
 *  DOMPredicateEffect.cpp
 *  
 *
 *  Created by Owen Thomas on 6/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMPredicateEffect.h"
#include "DOMDomain.h"

DOMPredicateEffect::DOMPredicateEffect (DOMPredicate& predicate,
	DOMDocument& doc, bool negated) : DOMAtomicEffect ()
{
	this->predicate = &predicate;
	
	XMLCh* predicateName = XMLString::transcode ("predicate");
	
	this->node = doc.createElement (predicateName);
	setAttribute ("name", predicate.getName());
	setAttribute ("negated", negated);
}

DOMPredicateEffect::DOMPredicateEffect (DOMElement& element, 
	DOMDomain& domain){
	this->node = &element;
	char* name = getAttribute ("name");
	this->predicate = domain.getPredicate (name);
}

void DOMPredicateEffect::setNegated (bool value) {
    cout<<"Setting predicate '"<<predicate->getName()<<"' to "<<value<<endl;
	setAttribute ("negated", value);
	cout<<"New attribute value is "<<getAttributeAsBool ("negated")<<endl;
}

bool DOMPredicateEffect::isNegated () {
	return getAttributeAsBool ("negated");
}

DOMPredicate* DOMPredicateEffect::getPredicate () {
	return predicate;
}

void DOMPredicateEffect::setPredicate (DOMPredicate* predicate) {
	this->predicate = predicate;
	setAttribute ("name", predicate->getName());
}





