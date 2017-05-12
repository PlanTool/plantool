/*
 *  DOMPredicate.cpp
 *  
 *
 *  Created by Owen Thomas on 21/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMPredicate.h"
#include "DOMDomain.h"

DOMPredicate::DOMPredicate (const char* name, DOMDomain* domain) {
	DOMDocument* doc = domain->getDOMElement ()->getOwnerDocument ();
	XMLCh* predicate = XMLString::transcode ("predicate");
	this->node = doc->createElement (predicate);
	setAttribute ("name",name);
	
	domain->addPredicate (this);
	
}


