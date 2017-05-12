/*
 *  DOMOutcome.cpp
 *  
 *
 *  Created by Owen Thomas on 6/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMOutcome.h"
#include "DOMAction.h"
DOMOutcome::DOMOutcome (DOMElement *node, DOMProbabilistic* parent)
{
	XMLCh* delayedEffect = XMLString::transcode ("effect");
	XMLCh* atStartEffect = XMLString::transcode ("at-start");
	
	this->node = node;
	this->parent = parent;
	
	this->listener = NULL;
	this->name = strdup("DOMOutcome");

	
	
	DOMNodeList* nodeList = node->getElementsByTagName (delayedEffect);
	for(unsigned int i = 0; i < nodeList->getLength (); i++) {
		effects.push_back (new DOMEffectSet (*(DOMElement*) nodeList->item(i),*parent->getParent()->getDomain()));
	}

	
	nodeList = node->getElementsByTagName (atStartEffect);
	if (nodeList->getLength() > 0) {
	    atStartEffects = new DOMEffectSet (*(DOMElement*) nodeList->item(0),*parent->getParent()->getDomain());
	} 
	else {
	    // No at-start effects. Let's create an empty one
	    DOMDocument* doc = node->getOwnerDocument();	    	
	    DOMElement* atStartElement = doc->createElement (atStartEffect);
	    node->appendChild(atStartElement);
	    atStartEffects = new DOMEffectSet (*atStartElement, *parent->getParent()->getDomain()); 
	}
	
	XMLString::release (&delayedEffect);
	XMLString::release (&atStartEffect);
}


DOMOutcome::DOMOutcome (char* label, DOMProbabilistic* parent, DOMDocument* doc) 
{
	XMLCh* outcomeString = XMLString::transcode ("outcome");
	XMLCh* atStartEffect = XMLString::transcode ("at-start");

	this->node = doc->createElement(outcomeString);
	this->parent = parent;
	this->name = "DOMOutcome";
	setAttribute ("label", label); 
	
	// No at-start effects. Let's create an empty one
	DOMElement* atStartElement = doc->createElement (atStartEffect);
	node->appendChild(atStartElement);
	atStartEffects = new DOMEffectSet (*atStartElement, *parent->getParent()->getDomain()); 
	
	XMLString::release(&outcomeString);
	XMLString::release(&atStartEffect);
}


DOMOutcome::~DOMOutcome()
{
	//TO-DO: Iterate through and delete all effect children.
	if(this->node->getParentNode() == NULL)
	{
		this->node->release();
	}
}


void DOMOutcome::setProbabilistic (DOMProbabilistic* parent) {
	this->parent = parent;
}


