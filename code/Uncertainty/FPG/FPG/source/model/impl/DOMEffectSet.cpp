/*
 *  DOMEffectSet.cpp
 *  
 *
 *  Created by Owen Thomas on 25/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMEffectSet.h"

#include "DOMDuration.h"
#include "DOMAtomicEffect.h"
#include "DOMAtomicEffectFactory.h"
#include "../EffectListener.h"
#include <xercesc/dom/DOM.hpp>
DOMEffectSet::DOMEffectSet (DOMElement& element, DOMDomain& domain) {
	this->domain = &domain;
	this->node = &element;
	this->delay = NULL;
	DOMNodeList* childNodes = element.getChildNodes ();

	//Loop through and process children.
	for(unsigned int i = 0; i < childNodes->getLength(); i++) {
		if(childNodes->item(i)->getNodeType () == DOMNode::ELEMENT_NODE) {
			DOMElement* nextElement = (DOMElement*) childNodes->item (i);
			if(DOMDuration::canRead (nextElement)) {
				if(this->delay) delete this->delay;
				this->delay = DOMDuration::create (nextElement);
			}
			else if (DOMAtomicEffectFactory::canRead (*nextElement)) {
				children.push_back (DOMAtomicEffectFactory::create 
					(*nextElement,domain, *this));
			}
		}
	}
}

DOMEffectSet::~DOMEffectSet () {
	if(this->delay) delete this->delay;
	
	list<DOMAtomicEffect*>::iterator it;
	for(it = children.begin(); it != children.end(); it++) {
		delete *it;
	}
	
	if(this->node->getParentNode () == NULL) {
		this->node->release ();
	}
}

int DOMEffectSet::getNumAtomicEffects () {
	return children.size ();
}

bool DOMEffectSet::isDelayed () {
	return delay != NULL;
}

DOMDuration* DOMEffectSet::getDelay () {
	return delay;
}

void DOMEffectSet::addAtomicEffect (DOMAtomicEffect* effect) {
	unsigned int sizeBefore = children.size();
	children.push_back (effect);
	if(sizeBefore != children.size()) {
		this->node->appendChild (effect->getDOMElement());
		
		list<EffectListener*>::iterator it;
		for(it = listeners.begin(); it != listeners.end(); it++) {
			(*it)->effectAdded (effect);
		}
	}
}

void DOMEffectSet::removeAtomicEffect (DOMAtomicEffect* effect) {
	unsigned int sizeBefore = children.size ();
	children.remove (effect);
	if(sizeBefore != children.size ()) {
		this->node->removeChild (effect->getDOMElement ());
		
		list<EffectListener*>::iterator it;
		for(it = listeners.begin(); it != listeners.end(); it++) {
			(*it)->effectRemoved (effect);
		}
	}
}

void DOMEffectSet::setDelay (DOMDuration* delay) {
	if(this->delay) {
		this->node->removeChild (this->delay->getDOMElement ());
		
		delete this->delay;
	}
	this->delay = delay;
	this->node->appendChild (delay->getDOMElement ());
	list<EffectListener*>::iterator it;
	for(it = listeners.begin(); it != listeners.end(); it++) {
		(*it)->durationChanged ();
	}
}

void DOMEffectSet::addListener (EffectListener* listener) {
	listeners.push_back (listener);
}

void DOMEffectSet::removeListener (EffectListener* listener) {
	listeners.remove (listener);
}

list<DOMAtomicEffect*>& DOMEffectSet::getAtomicEffects () {
	return children;
}
