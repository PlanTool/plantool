/*
 *  DOMExponentialDuration.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMExponentialDuration.h"

DOMExponentialDuration::DOMExponentialDuration (double lambda, DOMDocument *doc)
{
	XMLCh* delay = XMLString::transcode ("delay");
	this->node = doc->createElement(delay);
	XMLString::release (&delay);

	setAttribute ("type", "exponential");
	setLambda (lambda);
}

