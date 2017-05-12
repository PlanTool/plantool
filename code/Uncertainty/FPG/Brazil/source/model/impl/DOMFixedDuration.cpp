/*
 *  DOMFixedDuration.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMFixedDuration.h"

DOMFixedDuration::DOMFixedDuration (time_t duration, DOMDocument *document)
{
	XMLCh* delay = XMLString::transcode ("delay");
	this->node = document->createElement (XMLString::transcode ("delay"));
	
	XMLString::release (&delay);
	
	setAttribute ("type", "fixed");
	
	setDuration (duration);
}

