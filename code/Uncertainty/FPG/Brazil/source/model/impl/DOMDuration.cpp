/*
 *  DOMDuration.cpp
 *  
 *
 *  Created by Owen Thomas on 9/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMDuration.h"
#include "DOMFixedDuration.h"
#include "DOMUniformDuration.h"
#include "DOMExponentialDuration.h"
#include "DOMNormalDuration.h"
DOMDuration* DOMDuration::create (DOMElement* element) {
	
	DOMDuration* duration = NULL;
	
	XMLCh* typeString = XMLString::transcode ("type");
	char *type = XMLString::transcode (element->getAttribute (typeString));
	
	XMLString::release (&typeString);
	
	if(0 == strcmp("fixed",type))
	{
		duration =  new DOMFixedDuration (element);
	}
	
	else if(0 == strcmp("uniform",type))
	{
		duration = new DOMUniformDuration (element);
	}
	
	else if(0 == strcmp("exponential",type))
	{
		duration = new DOMExponentialDuration (element);
	}
	
	else if(0 == strcmp("normal",type))
	{
		duration = new DOMNormalDuration (element);
	}
	
	else
	{
		cout << type << endl;
		throw std::invalid_argument ("unknown type string");
	}
	
	XMLString::release (&type);
	return duration;
}

bool DOMDuration::canRead (DOMElement* element) {
	static XMLCh* xmlType = XMLString::transcode ("type");
	char *type = XMLString::transcode (element->getAttribute (xmlType));

	return element->hasAttribute (xmlType) &&
		((0 == strcmp("fixed",type)) 
		|| ( 0 == strcmp("uniform",type)) 
		|| (0 == strcmp("exponential",type))
		|| (0 == strcmp("normal",type)) );
}
