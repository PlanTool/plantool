/*
 *  DOMUniformDuration.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMUniformDuration.h"


DOMUniformDuration::DOMUniformDuration (time_t minDuration, time_t maxDuration, DOMDocument *doc)
{
	XMLCh* delay = XMLString::transcode ("delay");
	this->node = doc->createElement (delay);
	XMLString::release (&delay);
	setAttribute ("type","uniform");
			
	setMaxDuration (maxDuration);
	setMinDuration (minDuration);
}

void DOMUniformDuration::setMaxDuration (time_t duration)
{
	XMLCh* min = XMLString::transcode("min");
	
	if(duration < 0) throw invalid_argument ("duration < 0");
	if(this->node->hasAttribute (min))
	{
		if(duration < getMinDuration())
		{
			throw invalid_argument ("Max Duration < Min Duration");
		}
	}
	XMLString::release (&min);
	setAttribute ("max", duration);
}

void DOMUniformDuration::setMinDuration (time_t duration)
{
	XMLCh* max = XMLString::transcode("max");
	
	if(duration < 0) throw invalid_argument ("duration < 0");
	if(this->node->hasAttribute (XMLString::transcode ("max")))
	{
		if(duration > getMaxDuration())
		{
			throw invalid_argument ("Max Duration < Min Duration");
		}
	}
	
	
	XMLString::release (&max);
	setAttribute ("min", duration);
}

