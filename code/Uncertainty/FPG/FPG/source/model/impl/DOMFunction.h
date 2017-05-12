/*
 *  DOMFunction.h
 *  
 *
 *  Created by Owen Thomas on 14/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_function
#define dom_function

#include "DOMWrapper.h"

class DOMFunction : public DOMWrapper
{
	
	public:
		DOMFunction (DOMElement* node)
		{
			this->node = node;
		}
		
		DOMFunction (const char* name,DOMDocument* doc)
		{
			XMLCh* function = XMLString::transcode ("function");
			this->node = doc->createElement (function);
			
			setAttribute ("name",name);
		}
		
		virtual ~DOMFunction () {}
		
		/**
		 * Return the name of this function.
		 */
		char* getName () {
			return getAttribute  ("name");
		}
};

#endif
