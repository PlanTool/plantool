/*
 *  DOMResource.h
 *  
 *
 *  Created by Owen Thomas on 24/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_resource
#define dom_resource

#include "DOMBrazilObject.h"
class DOMResource : public DOMBrazilObject
{
	public:
		DOMResource (DOMElement* node)
		{
			this->node = node;
		}
		
		DOMResource (const char* name, DOMDocument* doc)
		{
			XMLCh* predicate = XMLString::transcode ("resource");
			this->node = doc->createElement (predicate);
			
			setName (name);
		}
		
		virtual ~DOMResource () { }
		
		virtual bool isResource() { return true; }
				
};
#endif
