/*
 *  DOMBrazilObject.h
 *  
 *
 *  Created by Owen Thomas on 24/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_brazil_object
#define dom_brazil_object

#include "DOMWrapper.h"

/**
 * An abstract base class for planning objects such as
 * resources (DOMResource) and predicates (DOMPredicate).
 */
class DOMBrazilObject : public DOMWrapper {

	public:
	
	virtual ~DOMBrazilObject ()
		{
			if(node->getParentNode() == NULL) node->release();
		}
		
		virtual char* getName()
		{
			return getAttribute ("name");
		}
			
		virtual void setName (const char* name)
		{
			setAttribute("name", name);
		}
		
		virtual bool isPredicate () { return false; }
		virtual bool isResource () { return false; }
};

#endif
