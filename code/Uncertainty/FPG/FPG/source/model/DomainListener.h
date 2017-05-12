/*
 *  DomainListener.h
 *  
 *
 *  Created by Owen Thomas on 9/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef domain_listener
#define domain_listener


class DOMPredicate; 
class DOMAction;
class DOMFunction;

class DomainListener
{
	public:
		virtual ~DomainListener () { }
		virtual void predicateAdded (DOMPredicate*) = 0;
		virtual void functionAdded (DOMFunction*) = 0;
		virtual void actionAdded (DOMAction*) = 0;
		virtual void actionRemoved (DOMAction*) = 0;
		virtual void nameChanged (const char* name) = 0;
};

#endif
