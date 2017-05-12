/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  DOMBrazilObject.h
 *  
 *
 *  Created by Owen Thomas on 24/05/06.
 *  
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
