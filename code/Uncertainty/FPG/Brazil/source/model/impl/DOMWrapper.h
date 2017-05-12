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

#ifndef dom_wrapper
#define dom_wrapper

#include <xercesc/dom/DOM.hpp>

#include <sstream>
#include <iostream>
#include <stdexcept>

using namespace std;

XERCES_CPP_NAMESPACE_USE

struct cmp_str {
	bool operator() (char const *a, char const *b) const {
		return strcmp (a,b) < 0;
	}
};
/*
 *  DOMWrapper.h
 *  
 *  Base class for all DOM Model classes. It provides convience
 *  functions that they can call to manipulate information their 
 *  DOMElement. It also maintains the pointer to the DOMElement 
 *  they manipulate.
 *  
 *  Created by Owen Thomas on 2/03/06.
 *  
 *
 */
class DOMWrapper 
{
	
	protected:
	
		//This is the underlying DOMElement being manipulated.
		DOMElement *node;
		
		time_t getAttributeAsTime (const char* attributeName);
		
		double getAttributeAsDouble (const char* attributeName);
		
		bool getAttributeAsBool (const char* attributeName);
		
		char* getAttribute (const char* attributeName);
		
		char* getTagName ();
		
		
		//QColor getAttributeAsQColor (const char* attributeName);
		
		
		//void setAttribute (const char* name, QColor value);
		
		void setAttribute (const char* name, const char* value);
		
		/**
		 * @throw an invalid_argument if the double value cannot
		 * be encoded into a DOM attribute value.
		 */
		void setAttribute (const char* name, double value);
		
		void setAttribute (const char* name, char* value);
		
		void setAttribute (const char* name, time_t value);
		
		void setAttribute (const char* name, bool value);
		
		//QString toQString (const XMLCh* string);
		
	public:
		
		DOMElement* getDOMElement()
		{
			return this->node;
		}
		
		virtual ~DOMWrapper () { }
		virtual char* getTypeName ()
		{
			return "DOMWrapper";
		}
};
#endif
