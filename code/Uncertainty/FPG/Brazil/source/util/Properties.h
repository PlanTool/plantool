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
 *  Properties.h
 *  
 *
 *  Created by Owen Thomas on 16/03/06.
 *  
 *
 */

#include <xercesc/dom/DOM.hpp>
class Properties
{
	private:
		DOMElement* dom;
		DOMDocument* doc;
		XMLCh* propertyString;
		XMLCh* nameString;
		XMLCh* valueString;

		DOMElement* findProperty (char* name)
		{
			XMLCh* propName = XMLString::transcode (name);			
			DOMNodeList* children = dom->getElementsByTagName (propertyString);
			
			for(int i = 0; i < children->getLength(); i++)
			{
				DOMElement* current = (DOMElement*) children->item(i);
				
				if (XMLString::compareIString (propName, current->getAttribute (nameString)) ==0)
				{
					return current;
				}
			}
			
			return NULL;
		}
		
	public:
		Properties (DOMElement* dom, DOMDocument* doc)
		{
			this->dom = dom;
			this->doc = doc;
			propertyString = XMLString::transcode ("property");
			nameString = XMLString::transcode("name");
			valueString = XMLString::transcode("value");
		}
		
		~Properties ()
		{
			XMLString::release(&propertyString);
			XMLString::release(&valueString);
			XMLString::release(&nameString);
		}
		
		char* getProperty(char* name)
		{
			DOMElement* entry = findProperty (name);
			
			if(NULL != entry)
			{
				return XMLString::transcode(entry->getAttribute(valueString));
			}
			else
			{
				return "";
			}
		}
		
		void setProperty (char* name, char* value)
		{
			DOMElement* entry = findProperty (name);
			
			if(NULL == entry)
			{
				entry = doc->createElement (propertyString);
				dom->appendChild(entry);
				
				XMLCh* newName = XMLString::transcode (name);
				entry->setAttribute (nameString, newName);
				XMLString::release(&newName);
			}
			
			XMLCh* newValue = XMLString::transcode (value);
			entry->setAttribute (valueString, newValue);
			XMLString::release(&newValue);
		}
};
