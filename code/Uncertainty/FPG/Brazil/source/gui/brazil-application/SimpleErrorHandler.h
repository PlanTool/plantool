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
 *  
 *
 *  Created by Owen Thomas on 30/08/06.
 *  
 *
 */
#include <list>
#include <iostream>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/sax/ErrorHandler.hpp>
#include <xercesc/util/PlatformUtils.hpp>
XERCES_CPP_NAMESPACE_BEGIN
using namespace std;
class SimpleErrorHandler : public ErrorHandler {
	private:
		list<char*> errorMessages;
		
	public:
		SimpleErrorHandler () {
		}
		virtual void error (const SAXParseException& exc) {
			char* errorMessage = XMLString::transcode (exc.getMessage ());
			errorMessages.push_back (errorMessage);
		}
		
		virtual void fatalError (const SAXParseException& exc) {
			char* errorMessage = XMLString::transcode (exc.getMessage ());
			errorMessages.push_back (errorMessage);		
		}
		
		virtual void warning (const SAXParseException& exc) { 
			cout << XMLString::transcode (exc.getMessage()) << endl;
		}

		virtual void resetErrors () {
			list<char*>::iterator it;
			for(it = errorMessages.begin (); it != errorMessages.end(); it++) {
				char* message = *it;
				errorMessages.erase (it);
				XMLString::release (&message);
			}
		}
		virtual int getErrorCount () {
			return errorMessages.size ();
		}
		
		virtual list<char*> getMessages () {
			return errorMessages;
		}
};

XERCES_CPP_NAMESPACE_END

