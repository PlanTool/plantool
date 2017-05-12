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
 *  XMLBrazilConfiguration.h
 *  
 *
 *  Created by Owen Thomas on 11/10/06.
 *  
 *
 */

#include "BrazilConfiguration.h"

#include <xercesc/dom/DOM.hpp>

#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include <xercesc/framework/LocalFileFormatTarget.hpp>

XERCES_CPP_NAMESPACE_USE
class XMLBrazilConfiguration : public BrazilConfiguration {

	private:
		XMLCh* dirString;
		XMLCh* valueString;

		DOMElement* config;

		QString helper (XMLCh* name);
		
		void helperSave (XMLCh* name, QString value);
		
	public:

		XMLBrazilConfiguration (DOMElement& config);
		XMLBrazilConfiguration ();
		
		
		virtual ~XMLBrazilConfiguration () {
			XMLString::release (&dirString);
		}
		
		virtual QString getDefaultDir ();
		
		virtual void setDefaultDir (QString& directory);
		
		virtual void save (QString& file);
};

