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
 * $Id$
 *
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 *
 */

#ifndef dom_property
#define dom_property

#include <xercesc/dom/DOM.hpp>
#include "DOMWrapper.h"



class DOMProperty : public DOMWrapper
{

 private:

    XMLCh* propertyTag;
    XMLCh* propertyNameTag;
    XMLCh* propertyValueTag;

    DOMElement* propertyNameElement;
    DOMElement* propertyValueElement;

    void createStrings();
    void freeStrings();

 public:
	  
    DOMProperty (DOMDocument* doc, XMLCh* name, XMLCh* val);	
    DOMProperty (DOMElement& element);
    ~DOMProperty();

    const XMLCh* getPropertyName();
    void setPropertyValue (XMLCh* newValue);
    const XMLCh* getPropertyValue();
		
    virtual char* getTypeName ();

    
		
};
#endif
