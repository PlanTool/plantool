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

#ifndef dom_property_set
#define dom_property_set


#include <list>
#include <xercesc/dom/DOM.hpp>


#include "DOMWrapper.h"
#include "DOMProperty.h"



/** 
 * A collection of DOM properties for use by any part of the Brazil planner, or any other planner.
 * Essentially a way to embed planner specific information in the XML file without having to change
 * the schema.
 *
 * This collection may potentially be empty.
 */
class DOMPropertySet : public DOMWrapper {

 public:
    /**
     * Public shortcut types
     */
    typedef list<DOMProperty*> Properties;
    typedef Properties::iterator PropertiesIt;
    typedef Properties::const_iterator PropertiesCIt;

 private:

    static DOMDocumentFragment* df;
    DOMPropertySet* defaultProps; // Optional alternate set of properties.
    
    Properties children;
    XMLCh* typeName;

 public:


    DOMPropertySet(DOMElement* element);
    DOMPropertySet(DOMDocument* doc);

    /**
     * Remove any listeners and delete itself and all children,
     * releasing the associated dom node.
     */
    virtual ~DOMPropertySet ();
		
    virtual Properties& getProperties();

    virtual void addProperty(DOMProperty*);
    virtual void addProperty(char* name, char* val);
    virtual void addProperty(char* name, int val);
    virtual void addProperty(char* name, double val);

    virtual DOMProperty* findProperty(char* name);

    virtual char*  getPropertyString(char* name);
    virtual int    getPropertyInt(char* name);
    virtual double getPropertyDouble(char* name);

    virtual void removeProperty(DOMProperty*);
		

    void setDefaultPropertySet(DOMPropertySet* defaults);

    virtual char* getTypeName() { return XMLString::transcode(typeName); }


    /** 
     * Nested class to provide a property not found exception
     */
    class PropertyException : public std::exception {
    private:
	char* propName;
    public:
	PropertyException(char* propName) { this->propName = propName; }
	virtual const char* what() const throw() { return propName; }
    };

    static DOMPropertySet* makeDefaults(DOMDocument* doc);

};

#endif
