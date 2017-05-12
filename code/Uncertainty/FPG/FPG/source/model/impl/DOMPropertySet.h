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
