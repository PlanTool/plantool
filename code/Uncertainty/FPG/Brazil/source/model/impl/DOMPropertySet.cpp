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

#include <xercesc/dom/DOM.hpp>
#include "DOMProperty.h"
#include "DOMPropertySet.h"

#define PROPERTY_NUMBER_LEN 32

/**
 * parse DOM tree to extract properties.
 */
DOMPropertySet::DOMPropertySet(DOMElement* element) {

    this->node = element;

    DOMNodeList* childNodes = element->getChildNodes ();

    //Loop through and process children.
    for (unsigned int i = 0; i < childNodes->getLength(); i++) {
	if(childNodes->item(i)->getNodeType () == DOMNode::ELEMENT_NODE) {
	    DOMElement* nextElement = (DOMElement*) childNodes->item (i);
	    children.push_back (new DOMProperty(*nextElement));
	}
    }

    typeName = XMLString::transcode("properties");
    defaultProps = NULL;
    df = NULL;
}


/**
 * Create empty property set
 */
DOMPropertySet::DOMPropertySet(DOMDocument* doc) {

    typeName = XMLString::transcode("properties");
    this->node = doc->createElement(typeName);
    defaultProps = NULL;


}


DOMPropertySet::~DOMPropertySet () {

    PropertiesIt it;
    for(it = children.begin(); it != children.end(); it++) {
	node->removeChild((*it)->getDOMElement());
	delete *it;
    }
	

    if (node->getParentNode () == NULL) node->release ();

    if (defaultProps != NULL) delete defaultProps;

    XMLString::release(&typeName);

}


/**
 * Add a new property to the list. Duplicates not allowed.
 */
void DOMPropertySet::addProperty (DOMProperty* p) {
    
    unsigned int sizeBefore = children.size();
    children.push_back (p);
    // A simple way to find out if the item exists in the DOM tree
    if (sizeBefore != children.size()) {
	node->appendChild(p->getDOMElement());

    }
}


/**
 * Create a new property from name and string value
 */
void DOMPropertySet::addProperty(char* name, char* val) {

    XMLCh* xName;
    XMLCh* xVal;

    xName = 	XMLString::transcode(name);
    xVal = 	XMLString::transcode(val);
    
    addProperty(new DOMProperty(node->getOwnerDocument(), xName, xVal));

    delete[] xName;
    delete[] xVal;

}


/**
 * Create a new property from name and integer value
 */
void DOMPropertySet::addProperty(char* name, int val) {
    
    char tmpVal[PROPERTY_NUMBER_LEN];
    snprintf(tmpVal, PROPERTY_NUMBER_LEN, "%d", val);
    addProperty(name, tmpVal);

}


/**
 * Create a new property from name and double value
 */
void DOMPropertySet::addProperty(char* name, double val) {
    
    char tmpVal[PROPERTY_NUMBER_LEN];
    snprintf(tmpVal, PROPERTY_NUMBER_LEN, "%g", val);
    addProperty(name, tmpVal);

}


/**
 * Search for a property with a particular name. Will fall back to defaults if found.
 * @return NULL if no property with that name found.
 */
DOMProperty* DOMPropertySet::findProperty(char* name) {

    XMLCh* xmlName = XMLString::transcode(name);
    for (PropertiesIt it = children.begin(); it != children.end(); it++) {
	XMLCh* pName = XMLString::replicate((*it)->getPropertyName());
	XMLString::trim(pName);
	if (XMLString::compareIString(pName, xmlName) == 0) {
	    delete[] xmlName;
	    delete[] pName;
	    return *it;
	}
	delete[] pName;
    }
    
    delete[] xmlName;
    
    // Didn't find the property. Consult defaults if they exist
    if (defaultProps != NULL) return defaultProps->findProperty(name);
    return NULL;
}


/**
 * Retrieve the value of a property. Throws PropertyException  if not found 
 */
char* DOMPropertySet::getPropertyString(char* name) {

    DOMProperty* p = findProperty(name);
    if (p == NULL) throw PropertyException(name);
    return XMLString::transcode(p->getPropertyValue());

}


/**
 * Retrieve the value of a property as an int. Throws PropertyException  if not found 
 */
int DOMPropertySet::getPropertyInt(char* name) {

    DOMProperty* p = findProperty(name);
    if (p == NULL) throw PropertyException(name);
    return XMLString::parseInt(p->getPropertyValue());

}


/**
 * Retrieve the value of a property as a double. Throws PropertyException  if not found 
 */
double DOMPropertySet::getPropertyDouble(char* name) {

    DOMProperty* p = findProperty(name);
    if (p == NULL) throw PropertyException(name);
    return strtod(XMLString::transcode(p->getPropertyValue()), NULL);

}


/**
 * Extract the property. Does not delete?
 */
void DOMPropertySet::removeProperty (DOMProperty* p) {
    unsigned int sizeBefore = children.size ();
    children.remove (p);
    if(sizeBefore != children.size ()) {
	this->node->removeChild(p->getDOMElement());
    }
}


/**
 * Get list of properties in this XML document
 */
DOMPropertySet::Properties& DOMPropertySet::getProperties() {
    return children;
}


/**
 * Set a default properties class.
 * Defaults to NULL in constructor, which will mean no defaults.
 * If a property is not found, and the default properties class is not null, it will be queried for
 * the property.
 * Thus PropertySet classes can be chained together to create a hierarchy of fall back defaults.
 */
void DOMPropertySet::setDefaultPropertySet(DOMPropertySet* defaults) {
    defaultProps = defaults;
}




DOMDocumentFragment* DOMPropertySet::df;


/**
 * Request hard wired default values
 */
DOMPropertySet* DOMPropertySet::makeDefaults(DOMDocument* doc) {

    df = doc->createDocumentFragment();
    DOMPropertySet* d = new DOMPropertySet(df->getOwnerDocument());

#include"DefaultProperties.h"

    return d;

}
