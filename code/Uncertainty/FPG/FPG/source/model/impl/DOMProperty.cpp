
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


#include "DOMProperty.h"


/**
 * Create a single property node. There are sub elements to have a
 * "strict" XML document, but since the Brazil planner can query this
 * class directly for property names and values, and since there are
 * exactly one name and one value per property there are no additional
 * DOM classes for these.
 */
DOMProperty::DOMProperty (DOMDocument* doc, XMLCh* name, XMLCh* val) {

    createStrings();

    this->node = doc->createElement(propertyTag);

    // Add name element
    propertyNameElement = doc->createElement(propertyNameTag);
    propertyNameElement->appendChild(doc->createTextNode(name));
    this->node->appendChild(propertyNameElement);

    // Add value element
    propertyValueElement = doc->createElement(propertyValueTag);
    propertyValueElement->appendChild(doc->createTextNode(val));
    this->node->appendChild(propertyValueElement);

}


/**
 * I guess this is called when loading DOM structure from an XML file 
 */
DOMProperty::DOMProperty (DOMElement& element) {

    
    createStrings();
    this->node = &element;

    DOMNodeList* propList = node->getElementsByTagName (propertyNameTag);
    propertyNameElement = (DOMElement*)propList->item(0);

    propList = node->getElementsByTagName (propertyValueTag);
    propertyValueElement = (DOMElement*)propList->item(0);

    
    //cout<<"DOMProperty(): "<<XMLString::transcode(node->getTextContent())<<endl;
}


DOMProperty::~DOMProperty() {

    node->removeChild(propertyNameElement);
    propertyNameElement->release();

    node->removeChild(propertyValueElement);
    propertyValueElement->release();

    if (node->getParentNode() == NULL) node->release();
    
    freeStrings();

}


void DOMProperty::createStrings() {

    propertyTag = XMLString::transcode ("property");
    propertyNameTag = XMLString::transcode ("name");
    propertyValueTag = XMLString::transcode ("value");
}


void DOMProperty::freeStrings() {

    XMLString::release(&propertyTag);
    XMLString::release(&propertyNameTag);
    XMLString::release(&propertyValueTag);
}


/** 
 * Return the name of this property
 */
const XMLCh* DOMProperty::getPropertyName() {
    return propertyNameElement->getFirstChild()->getNodeValue();
}


void DOMProperty::setPropertyValue(XMLCh* newValue) {
    propertyValueElement->getFirstChild()->setNodeValue(newValue);
}


/**
 * Return XMLCh version of whatever the value is. 
 * Conversions done in PropertySet.
 */
const XMLCh* DOMProperty::getPropertyValue() {
    return propertyValueElement->getFirstChild()->getNodeValue();
}


char* DOMProperty::getTypeName() {
    return XMLString::transcode(propertyTag); 
}
