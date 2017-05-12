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
