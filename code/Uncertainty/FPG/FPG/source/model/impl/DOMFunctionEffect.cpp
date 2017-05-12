/*
 *  DOMFunctionEffect.cpp
 *  
 *
 *  Created by Owen Thomas on 14/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMFunctionEffect.h"
#include "DOMDomain.h"
DOMFunctionEffect::DOMFunctionEffect (DOMElement& element, 
	 DOMDomain& domain) : DOMAtomicEffect (){
	
	this->node = &element;
	this->op = readOperator ();
	
	XMLCh* functionName = XMLString::transcode ("function");
	
	DOMNodeList* functionChildren = element.getElementsByTagName (functionName);
	
	if(functionChildren->getLength()) {
		this->functionElement = (DOMElement*)functionChildren->item (0);
		XMLCh* name = XMLString::transcode ("name");
		char* foundFunctionName = XMLString::transcode(functionElement->getAttribute(name));
		this->function = domain.getFunction (foundFunctionName);
		XMLString::release (&foundFunctionName);
		XMLString::release (&name);
	}

	XMLString::release (&functionName);
	
	DOMNodeList* children = element.getChildNodes ();
	for(unsigned int i = 0; i < children->getLength (); i++) {
		DOMNode* node = children->item (i);
		if(children->item (i) != functionElement 
			&& children->item (i)->getNodeType () == DOMNode::ELEMENT_NODE) {
			
			expression = DOMFunctionExpression::createExpression (*((DOMElement*)node), domain);
			if(expression) {
				break;
			}
		}
	}
}

//
//Construct a DOMFunctionEffect from a function, operator and expression.
//This creates a new DOMElement.
//

DOMFunctionEffect::DOMFunctionEffect (DOMFunction& function, Operator op, 
	DOMFunctionExpression& expression, DOMDocument& doc) {
	
	//We do not assume ownership of the function or its dom element.
	functionElement = (DOMElement*) function.getDOMElement ()->cloneNode (true);
	this->function = &function;
	
	this->node = doc.createElement (XMLString::transcode ("functionEffect"));
	
	this->node->appendChild (functionElement);
	
	//But we do of the expressoin.
	this->expression = &expression;
	
	if(expression.getDOMElement()->getParentNode () != NULL) 
		expression.getDOMElement()->getParentNode()->removeChild (expression.getDOMElement());
	
	this->node->appendChild (expression.getDOMElement ());
	setOperator (op);
}

DOMFunctionEffect::Operator DOMFunctionEffect::getOperator () {
	return op;
}

void DOMFunctionEffect::setOperator (Operator op ) {
	this->op = op;
	setAttribute ("type", writeOperator ());
}

DOMFunctionExpression* DOMFunctionEffect::getExpression () {
	return expression;
}
//
//Assume ownership of expression ...
//
void DOMFunctionEffect::setExpression (DOMFunctionExpression* expression) {
	
	//if the expression's dom element is not a child of this effect's node, then
	//make it so.
	
	 if(expression->getDOMElement()->getParentNode () != this->node) {
		if(expression->getDOMElement()->getParentNode () != NULL) {
			expression->getDOMElement()->getParentNode ()->removeChild (expression->getDOMElement());
		}
		if(this->expression->getDOMElement()->getParentNode () == this->node) {
			this->node->replaceChild (expression->getDOMElement(), this->expression->getDOMElement());
		}
		else {
			this->node->appendChild (expression->getDOMElement ());
		}
	}

	delete this->expression;
	this->expression = expression;
}

DOMFunction* DOMFunctionEffect::getFunction () {
	return function;
}

//
//Do not take ownership of function.
//
void DOMFunctionEffect::setFunction (DOMFunction* function) {
	//1. Create a clone of the function's dom element.
	DOMElement* clonedFunctionElement = (DOMElement*) function->getDOMElement()->cloneNode (true);
	
	//2. replace cloned with existing functionElement.
	this->node->replaceChild (clonedFunctionElement, functionElement);
	
	//3. remove from memory existing function element
	functionElement->release ();
	
	this->functionElement = clonedFunctionElement;
	this->function = function;
}

DOMFunctionEffect::Operator DOMFunctionEffect::readOperator () {

	char* operatorName = getAttribute ("type");

	DOMFunctionEffect::Operator returnOperator;
	
	if(XMLString::equals (operatorName, "assign")) {
		returnOperator = DOMFunctionEffect::assign;
	}
	else if (XMLString::equals (operatorName, "scaleUp")) {
		returnOperator = DOMFunctionEffect::scaleUp;
	}
	else if (XMLString::equals (operatorName, "scaleDown")) {
		returnOperator = DOMFunctionEffect::scaleDown;
	}
	else if (XMLString::equals (operatorName, "increase")) {
		returnOperator = DOMFunctionEffect::increase;
	}
	else if (XMLString::equals (operatorName, "decrease")) {
		returnOperator = DOMFunctionEffect::decrease;
	}	
	XMLString::release (&operatorName);
	
	return returnOperator;
}

char* DOMFunctionEffect::writeOperator () {
	if(op == DOMFunctionEffect::assign) return "assign";
	if(op == DOMFunctionEffect::scaleUp) return "scaleUp";
	if(op == DOMFunctionEffect::scaleDown) return "scaleDown";
	if(op == DOMFunctionEffect::increase) return "increase";
	return "decrease";
}

 bool DOMFunctionEffect::canRead (DOMElement* element) {
	static XMLCh* functionEffect = XMLString::transcode ("functionEffect");
	return XMLString::equals (functionEffect, element->getTagName());
}


