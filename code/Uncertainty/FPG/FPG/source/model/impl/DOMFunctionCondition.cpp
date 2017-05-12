/*
 *  DOMFunctionCondition.cpp
 *  
 *
 *  Created by Owen Thomas on 14/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMFunctionCondition.h"
#include "DOMDomain.h"
DOMFunctionCondition::DOMFunctionCondition (DOMElement& element, DOMDomain& domain) : DOMCondition (&element){
	this->op = readOperator ();
	this->function = NULL;
	this->expression = NULL;
	
	XMLCh* functionName = XMLString::transcode ("function");
	
	DOMNodeList* functionChildren = element.getElementsByTagName (functionName);
	
	if(functionChildren->getLength()) {
		functionElement = (DOMElement*)functionChildren->item (0);
		XMLCh* name  = XMLString::transcode ("name");
		char* foundFunctionName = XMLString::transcode(functionElement->getAttribute(name));
		this->function = domain.getFunction (foundFunctionName);
		XMLString::release (&foundFunctionName);
		XMLString::release (&name);
	}
	
	XMLString::release (&functionName);
	DOMNodeList* children = element.getChildNodes ();
	for(unsigned int i = 0; i < children->getLength (); i++) {
		DOMNode* node = children->item (i);
		if(node != functionElement 
			&& node->getNodeType () == DOMNode::ELEMENT_NODE) {
			
			expression = DOMFunctionExpression::createExpression (*(DOMElement*)node,domain);
			if(expression) {
				break;
			}
		}
	}	
}

DOMFunctionCondition::DOMFunctionCondition (DOMFunction* function,
	Operator op, DOMFunctionExpression* expression, DOMDocument* doc) :
	DOMCondition (doc->createElement (XMLString::transcode ("functionCondition"))){

	this->function = function;
	this->op = op;
	this->expression = expression;
	
	functionElement = (DOMElement*) function->getDOMElement()->cloneNode (true);
	this->node->appendChild(functionElement);
	
	this->node->appendChild(expression->getDOMElement ());
	setAttribute ("type", writeOperator ());
	
}

DOMFunctionCondition::Operator DOMFunctionCondition::getOperator () {
	return op;
}

void DOMFunctionCondition::setOperator (Operator op) {
	this->op = op;
	setAttribute ("type", writeOperator ());
}

DOMFunctionExpression* DOMFunctionCondition::getExpression () {
	return expression;
}

//
//Assume ownership of expression ...
//
void DOMFunctionCondition::setExpression (DOMFunctionExpression* expression) {
	node->replaceChild (expression->getDOMElement (), this->expression->getDOMElement ());
	this->expression = expression;
}

DOMFunction* DOMFunctionCondition::getFunction () {
	return function;
}

//
//Do not take ownership of function.
//
void DOMFunctionCondition::setFunction (DOMFunction* function) {
	DOMElement* newFunctionElement = (DOMElement*)function->getDOMElement ()->cloneNode (true);
	node->replaceChild (newFunctionElement, functionElement);

	functionElement->release();
	functionElement = newFunctionElement;
	this->function = function;
}

DOMFunctionCondition::Operator DOMFunctionCondition::readOperator () {

	char* operatorName = getAttribute ("type");

	DOMFunctionCondition::Operator returnOperator;
	
	if(XMLString::equals (operatorName, "gt")) {
		returnOperator = DOMFunctionCondition::greaterThan;
	}
	else if (XMLString::equals (operatorName, "lt")) {
		returnOperator = DOMFunctionCondition::lessThan;
	}
	else if (XMLString::equals (operatorName, "equals")) {
		returnOperator = DOMFunctionCondition::equals;
	}
	else if (XMLString::equals (operatorName, "le")) {
		returnOperator = DOMFunctionCondition::lessThanOrEqualTo;
	}
	else if (XMLString::equals (operatorName, "ge")) {
		returnOperator = DOMFunctionCondition::greaterThanOrEqualTo;
	}	
	XMLString::release (&operatorName);
	
	return returnOperator;
}

char* DOMFunctionCondition::writeOperator () {
	if(op == DOMFunctionCondition::greaterThan) return "gt";
	if(op == DOMFunctionCondition::lessThan) return "lt";
	if(op == DOMFunctionCondition::equals) return "equals";
	if(op == DOMFunctionCondition::lessThanOrEqualTo) return "le";
	return "ge";
}
