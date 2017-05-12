/*
 *  DOMProblem.cpp
 *  
 *
 *  Created by Owen Thomas on 18/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMProblem.h"

#include <stdexcept>
#include "../../planner/BrazilState.h"

DOMProblem::DOMProblem (DOMElement* element, DOMDomain& domain)
{
	this->node = element;
	this->domain = &domain;
	
	initString = XMLString::transcode ("init");
	predicate = XMLString::transcode ("predicate");
	predicates = XMLString::transcode ("predicates");
	function = XMLString::transcode ("function");
	functions = XMLString::transcode ("functions");
	value = XMLString::transcode ("value");
	name = XMLString::transcode ("name");
	goalString = XMLString::transcode ("goal");
	assignString = XMLString::transcode ("assign");
	number = XMLString::transcode ("number");
	
	createInit ();
	createGoal ();
	createInitialState ();
}

void DOMProblem::createInit ()
{
	DOMNodeList* initNodeList = this->node->getElementsByTagName (initString);
	
	initFunctionsElement = NULL;
	initPredicatesElement = NULL;

	assert (initNodeList->getLength () > 0);
	
	DOMElement* initElement = (DOMElement*) initNodeList->item (0);
	
	initFunctionsElement = (DOMElement*)initElement->getElementsByTagName (functions)->item (0);
	initPredicatesElement = (DOMElement*)initElement->getElementsByTagName (predicates)->item (0);
	
	assert (initFunctionsElement != NULL && initPredicatesElement != NULL);
	
	DOMNodeList* predicateNodeList = initPredicatesElement->getElementsByTagName (predicate);
		
	for(unsigned int i = 0; i < predicateNodeList->getLength(); i++) {
		DOMElement* predicateElement = (DOMElement*) predicateNodeList->item (i);
		
		//look up predicate in DOMDomain.
		char* predicateName = 
			XMLString::transcode (predicateElement->getAttribute(name));
		DOMPredicate* predicate  = this->domain->getPredicate (predicateName);
		XMLString::release(&predicateName);
		
		assert (predicate != NULL);
		
		initPredicates.insert (predicate);
	}
	
	DOMNodeList* assignNodeList = initFunctionsElement->getElementsByTagName (assignString);
	
	for(unsigned int i = 0; i < assignNodeList->getLength (); i++) {
		DOMElement* assignElement = (DOMElement*) assignNodeList->item (i);
		
		DOMElement* functionElement = (DOMElement*)assignElement->getElementsByTagName (function)->item (0);
		DOMElement* numberElement = (DOMElement*)assignElement->getElementsByTagName (number)->item (0);
		
		char* functionName = XMLString::transcode (functionElement->getAttribute(name));
		DOMFunction* function  = this->domain->getFunction (functionName);
		XMLString::release(&functionName);
		
		assert(function != NULL);
		
		char* numberString = XMLString::transcode (numberElement->getAttribute (value));
		
		double functionValue = atof (numberString);
		
		initFunctions [function] = functionValue;
	}
	
	//Not all functions have to be defined in the problem. Assign them a value of 0.
	set<DOMFunction*>::iterator it;
	for(it = domain->getFunctions().begin(); it != domain->getFunctions().end();
		it++) {
	
		if(initFunctions.count (*it) == 0) {
			initFunctions [*it] = 0;
		}
	}
}
	

void DOMProblem::createGoal ()
{
	this->goal = NULL;
	DOMNodeList* goalNodeList = this->node->getElementsByTagName (goalString);
	if(goalNodeList->getLength ()) {
		DOMElement* goalElement = (DOMElement*)goalNodeList->item (0);
		goal = new DOMRootCondition (goalElement, *domain);
	}
}

void DOMProblem::createInitialState () {
	//Create new Brazil State with no events scheduled, starting
	//at time 0, with current time 0 and with the problem's
	//initial predicate and resource values.
	//this->initialState = new BrazilState (0, 0, initPredicates, initFunctions);
}

map<DOMFunction*, double>& DOMProblem::initialFunctionValues () {
	return initFunctions;
}

void DOMProblem::setFunctionValue (DOMFunction* function, double functionValue) {
	initFunctions [function] = functionValue;
	
	DOMElement* assignElement = NULL;
	DOMNodeList* assignElements = 
		initFunctionsElement->getElementsByTagName (assignString);
	
	for(unsigned int i = 0; i < assignElements->getLength(); i++) {
		DOMElement* curAssignElement = (DOMElement*) assignElements->item (i);
		
		DOMElement* functionElement = 
			(DOMElement*)curAssignElement->getElementsByTagName 
				(this->function)->item (0);
			
		char* functionName = 
			XMLString::transcode (functionElement->getAttribute (name));
		
		if(0 == strcmp (functionName, function->getName ())) {
			assignElement = curAssignElement;
			XMLString::release (&functionName);
			break;
		}
		XMLString::release (&functionName);
	}
	
	if(NULL == assignElement) {
		DOMDocument* doc = this->node->getOwnerDocument();
		assignElement = doc->createElement (assignString);
		DOMElement* numberElement = doc->createElement (number);
		DOMElement* functionElement = doc->createElement (this->function);
		
		ostringstream stream;
		stream << functionValue << std::ends;
		
		if(stream) {
			string valueString = stream.str();
			const char *valueCString = valueString.c_str();		
			XMLCh *valueXMLCh = XMLString::transcode (valueCString);
			numberElement->setAttribute (value, valueXMLCh);
			//delete valueCString;
		}
		
		
		functionElement->setAttribute (name, function->getDOMElement()->getAttribute (name));
		
		assignElement->appendChild (functionElement);
		assignElement->appendChild (numberElement);
		
		initFunctionsElement->appendChild (assignElement);
	}
	else {
		DOMElement* numberElement = (DOMElement*)
			assignElement->getElementsByTagName (number)->item (0);
		
		ostringstream stream;
		stream << functionValue << std::ends;
		
		if(stream) {
			string valueString = stream.str();
			const char *valueCString = valueString.c_str();		
			XMLCh *valueXMLCh = XMLString::transcode (valueCString);
			numberElement->setAttribute (value, valueXMLCh);
			//delete valueCString;
		}
	}
}

DOMProblem::~DOMProblem ()
{
	XMLString::release (&initString);
	XMLString::release (&predicate);
	XMLString::release (&name);
	XMLString::release (&goalString);
	XMLString::release (&assignString);
	XMLString::release (&value);
	XMLString::release (&function);
	XMLString::release (&functions);
	XMLString::release (&predicates);
	XMLString::release (&number);
	delete goal;
}
