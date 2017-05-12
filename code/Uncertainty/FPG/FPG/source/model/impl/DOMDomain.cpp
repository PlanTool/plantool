/*
 *  DOMDomain.cpp
 *  
 *
 *  Created by Owen Thomas on 9/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <string>
#include "DOMDomain.h"
#include "DOMProblem.h"
#include "DOMPropertySet.h"

/**
 * Entry in C++ File.
 */
DOMDomain::DOMDomain (DOMElement* node)
{
	this->node = node;
	this->predicatesElement = NULL;
	this->actionsElement = NULL;
	this->functionsElement = NULL;
	this->propertySet = NULL;
	
	createPredicates ();
	createFunctions ();
	createActions ();
	createProblem ();
	createPropertySet();
}

DOMDomain::~DOMDomain ()
{
	//delete predicates, functions, actions.
	map<const char*,DOMPredicate*>::iterator it;
	for(it = predicates.begin(); it != predicates.end(); it++) {
		delete (it->second);
	}
	list<DOMAction*>::iterator actionIt;
	for(actionIt = actions.begin(); actionIt != actions.end(); actionIt ++) {
		delete (*actionIt);
	}
	map<const char*, DOMFunction*>::iterator functionIt;
	for(functionIt = functions.begin(); functionIt != functions.end(); functionIt++) {
		delete (functionIt->second);
	}
	
	delete problem;

	if (propertySet != NULL) {
		node->removeChild(propertySet->getDOMElement());
	    //propertySet->getDOMElement()->release();
	    delete propertySet;
	}


	//release myself if I can.
	if(node->getParentNode () == NULL) node->release ();

}

void DOMDomain::createProblem () 
{
	XMLCh* problemName = XMLString::transcode ("problem");
	DOMNodeList* problemList = node->getElementsByTagName (problemName);
	problem = new DOMProblem ((DOMElement*)problemList->item (0), *this);
	XMLString::release (&problemName);
}


/**
 * Find the first tag with the name "properties" and hook it in as the property set.
 * Also provide the property set with the default properties that are hard wired in code.
 * In future the user might override the properties with another file.
 * @author daa
 */
void DOMDomain::createPropertySet () 
{

        // Generate defaults
	DOMDocument* doc = node->getOwnerDocument();
	DOMPropertySet* dps =  DOMPropertySet::makeDefaults(doc);

	// Are there any props in the file
	XMLCh* propertiesTag = XMLString::transcode ("properties");
	DOMNodeList* propList = node->getElementsByTagName (propertiesTag);
	XMLString::release (&propertiesTag);
	if (propList->getLength() > 0) 	{
	         propertySet = new DOMPropertySet((DOMElement*)propList->item (0));
		// Provide some hard wired defaults for critical properties
		propertySet->setDefaultPropertySet(dps);
	}
	else {
		propertySet = dps;
		this->node->appendChild (propertySet->getDOMElement ());
	}

}


void DOMDomain::createPredicates ()
{
	XMLCh* predicatesName = XMLString::transcode ("predicates");
	DOMNodeList* predicatesChild = node->getElementsByTagName (predicatesName);
	
	//If we have a <predicates> element, construct a DOMPredicate for each 
	//<predicate> child.
	if (predicatesChild->getLength()) {
		XMLCh* predicateName = XMLString::transcode ("predicate");
		this->predicatesElement = (DOMElement*)predicatesChild->item (0);
	
		DOMNodeList* predicateChildren = predicatesElement->getElementsByTagName (predicateName);
		
		for(unsigned int i = 0; i < predicateChildren->getLength(); i++) {
			DOMPredicate* newPredicate = new DOMPredicate ((DOMElement*)predicateChildren->item(i));
			
			
			char*  copyName = new char [ strlen (newPredicate->getName()) + 1];
			strcpy (copyName, newPredicate->getName ());
	
			predicates [copyName] = newPredicate;

			predicateSet.insert (newPredicate);
		}
		XMLString::release(&predicateName);
	}
	
	
	XMLString::release (&predicatesName);
}


void DOMDomain::createFunctions ()
{
	XMLCh* functionsName = XMLString::transcode ("functions");
	
	DOMNodeList* functionsChild = node->getElementsByTagName (functionsName);
	
	if(functionsChild->getLength ()) {
		XMLCh* functionName = XMLString::transcode ("function");
		this->functionsElement = (DOMElement*)functionsChild->item(0);
		
		DOMNodeList* functionChild = functionsElement->getElementsByTagName (functionName);
		
		for(unsigned int i = 0; i < functionChild->getLength(); i++)
		{
			DOMFunction* newFunction = new DOMFunction ((DOMElement*)functionChild->item (i));
			functionSet.insert (newFunction);
			functions [newFunction->getName()] = newFunction;
		}
		XMLString::release (&functionName);
	}
	XMLString::release (&functionsName);
}

void DOMDomain::createActions ()
{
	XMLCh* actionsName = XMLString::transcode ("actions");
	DOMNodeList* actionsChild = node->getElementsByTagName (actionsName);
	
	//If we have an <actions> element, construct a DOMAction for
	//each <action> child.
	if(actionsChild->getLength ()) {
		XMLCh* actionName = XMLString::transcode ("action");
		this->actionsElement = (DOMElement*)actionsChild->item(0);
		DOMNodeList* actionChildren = actionsElement->getElementsByTagName (actionName);
		
		for(unsigned int i = 0; i < actionChildren->getLength(); i++) {
			DOMAction* action = new DOMAction ( *(DOMElement*)actionChildren->item (i), *this);
			actions.push_back (action);
			
		}
		XMLString::release(&actionName);
	}
	
	XMLString::release (&actionsName);
}

list<DOMAction*>& DOMDomain::getActions ()
{
	return actions;
}

void DOMDomain::addPredicate (DOMPredicate* predicate)
{
	
	if(!predicatesElement) {
		XMLCh* predicatesName = XMLString::transcode ("predicates");
		predicatesElement = node->getOwnerDocument()->createElement (predicatesName);
			
		if(functionsElement) {
			node->insertBefore (predicatesElement, functionsElement);
		} else if (actionsElement) {
			node->insertBefore (predicatesElement, actionsElement);
		} else {
			node->appendChild (predicatesElement);
		}
	}
	predicateSet.insert (predicate);
	
	char*  copyName = new char [ strlen (predicate->getName()) + 1];
	strcpy (copyName, predicate->getName ());

	predicates [copyName] = predicate;
	
	
	if(predicate->getDOMElement()->getParentNode() != predicatesElement) {
		predicatesElement->appendChild (predicate->getDOMElement ());		
	}
	
	list<DomainListener*>::iterator it;
	for(it = listeners.begin(); it != listeners.end(); it++) {
		(*it)->predicateAdded (predicate);
	}
}

void DOMDomain::addFunction (DOMFunction* function)
{
	if(!functionsElement) {
		XMLCh* functionsName = XMLString::transcode ("functions");
		functionsElement = node->getOwnerDocument()->createElement (functionsName);
		
		if(actionsElement) {
			node->insertBefore (functionsElement, actionsElement);
		} else {
			node->appendChild (functionsElement);
		}
	}
	
	if(function->getDOMElement()->getParentNode() != functionsElement) {
		if(NULL != function->getDOMElement()->getParentNode ()) {
			function->getDOMElement()->getParentNode()->removeChild (function->getDOMElement());
		}
		functionsElement->appendChild (function->getDOMElement());
	}
	
	functionSet.insert (function);
	functions [function->getName()] = function;

	problem->setFunctionValue (function, 0);
	
	list<DomainListener*>::iterator it;
	for(it = listeners.begin(); it != listeners.end(); it++) {
		(*it)->functionAdded (function);
	}

}

void DOMDomain::addAction (DOMAction* action)
{
	
	if(!actionsElement) {
		XMLCh* actionsName = XMLString::transcode ("actions");
		actionsElement = node->getOwnerDocument()->createElement (actionsName);
		node->appendChild (actionsElement);

	}
	
	unsigned int sizeBefore = actions.size();
	
	actions.push_back (action);
	
	if(action->getDOMElement()->getParentNode() != actionsElement) {
		actionsElement->appendChild (action->getDOMElement ());
	}
	
	if(sizeBefore != actions.size()) {
		list<DomainListener*>::iterator it;
		for(it = listeners.begin(); it != listeners.end(); it++) {
			(*it)->actionAdded (action);
		}
	}
}

void DOMDomain::removeAction (DOMAction* action)
{
	unsigned int sizeBefore = actions.size();
	actions.remove(action);
	
	if(actionsElement) {
		if(action->getDOMElement()->getParentNode() == actionsElement) {
			actionsElement->removeChild (action->getDOMElement ());
		}
	}
	
	if(sizeBefore != actions.size()) {
		list<DomainListener*>::iterator it;
		for(it = listeners.begin(); it != listeners.end(); it++) {
			(*it)->actionRemoved (action);
		}
	}
}

char* DOMDomain::getName ()
{
	return getAttribute ("name");
}

void DOMDomain::setName (const char* name)
{
	setAttribute ("name", name);
	list<DomainListener*>::iterator it;
	for(it = listeners.begin(); it != listeners.end(); it++) {
		(*it)->nameChanged (name);
	}
}






