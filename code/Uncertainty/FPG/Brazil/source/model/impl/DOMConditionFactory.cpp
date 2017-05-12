/*
 *  DOMConditionFactory.cpp
 *  
 *
 *  Created by Owen Thomas on 17/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMConditionFactory.h"

#include "DOMDomain.h"


DOMCondition* DOMConditionFactory::createCondition(DOMElement* element, DOMDomain& domain)
{
	//Currently all we do is switch on the element tag name,
	//if it's predicate we return a predicate condition other
	//wise we return an internal condition.

	static XMLCh* predicate = XMLString::transcode ("predicate");
	static XMLCh* conditionString = XMLString::transcode ("condition");
	static XMLCh* functionString = XMLString::transcode ("functionCondition");
	
	if(XMLString::equals (predicate, element->getNodeName()))
	{
		DOMPredicateCondition* condition = new DOMPredicateCondition (element);
		condition->setPredicate (*(domain.getPredicate (condition->getValue())));
				return condition;
	}
	else if (XMLString::equals (conditionString, element->getNodeName()))
	{
		return new DOMInternalCondition  (element,domain);
	}
	else if (XMLString::equals (functionString, element->getNodeName ()))
	{
		DOMFunctionCondition* condition = new DOMFunctionCondition (*element, domain);
		return condition;

	}
	else
	{
		return NULL;
	}
}

