/*
 *  DOMInternalCondition.cpp
 *  
 *
 *  Created by Owen Thomas on 10/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMInternalCondition.h"
#include "DOMConditionFactory.h"
#include "DOMDomain.h"
DOMInternalCondition::DOMInternalCondition (DOMElement* node, DOMDomain& domain) : DOMCondition (node)
{
	this->domain = &domain;
	this->parent = NULL;
	this->andString = XMLString::transcode ("and");
	this->orString = XMLString::transcode ("or");
	this->typeString = XMLString::transcode ("type");
	
	DOMNodeList* nodeChildren = node->getChildNodes();

	///Loop through all element children and create child
	//condition elements.
	for(unsigned int i = 0; i < nodeChildren->getLength(); i++)
	{
	    DOMNode* current = nodeChildren->item(i);

		
		if(current->getNodeType() == DOMNode::ELEMENT_NODE)
		{
		
	                //Only create a child Condition if the
			//current element is a condition.
			if(DOMConditionFactory::isCondition ((DOMElement*)current)) {
			
				DOMCondition* nextChild = 
					DOMConditionFactory::createCondition ((DOMElement*)current, *this->domain);
			
				nextChild->parent  = this;
				children.push_back (nextChild);
			}
		}
	}
}


void DOMInternalCondition::addChild (DOMCondition* condition)
{
	node->appendChild(condition->getDOMElement());	
	children.push_back(condition);
	condition->parent = this;
	
	list<InternalConditionListener*>::iterator it;
	for(it = listeners.begin(); it != listeners.end(); it++) 
		(*it)->conditionAdded (condition);
}

void DOMInternalCondition::addChild (DOMCondition* after, DOMCondition* condition)
{
	if(after == NULL) { 
		addChild (condition);
		return;
	}
	list<DOMCondition*>::iterator it;
	for(it = children.begin(); it != children.end(); it++) {
		if (after == *it) {
			list<DOMCondition*>::iterator pos = it;
			pos ++;
			if(pos == children.end()) {
				addChild (condition);
				return;
			}
			
			children.insert (pos, condition);
			
			node->insertBefore (condition->getDOMElement (),(*pos)->getDOMElement ());
			condition->parent = this;
			list<InternalConditionListener*>::iterator it;
			for(it = listeners.begin(); it != listeners.end(); it++) 
				(*it)->conditionAdded (condition);
				
			break;
		}
	}

}

void DOMInternalCondition::removeChild (DOMCondition* condition)
{
	children.remove(condition);
	if(condition->getDOMElement()->getParentNode() == this->node) {
		this->node->removeChild(condition->getDOMElement());
	}
	if(condition->parent == this) {
		condition->parent = NULL;
	}
	
	list<InternalConditionListener*>::iterator it;
			for(it = listeners.begin(); it != listeners.end(); it++) 
				(*it)->conditionRemoved (condition);
}

DOMInternalCondition* DOMInternalCondition::createAndCondition ()
{
	XMLCh* andString = XMLString::transcode("and");
	XMLCh* conditionString = XMLString::transcode ("condition");
	XMLCh* typeString = XMLString::transcode ("type");
	DOMElement* andElement = node->getOwnerDocument()->createElement (conditionString);
	andElement->setAttribute (typeString, andString);
	
	DOMInternalCondition* child = new DOMInternalCondition (andElement, *this->domain);

	
	return child;
}

DOMInternalCondition* DOMInternalCondition::createOrCondition ()
{
	
	XMLCh* orString = XMLString::transcode("or");
	XMLCh* conditionString = XMLString::transcode ("condition");
	XMLCh* typeString = XMLString::transcode ("type");

	DOMElement* orElement = node->getOwnerDocument()->createElement (conditionString);
	orElement->setAttribute (typeString, orString);
	
	DOMInternalCondition* child = new DOMInternalCondition (orElement, *this->domain);


	return child;
}






