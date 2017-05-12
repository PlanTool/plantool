/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  DOMCondition.h
 *  
 *
 *  Created by Owen Thomas on 6/03/06.
 *  
 *
 */

#ifndef inc_dom_internal_condition
#define inc_dom_internal_condition

#include <xercesc/dom/DOM.hpp>

#include <list>


#include "DOMPredicateCondition.h"

#include "../InternalConditionListener.h"


class DOMDomain;
/**
 * A boolean condition that's evaluated over a set of 
 * child conditions - such as or, not or and.
 *
 */
class DOMInternalCondition : public DOMCondition
{
	private:
						
	protected:
		XMLCh* andString;
		XMLCh* orString;
		XMLCh* typeString;
		
		DOMDomain* domain;

		list<DOMCondition*> children;
		list<InternalConditionListener*> listeners;
		
	public:
		DOMInternalCondition (DOMElement* node, DOMDomain& domain);

		/**
		 * Deletes the DOMInternalCondition, release the underlying
		 * DOMElement and deletes any child conditions.
		 */
		virtual ~DOMInternalCondition () 
		{ 
			//I remove the node from the parents node.
			//I do not however remove this DOMInternalCondition from the
			//parent DOMInternalCondition. This is because if this DOMInternal
			//Condition is being itereated over, this will corrupt the iteration.
			//The list of child dom conditions in this dom internal condition's parent
			//will be deleted when the parent is deleted.
			
			//A consequence of this is that the condition must be removed from
			//any parent conditions before it is deleted.
			
			if(node->getParentNode()) {
				node->getParentNode()->removeChild (node);
			}
			list<DOMCondition*>::iterator it;
			for(it = children.begin (); it != children.end(); it++) {
				delete (*it);
			}
			
			node->release(); 
			
			XMLString::release (&andString);
			XMLString::release (&typeString);
			XMLString::release (&orString);
		}
		
		/**
		 * Visit this internal condition with an application specific 
		 * visitor.
		 * @return can return true or false to indicate success of operation
		 */
		 
		 virtual bool visit (DOMConditionVisitor* visitor) {
			return visitor->visitInternal (this);
		 }
		 
		/**
		 * Add the DOMCondition as a child of this
		 * condition.
		 *
		 * @param condition, the DOMCondition to be added
		 * as a child of this condition.
		 *
		 * @throw invalid_argument if the condition
		 * cannot be added.
		 *
		 * TO-DO, a not condition should be made a 
		 * subclass of DOMInternalCondition and it should
		 * throw an exception if addChild is called when
		 * it already has a child.
		 */
		virtual void addChild (DOMCondition* condition);
		
		virtual void addChild (DOMCondition* after, DOMCondition* condition);
		
		/**
		 * Returns the children conditions of this
		 * InternalCondition.
		 *
		 * @return the Children of this condition.
		 */
		virtual list<DOMCondition*>& getChildren()
		{
			return children;
		}
		
		/**
		 * Removes the DOMCondition from the children of
		 * this condition. If condition is not a child
		 * of this Condition then this Condition is
		 * unchanged.
		 *
		 * @param condition, the child condition to 
		 * remove.
		 */
		virtual void removeChild (DOMCondition* condition);
		
		/**
		 * Returns the value of this Condition.
		 *
		 * @return "", the value of this condition.
		 */
		virtual char* getValue()
		{
			return "";
		}
		
		/**
		 * Returns the operator name of this Condition.
		 *
		 * @return the Operator name of this condition.
		 */
		virtual char* getOperatorName()
		{
			return getAttribute("type");
		}
			
		/**
		 * @return true.
		 */
		virtual bool isInternal ()
		{
			return true;
		}

		virtual DOMInternalCondition* createAndCondition ();
		virtual DOMInternalCondition* createOrCondition ();
		
		/**
		 * Returns true if this internal condition defines a conjunction
		 * over its child conditions. That is that for this condition to
		 * be regarded as 'true' each of it's child conditions must also
		 * be regarded as true.
		 (
		 */
		virtual bool isConjunctive ()
		{
			return XMLString::equals (andString,this->node->getAttribute (typeString) );
		}
		
		virtual void setConjunctive (bool conjunctive)
		{
			if(conjunctive)
				this->node->setAttribute (typeString, andString);
			else
				this->node->setAttribute (typeString, orString);
		}
		
		virtual void addListener (InternalConditionListener* listener) 
		{
			listeners.push_back (listener);
		}
		
		virtual void removeListener (InternalConditionListener* listener) {
			listeners.remove (listener);
		}
		virtual int getNumberOfChildConditions ()
		{
			return children.size();
		}
		
		virtual DOMDomain* getDomain () {
			return domain;
		}
};
#endif
