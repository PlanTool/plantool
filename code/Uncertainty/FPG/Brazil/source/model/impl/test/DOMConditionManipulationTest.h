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
 *  DOMConditionManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 14/03/06.
 *  
 *
 */

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <xercesc/dom/DOM.hpp>

#include "../DOMConditionBase.h"
#include "../DOMConditionFactory.h"
#include "../DOMInternalCondition.h"

#include <stdexcept>

class DOMConditionManipulationTest : public CppUnit::TestFixture
{

	private:
		DOMDocument* doc;
		
	public:
		
		DOMConditionManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void testCreateOrCondition ()
		{
			DOMCondition* condition = DOMConditionFactory::createOrCondition (doc);
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString ("or"));
			CPPUNIT_ASSERT (condition->isInternalCondition());
			CPPUNIT_ASSERT (condition->getValue () == QString(""));
			CPPUNIT_ASSERT (condition->parent == NULL);
			
			DOMInternalCondition* intCondition = (DOMInternalCondition*) condition;
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 0);
		}
		
		void testCreateAndCondition ()
		{
			DOMCondition* condition = DOMConditionFactory::createAndCondition (doc);
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString ("and"));
			CPPUNIT_ASSERT (condition->isInternalCondition());
			CPPUNIT_ASSERT (condition->getValue () == QString(""));
			CPPUNIT_ASSERT (condition->parent == NULL);
			
			DOMInternalCondition* intCondition = (DOMInternalCondition*) condition;
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 0);
		}
		
		void testCreateNotCondition ()
		{
			DOMCondition* condition = DOMConditionFactory::createNotCondition (doc);
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString ("not"));
			CPPUNIT_ASSERT (condition->isInternalCondition());
			CPPUNIT_ASSERT (condition->getValue () == QString(""));
			CPPUNIT_ASSERT (condition->parent == NULL);
			
			DOMInternalCondition* intCondition = (DOMInternalCondition*) condition;
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 0);
		}
		
		void testInternalCondition ()
		{
			DOMCondition* condition = DOMConditionFactory::createAndCondition (doc);
			DOMInternalCondition* intCondition = (DOMInternalCondition*) condition;
			
			CPPUNIT_ASSERT(intCondition->getChildren().size() == 0);
			
			DOMCondition* childCondition = DOMConditionFactory::createNotCondition (doc);
			
			intCondition->addChild (childCondition);
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 1);
			CPPUNIT_ASSERT (childCondition->parent == intCondition);
			
			DOMCondition* anotherChildCondition = DOMConditionFactory::createNotCondition (doc);
			intCondition->addChild (anotherChildCondition);
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 2);
			CPPUNIT_ASSERT (anotherChildCondition->parent ==  intCondition);
			
			
			intCondition->removeChild (childCondition);
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 1);
			
			intCondition->removeChild (anotherChildCondition);
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 0);
			
			intCondition->addChild(childCondition);
			intCondition->addChild(childCondition);
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 1);
		}
		
		void testCreatePredicateConditionFromDOM ()
		{
			XMLCh* predicateString = XMLString::transcode ("predicate");
			XMLCh* nameString = XMLString::transcode ("name");
			XMLCh* nameValue = XMLString::transcode ("a-name");
			
			DOMElement* dom = doc->createElement (predicateString);
			dom->setAttribute (nameString, nameValue);
			
			DOMCondition* condition = DOMConditionFactory::createCondition (dom);
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString("predicate"));
			CPPUNIT_ASSERT (condition->getValue() == QString("a-name"));
			CPPUNIT_ASSERT (!condition->isInternalCondition());
			
			XMLString::release(&predicateString);
			XMLString::release(&nameString);
			XMLString::release(&nameValue);
		}
		
		void testCreateSimpleInternalConditionFromDOM ()
		{
			XMLCh* andString = XMLString::transcode ("and");
			
			DOMElement* dom = doc->createElement(andString);
			
			DOMCondition* condition = DOMConditionFactory::createCondition (dom);
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString("and"));
			CPPUNIT_ASSERT (condition->getValue() == QString (""));
			CPPUNIT_ASSERT (condition->isInternalCondition());
			
			DOMInternalCondition* intCondition = (DOMInternalCondition*)condition;
			
			CPPUNIT_ASSERT (intCondition->getChildren().size() == 0);
			
			XMLString::release(&andString);
		}
		
		void testCreateComplexInternalConditionFromDOM ()
		{
			XMLCh* andString = XMLString::transcode ("and");
			XMLCh* predicateString = XMLString::transcode ("predicate");
			XMLCh* orString = XMLString::transcode ("or");
			
			XMLCh* nameString = XMLString::transcode ("name");
			XMLCh* nameValue1String = XMLString::transcode("a-name");
			XMLCh* nameValue2String = XMLString::transcode("b-name");
			XMLCh* nameValue3String = XMLString::transcode("c-name");
			
			DOMElement* andDom = doc->createElement (andString);
			DOMElement* orDom = doc->createElement (orString);
			DOMElement* predicate1Dom = doc->createElement (predicateString);
			predicate1Dom->setAttribute (nameString, nameValue1String);
			
			andDom->appendChild (orDom);
			andDom->appendChild (predicate1Dom);
			
			DOMElement* predicate2Dom = doc->createElement (predicateString);
			DOMElement* predicate3Dom = doc->createElement (predicateString);
			
			predicate2Dom->setAttribute (nameString, nameValue2String);
			predicate3Dom->setAttribute (nameString, nameValue3String);
			
			orDom->appendChild (predicate2Dom);
			orDom->appendChild (predicate3Dom);
			
			DOMInternalCondition* andCondition = (DOMInternalCondition*)DOMConditionFactory::createCondition(andDom);
			
			CPPUNIT_ASSERT (andCondition->getChildren().size() == 2);
			CPPUNIT_ASSERT (andCondition->parent == NULL);
			CPPUNIT_ASSERT (andCondition->getDOMElement() == andDom);
			
			list<DOMCondition*> children = andCondition->getChildren ();
			
			list<DOMCondition*>::iterator it = children.begin();
			
			DOMInternalCondition* orCondition = (DOMInternalCondition*) *it++;
			DOMCondition* predicateCondition = *it;
			
			CPPUNIT_ASSERT(orCondition->parent == andCondition);
			CPPUNIT_ASSERT(orCondition->getChildren().size() == 2);
			CPPUNIT_ASSERT(orCondition->getOperatorName() == QString("or"));
			
			CPPUNIT_ASSERT(predicateCondition->parent == andCondition);
			CPPUNIT_ASSERT(predicateCondition->getOperatorName() == QString("predicate"));
			CPPUNIT_ASSERT(predicateCondition->getValue() == QString("a-name"));
			
			children = orCondition->getChildren();
			it = children.begin();
			
			DOMCondition* predicate2Condition = *it++;
			DOMCondition* predicate3Condition = *it;
			
			CPPUNIT_ASSERT (predicate2Condition->parent == orCondition);
			CPPUNIT_ASSERT (predicate2Condition->getOperatorName() == QString ("predicate"));
			CPPUNIT_ASSERT (predicate2Condition->getValue() == QString ("b-name"));
			
			CPPUNIT_ASSERT (predicate3Condition->parent == orCondition);
			CPPUNIT_ASSERT (predicate3Condition->getOperatorName() == QString ("predicate"));
			CPPUNIT_ASSERT (predicate3Condition->getValue() == QString ("c-name"));
			
			XMLString::release (&andString);
			XMLString::release (&orString);
			XMLString::release (&predicateString);
			
			XMLString::release (&nameString);
			XMLString::release (&nameValue1String);
			XMLString::release (&nameValue2String);
			XMLString::release (&nameValue3String);
		}
		
		void testCreatePredicateCondition ()
		{
			XMLCh* nameValueString = XMLString::transcode ("a-name");
			DOMPredicateCondition* condition = (DOMPredicateCondition*) DOMConditionFactory::createPredicateCondition (nameValueString,doc);
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString ("predicate"));
			CPPUNIT_ASSERT (condition->getValue() == QString ("a-name"));
			CPPUNIT_ASSERT (condition->parent == NULL);
			
			XMLString::release(&nameValueString);
		}
};
