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
#include "../DOMPredicateCondition.h"
#include "../DOMConditionFactory.h"
#include "../DOMInternalCondition.h"
#include "../DOMDomain.h"
#include <stdexcept>

class DOMConditionTest : public CppUnit::TestFixture
{

	private:
		DOMDocument* doc;
		DOMDomain* domain;
		
		XMLCh* domainString;
		XMLCh* domainNameValue;
		XMLCh* predicatesString;
		XMLCh* predicateString;
		XMLCh* nameString;
		XMLCh* predicate1NameValue;
		XMLCh* predicate2NameValue;
		XMLCh* predicate3NameValue;
		XMLCh* predicate4NameValue;
		XMLCh* predicate5NameValue;
		XMLCh* andString;
		
	public:
		
		DOMConditionTest (DOMDocument* doc)
		{
			this->doc = doc;
			
			domainString = XMLString::transcode ("domain");
			domainNameValue = XMLString::transcode ("test-domain");
			predicatesString = XMLString::transcode ("predicates");
			predicateString = XMLString::transcode ("predicate");
			nameString = XMLString::transcode ("name");
			predicate1NameValue = XMLString::transcode ("predicate-1");
			predicate2NameValue = XMLString::transcode ("predicate-2");
			predicate3NameValue = XMLString::transcode ("predicate-3");
			predicate4NameValue = XMLString::transcode ("predicate-4");
			predicate5NameValue = XMLString::transcode ("predicate-5");
			
			andString = XMLString::transcode ("and");
			
			DOMElement* domainElement = doc->createElement (domainString);
			DOMElement* predicatesElement = doc->createElement (predicatesString);
		
			DOMElement* predicate1Element = doc->createElement (predicate1NameValue);
			DOMElement* predicate2Element = doc->createElement (predicate2NameValue);
			DOMElement* predicate3Element = doc->createElement (predicate3NameValue);
			DOMElement* predicate4Element = doc->createElement (predicate4NameValue);
			DOMElement* predicate5Element = doc->createElement (predicate5NameValue);
			
			domainElement->appendChild (predicatesElement);
			
			predicatesElement->appendChild (predicate1Element);
			predicatesElement->appendChild (predicate2Element);
			predicatesElement->appendChild (predicate3Element);
			predicatesElement->appendChild (predicate4Element);
			predicatesElement->appendChild (predicate5Element);
			
			domainElement->setAttribute (nameString, domainNameValue);
			
			predicate1Element->setAttribute (nameString, predicate1NameValue);
			predicate2Element->setAttribute (nameString, predicate2NameValue);
			predicate3Element->setAttribute (nameString, predicate3NameValue);
			predicate4Element->setAttribute (nameString, predicate4NameValue);
			predicate5Element->setAttribute (nameString, predicate5NameValue);
			
			domain = new DOMDomain (domainElement);
		}
		
		virtual ~DOMConditionTest ()
		{
			XMLString::release (&domainString);
			XMLString::release (&domainNameValue);
			XMLString::release (&predicatesString);
			XMLString::release (&predicateString);
			XMLString::release (&nameString);
			
			XMLString::release (&predicate1NameValue);
			XMLString::release (&predicate2NameValue);
			XMLString::release (&predicate3NameValue);
			XMLString::release (&predicate4NameValue);
			XMLString::release (&predicate5NameValue);
			
			XMLString::release (&andString);
		}
		
		void testCondition ()
		{
			XMLCh* negatedString = XMLString::transcode ("negated");
			XMLCh* falseString = XMLString::transcode ("false");
			XMLCh* trueString = XMLString::transcode ("true");
			
			
			DOMElement* dom = doc->createElement (predicateString);
			dom->setAttribute (nameString, predicate1NameValue);

			DOMCondition* condition = DOMConditionFactory::createCondition (dom, *domain);

			CPPUNIT_ASSERT (!condition->isNegated());
			
			dom->setAttribute (negatedString, falseString);
			
			CPPUNIT_ASSERT (!condition->isNegated());
			CPPUNIT_ASSERT (!condition->isInternal());
			
			condition->setNegated (true);
			
			CPPUNIT_ASSERT (condition->isNegated ());
			CPPUNIT_ASSERT (XMLString::equals (dom->getAttribute (negatedString), trueString));
			
			CPPUNIT_ASSERT (QString("predicate") == condition->getOperatorName());
			CPPUNIT_ASSERT (QString ("predicate-1") == condition->getValue ());
			
			XMLString::release (&negatedString);
			XMLString::release (&falseString);
			XMLString::release (&trueString);
		}
						
		void testInternalCondition ()
		{
			DOMElement* dom = doc->createElement (andString);
			DOMElement* predicate1Element = doc->createElement (predicateString);
			predicate1Element->setAttribute (nameString, predicate1NameValue);
			
			dom->appendChild (predicate1Element);
			
			DOMCondition* condition = DOMConditionFactory::createCondition (dom, *domain);
			cout << condition << endl;
			CPPUNIT_ASSERT (condition->isInternal());
			CPPUNIT_ASSERT (!condition->isNegated());
			
			DOMInternalCondition* internalCondition = (DOMInternalCondition*) condition;
			
			CPPUNIT_ASSERT (condition->getOperatorName() == QString ("and"));
			CPPUNIT_ASSERT (condition->getValue() == QString (""));
			CPPUNIT_ASSERT (internalCondition->getChildren().size() == 1);
			
			DOMElement* predicate2Element = doc->createElement (predicateString);
			predicate2Element->setAttribute (nameString, predicate2NameValue);
			
			DOMCondition* predicateCondition = DOMConditionFactory::createCondition (predicate2Element, *domain);
			
			internalCondition->addChild (predicateCondition);
			
			CPPUNIT_ASSERT (internalCondition->getChildren().size() == 2);
			CPPUNIT_ASSERT (predicateCondition->getValue() == QString ("predicate-2"));
			
			bool isChild = false;
			DOMNodeList* nodeList = dom->getElementsByTagName (predicateString);
			for(int i = 0; i < nodeList->getLength(); i++) {
				if(nodeList->item(i) == predicate2Element) isChild = true;
			}
			
			CPPUNIT_ASSERT (isChild);
			
			internalCondition->removeChild (predicateCondition);
			CPPUNIT_ASSERT(internalCondition->getChildren().size() == 1);
			
			isChild = false;
			nodeList = dom->getElementsByTagName (predicateString);
			for(int i = 0; i < nodeList->getLength(); i++) {
				if(nodeList->item(i) == predicate2Element) isChild = true;
			}
			
			
			CPPUNIT_ASSERT (!isChild);
			
			DOMCondition* childCondition = internalCondition->getChildren().front();
			CPPUNIT_ASSERT (childCondition->getValue() == QString ("predicate-1"));
			CPPUNIT_ASSERT (childCondition->getDOMElement() == predicate1Element);
			CPPUNIT_ASSERT (((DOMPredicateCondition*)childCondition)->getPredicate() == domain->getPredicate (QString("predicate-1")));
		}
};
