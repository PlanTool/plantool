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
 *  DOMFunctionExpressionTest.\h
 *  
 *
 *  Created by Owen Thomas on 17/07/06.
 *  
 *
 */

#ifndef dom_function_expression_test
#define dom_function_expression_test

#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMFunctionExpression.h"
#include <cppunit/extensions/HelperMacros.h>
#include <stdexcept>

class DOMFunctionExpressionValueTest : public CppUnit::TestFixture {
	
	private:
		DOMDocument* doc;
		DOMElement* testElement;
		
	public:
		DOMFunctionExpressionValueTest (DOMDocument* doc) {
			this->doc = doc;
		}
		
		virtual void setUp () {
			testElement = doc->createElement (XMLString::transcode ("number"));
			testElement->setTextContent (XMLString::transcode ("55"));
		}
		
		virtual void tearDown () {
			testElement->release ();
		}
		
		void testCreation () {
			DOMFunctionExpressionValue* value = new DOMFunctionExpressionValue (*testElement);
			CPPUNIT_ASSERT (value->getValue() == 55);
			const XMLCh* text = testElement->getTextContent ();
			XMLCh* fiftyfive = XMLString::transcode ("55");
			CPPUNIT_ASSERT (XMLString::equals (text, fiftyfive));
			XMLString::release (&fiftyfive);
		}
		
		void testAssignment () {
			DOMFunctionExpressionValue* value = new DOMFunctionExpressionValue (*testElement);
			CPPUNIT_ASSERT (value->getValue() == 55);
			value->setValue (20);
			CPPUNIT_ASSERT (value->getValue() == 20);
			const XMLCh* text = testElement->getTextContent ();
			XMLCh* twenty = XMLString::transcode ("20");
			CPPUNIT_ASSERT (XMLString::equals (text, twenty));
			XMLString::release (&twenty);
		}
};

#include "../DOMDomain.h"

class DOMFunctionExpressionFunctionTest : public CppUnit::TestFixture {
	private:
		DOMDocument* doc;
		DOMElement* testElement;
		DOMDomain*  testDomain;
		
	public:
		DOMFunctionExpressionFunctionTest (DOMDocument* doc) {
			this->doc = doc;
		}
		
		virtual void setUp () {
			testElement = doc->createElement (XMLString::transcode ("function"));
			XMLCh* name  = XMLString::transcode ("name");
			XMLCh* nameValue = XMLString::transcode ("a");
			
			testElement->setAttribute (name, nameValue);
			
			DOMElement* domainElement = doc->createElement (XMLString::transcode ("domain"));
			DOMElement* predicatesElement = doc->createElement (XMLString::transcode ("predicates"));
			DOMElement* functionsElement = doc->createElement (XMLString::transcode ("functions"));
			DOMElement* actionsElement = doc->createElement (XMLString::transcode ("actions"));
			
			domainElement->appendChild (predicatesElement);
			domainElement->appendChild (predicatesElement);
			domainElement->appendChild (functionsElement);
			domainElement->appendChild (actionsElement);
			
			DOMElement* functionElement = (DOMElement*)testElement->cloneNode (true);
			functionsElement->appendChild (functionElement);
			
			DOMElement* functionElementTwo = doc->createElement (XMLString::transcode ("function"));
			XMLCh* nameValueTwo = XMLString::transcode ("b");
			functionElementTwo->setAttribute (name, nameValueTwo);
			
			functionsElement->appendChild (functionElementTwo);
			
			DOMProbabilisticFactory* simpleFactory = new DOMProbabilisticFactory ();
			this->testDomain = new DOMDomain (domainElement, simpleFactory);
		}
		
		virtual void tearDown () {
			delete testDomain;
			testElement->release ();
		}
		
		void testCreation () {
			DOMFunctionExpressionFunction* function = new DOMFunctionExpressionFunction (*testElement, *testDomain);
			CPPUNIT_ASSERT (XMLString::equals (function->getFunction()->getName(), "a"));
		}
		
		void testAssignment () {
			DOMFunctionExpressionFunction* expression = new DOMFunctionExpressionFunction (*testElement, *testDomain);
			DOMFunction* function = testDomain->getFunction ("b");
			expression->setFunction (*function);
		
			CPPUNIT_ASSERT (expression->getFunction () == function);
			CPPUNIT_ASSERT (XMLString::equals(expression->getDOMElement()->getTagName (), XMLString::transcode("function")));
			XMLCh* name = XMLString::transcode ("name");
			XMLCh* b = XMLString::transcode ("b");
			CPPUNIT_ASSERT (XMLString::equals(expression->getDOMElement()->getAttribute (name), b));
			XMLString::release (&b);
			XMLString::release (&name);
		}
};

class DOMFunctionExpressionInternalTest {
	private:
		DOMDomain* testDomain;
		DOMDocument* doc;
		DOMElement* rootExpression;
		
	public:
		DOMFunctionExpressionInternalTest (DOMDocument* doc) {
			this->doc = doc;
		}
		
		virtual void setUp () {
			
			//Create Domain - very simple, just two functions.
			
			DOMElement* functionElement = doc->createElement (XMLString::transcode ("function"));
			XMLCh* name  = XMLString::transcode ("name");
			XMLCh* nameValue = XMLString::transcode ("a");
			
			functionElement->setAttribute (name, nameValue);
			
			DOMElement* domainElement = doc->createElement (XMLString::transcode ("domain"));
			DOMElement* predicatesElement = doc->createElement (XMLString::transcode ("predicates"));
			DOMElement* functionsElement = doc->createElement (XMLString::transcode ("functions"));
			DOMElement* actionsElement = doc->createElement (XMLString::transcode ("actions"));
			
			domainElement->appendChild (predicatesElement);
			domainElement->appendChild (predicatesElement);
			domainElement->appendChild (functionsElement);
			domainElement->appendChild (actionsElement);
			
			DOMElement* functionElementTwo = (DOMElement*)functionElement->cloneNode (true);
			functionsElement->appendChild (functionElementTwo);
			
			DOMElement* functionElementThree = doc->createElement (XMLString::transcode ("function"));
			XMLCh* nameValueThree = XMLString::transcode ("b");
			functionElementThree->setAttribute (name, nameValueThree);
			
			functionsElement->appendChild (functionElementTwo);
			
			DOMProbabilisticFactory* simpleFactory = new DOMProbabilisticFactory ();
			this->testDomain = new DOMDomain (domainElement, simpleFactory);

			//Create root expression: (30 - a) / 20
			rootExpression = doc->createElement (XMLString::transcode("functionExpression"));
			XMLCh* type = XMLString::transcode ("type");
			XMLCh* subtract = XMLString::transcode ("subtract");
			XMLCh* divide = XMLString::transcode ("divide");
			
			XMLCh* number = XMLString::transcode ("number");
			XMLCh* twenty = XMLString::transcode ("20");
			XMLCh* thirty = XMLString::transcode ("30");
			
			DOMElement* lhs = doc->createElement (XMLString::transcode("functionExpression"));
			DOMElement* rhs = doc->createElement (XMLString::transcode("number"));
			
			DOMElement* lhslhs = doc->createElement (XMLString::transcode ("number"));
			DOMElement* lhsrhs = doc->createElement (XMLString::transcode ("function"));
	
			rootExpression->appendChild (lhs);
			rootExpression->appendChild (rhs);
			
			lhs->appendChild(lhslhs);
			lhs->appendChild(lhsrhs);
			
			rootExpression->setAttribute (type, divide);
			lhs->setAttribute (type, subtract);
			
			rhs->setTextContent (twenty);
			
			lhslhs->setTextContent (thirty);
			lhsrhs->setAttribute (name, nameValue);
			
			
		}
		
		virtual void tearDown () {
			delete testDomain;
		}
		
		void testCreation () {
			DOMFunctionExpressionInternal* expression = 
				(DOMFunctionExpressionInternal*) DOMFunctionExpression::createExpression (*rootExpression, *testDomain);
				
			DOMFunctionExpressionInternal* lhs = (DOMFunctionExpressionInternal*)expression->getLeftHandSide ();
			DOMFunctionExpressionValue* rhs  = (DOMFunctionExpressionValue*)expression->getRightHandSide ();
			
			CPPUNIT_ASSERT (expression->getOperator () == DOMFunctionExpressionInternal::divide);
			CPPUNIT_ASSERT (lhs->getOperator () == DOMFunctionExpressionInternal::subtract);
			
			expression->setOperator (DOMFunctionExpressionInternal::add);
			CPPUNIT_ASSERT (expression->getOperator () == DOMFunctionExpressionInternal::add);
			XMLCh* add = XMLString::transcode ("add");
			XMLCh* type = XMLString::transcode ("type");
			
			CPPUNIT_ASSERT (XMLString::equals(expression->getDOMElement()->getAttribute (type), add));
			
			CPPUNIT_ASSERT (rhs->getValue () == 20);
			
			DOMFunctionExpressionValue* lhslhs = (DOMFunctionExpressionValue*)lhs->getLeftHandSide ();
			DOMFunctionExpressionFunction* lhsrhs = (DOMFunctionExpressionFunction*)lhs->getRightHandSide ();
			
			CPPUNIT_ASSERT (lhslhs->getValue() == 30);
			CPPUNIT_ASSERT (XMLString::equals(lhsrhs->getFunction()->getName(), "a"));
			
			DOMElement* newFunctionExpressionElement = doc->createElement (XMLString::transcode ("number"));
			XMLCh* forty = XMLString::transcode ("40");
			newFunctionExpressionElement->setTextContent (forty);
			
			DOMFunctionExpression* newExpression = 
				DOMFunctionExpression::createExpression (*newFunctionExpressionElement, *testDomain);
				
			lhs->setLeftHandSide (*newExpression);
			CPPUNIT_ASSERT (lhs->getLeftHandSide() == newExpression);
			CPPUNIT_ASSERT (newFunctionExpressionElement->getParentNode() == lhs->getDOMElement ());
			
		}
};
#endif
