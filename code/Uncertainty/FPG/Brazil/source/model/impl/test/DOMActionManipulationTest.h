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
 *  DOMActionManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 14/03/06.
 *  
 *
 */

#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMAction.h"
#include <cppunit/extensions/HelperMacros.h>

#include "../../Probabilistic.h"
#include <stdexcept>

class DOMActionManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMDocument* doc;
		
		
	public:
		DOMActionManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void testCreateBlankAction ()
		{
			XMLCh* nameString = XMLString::transcode ("name");
			XMLCh* nameValueString = XMLString::transcode ("a-name");
			XMLCh* actionString = XMLString::transcode ("action");
						
			DOMAction* action = new DOMAction ("a-name", doc);
			
			DOMElement* actionElement = action->getDOMElement();
			
			CPPUNIT_ASSERT (actionElement->getChildNodes()->getLength() == 0);
			CPPUNIT_ASSERT (0 == XMLString::compareIString (actionElement->getAttribute (nameString), nameValueString));
			CPPUNIT_ASSERT (0 == XMLString::compareIString (actionElement->getTagName(), actionString));
			CPPUNIT_ASSERT (NULL == action->getPrecondition());
			CPPUNIT_ASSERT (NULL == action->getEffect());
			CPPUNIT_ASSERT (action->getOutcomes().size() == 0);
		}
		
		void testAddPrecondition ()
		{
			XMLCh* nameString = XMLString::transcode ("name");
			XMLCh* actionString = XMLString::transcode ("action");
			XMLCh* nameValueString = XMLString::transcode ("a-name");
			
			XMLCh* predicate1Name = XMLString::transcode ("predicate1");
			XMLCh* predicate2Name = XMLString::transcode ("predicate2");
			
			XMLCh* preconditionString = XMLString::transcode ("precondition");
			
			cout << "make action" << endl;
			DOMAction* action = new DOMAction ("a-name", doc);

			cout << "make and condition" << endl;
			DOMInternalCondition* andCondition = (DOMInternalCondition*)DOMConditionFactory::createAndCondition (doc);
			cout << "p1" << endl;
			DOMCondition* predicate1Condition = DOMConditionFactory::createPredicateCondition(predicate1Name, doc);
			cout << "p2" << endl;
			
			DOMCondition* predicate2Condition = DOMConditionFactory::createPredicateCondition(predicate2Name, doc);
			
			cout << "add child" << endl;
			andCondition->addChild (predicate1Condition);
			andCondition->addChild (predicate2Condition);
			
			cout << "set precondition" << endl;
			action->setPrecondition (andCondition);
			
			CPPUNIT_ASSERT (action->getPrecondition() == andCondition);
			
			DOMElement* actionPreconditionElement = action->getPrecondition()->getDOMElement ();
			
			CPPUNIT_ASSERT (actionPreconditionElement == andCondition->getDOMElement ());
			CPPUNIT_ASSERT (actionPreconditionElement->getParentNode() == action->getDOMElement()->getElementsByTagName(preconditionString)->item(0));
			
			XMLString::release (&nameString);
			XMLString::release (&actionString);
			XMLString::release (&nameValueString);
			
			XMLString::release (&predicate1Name);
			XMLString::release (&predicate2Name);
		}
				
		void testAddEffect ()
		{
			DOMDuration* duration = new DOMFixedDuration (10, doc);
			DOMDelayedEffect* effect = new DOMDelayedEffect(duration,doc);
						
			XMLCh* name = XMLString::transcode("name");
			XMLCh* effectString = XMLString::transcode("effect");
			
			DOMAtomicEffect* atomicEffect = DOMAtomicEffect::createPredicate (name, doc);
			
			effect->addChild(atomicEffect);
			
			DOMAction* action = new DOMAction("blank", doc);
			
			CPPUNIT_ASSERT (NULL == action->getEffect());
			
			action->setEffect (effect);
			
			CPPUNIT_ASSERT (action->getEffect() == effect);
			
			CPPUNIT_ASSERT (effect->getDOMElement()->getParentNode() == action->getDOMElement()->getElementsByTagName (effectString)->item(0));
			CPPUNIT_ASSERT (action->getDOMElement()->getElementsByTagName(effectString)->getLength() == 1);
			CPPUNIT_ASSERT (action->getDOMElement()->getElementsByTagName(effectString)->item(0)->getChildNodes()->getLength() == 1);
			
			
			XMLString::release(&name);
			XMLString::release (&effectString);
			
		}
		
		void testAddOutcome ()
		{
			XMLCh* pString = XMLString::transcode("probabilistic");
			
			
			DOMAction* action = new DOMAction ("a-name", doc);
			DOMOutcome* outcome1 = new DOMOutcome(QString("label"), 0.1, NULL, doc);
			
			CPPUNIT_ASSERT (action->getOutcomes().size() == 0);
			
			action->addOutcome(outcome1);
			
			CPPUNIT_ASSERT (action->getOutcomes().size() == 1);
			CPPUNIT_ASSERT (action->getDOMElement()->getElementsByTagName (pString)->getLength() == 1);
			
			DOMOutcome* outcome2 = new DOMOutcome(QString("label"), 0.1, NULL, doc);
			
			action->addOutcome(outcome2);
			
			CPPUNIT_ASSERT (action->getOutcomes().size() == 2);
			
			CPPUNIT_ASSERT (action->getDOMElement()->getElementsByTagName (pString)->getLength() == 1);
			
			list<DOMOutcome*> children = action->getOutcomes();
			list<DOMOutcome*>::iterator it = children.begin();
			
			CPPUNIT_ASSERT (outcome1 ==  *it++);
		
			CPPUNIT_ASSERT (outcome2 == *it);
			
			CPPUNIT_ASSERT (outcome1->getDOMElement ()->getParentNode() == action->getDOMElement()->getElementsByTagName(pString)->item(0));
			CPPUNIT_ASSERT (outcome2->getDOMElement ()->getParentNode() == action->getDOMElement()->getElementsByTagName(pString)->item(0));
			
			CPPUNIT_ASSERT (outcome1->getProbabilistic() == (Probabilistic*) action);
			CPPUNIT_ASSERT (outcome2->getProbabilistic() == (Probabilistic*) action);
		}
		
		void testRemovePrecondition ()
		{
			XMLCh* nameString = XMLString::transcode ("name");
			XMLCh* actionString = XMLString::transcode ("action");
			XMLCh* nameValueString = XMLString::transcode ("a-name");
			XMLCh* preconditionString = XMLString::transcode ("precondition");
			
			DOMAction* action = new DOMAction ("a-name", doc);
			DOMCondition* andCondition = DOMConditionFactory::createAndCondition (doc);
						
			action->setPrecondition (andCondition);

			CPPUNIT_ASSERT (action->getPrecondition() != NULL);
			
			DOMCondition* returnedCondition = action->removePrecondition ();
			
			CPPUNIT_ASSERT (action->getPrecondition() == NULL);
			CPPUNIT_ASSERT (returnedCondition == andCondition);
			CPPUNIT_ASSERT (returnedCondition->getDOMElement ()->getParentNode() == NULL);
		}
		
		void testRemoveEffect ()
		{
			DOMDuration* duration = new DOMFixedDuration (10, doc);
			DOMDelayedEffect* effect = new DOMDelayedEffect(duration,doc);
						
			XMLCh* name = XMLString::transcode("name");
			XMLCh* effectString = XMLString::transcode("effect");
			
			DOMAtomicEffect* atomicEffect = DOMAtomicEffect::createPredicate (name, doc);
			
			effect->addChild(atomicEffect);
			
			DOMAction* action = new DOMAction("blank", doc);
			
			CPPUNIT_ASSERT (NULL == action->getEffect());
			
			action->setEffect (effect);
			
			CPPUNIT_ASSERT (action->getEffect() == effect);
	
			DOMDelayedEffect* removedEffect = action->removeEffect ();
	
			CPPUNIT_ASSERT (removedEffect == effect);
			CPPUNIT_ASSERT (removedEffect->getDOMElement ()->getParentNode() == NULL);
			
			XMLString::release(&name);
			XMLString::release(&effectString);
		}
		
		void testRemoveOutcome ()
		{
			XMLCh* pString = XMLString::transcode("probabilistic");
			
			DOMAction* action = new DOMAction ("a-name", doc);
			
			DOMOutcome* outcome1 = new DOMOutcome(QString("label"), 0.1, NULL, doc);
			
			DOMOutcome* outcome2 = new DOMOutcome(QString("label"), 0.1, NULL, doc);
			
			action->addOutcome(outcome1);
			
			action->addOutcome(outcome2);
			
			action->removeOutcome (outcome2);
			
			CPPUNIT_ASSERT(action->getOutcomes().size() == 1);
			CPPUNIT_ASSERT(outcome2->getDOMElement()->getParentNode() == NULL);
			
			list<DOMOutcome*> children = action->getOutcomes();
			list<DOMOutcome*>::iterator it = children.begin();

			CPPUNIT_ASSERT (outcome1 == *it);
			
			action->removeOutcome (outcome2);
			CPPUNIT_ASSERT (action->getOutcomes().size() == 1);
			
			action->removeOutcome (outcome1);
			CPPUNIT_ASSERT (action->getOutcomes().size() == 0);
			CPPUNIT_ASSERT (action->getDOMElement()->getElementsByTagName (pString)->getLength() == 0);
			
			XMLString::release(&pString);

		}
		
		void testProbabilitySum ()
		{
			XMLCh* pString = XMLString::transcode("probabilistic");
			
			DOMAction* action = new DOMAction ("a-name", doc);
			CPPUNIT_ASSERT_THROW (action->addOutcome (new DOMOutcome(QString("label"), 1.1, NULL, doc)), std::invalid_argument);
			
			CPPUNIT_ASSERT(action->getDOMElement()->getElementsByTagName (pString)->getLength() == 0);
			CPPUNIT_ASSERT(action->getOutcomes().size() == 0);
			
			DOMOutcome* outcome = new DOMOutcome(QString("label"), 0.1, NULL, doc);
			action->addOutcome (outcome);
			CPPUNIT_ASSERT_THROW (action->addOutcome (new DOMOutcome(QString("label"), 0.95, NULL, doc)), std::invalid_argument);
			
			CPPUNIT_ASSERT(action->getDOMElement()->getElementsByTagName (pString)->getLength() == 1);
			CPPUNIT_ASSERT(action->getOutcomes().size() == 1);

			action->addOutcome (new DOMOutcome(QString("label"), 0.1, NULL, doc));
			CPPUNIT_ASSERT_THROW (outcome->setProbability (0.95), std::invalid_argument);
			
			XMLString::release(&pString);
		}
		
		void testCreateActionFromDOM ()
		{
			XMLCh* actionString = XMLString::transcode ("action");
			XMLCh* nameString = XMLString::transcode ("name");
			XMLCh* nameValueString = XMLString::transcode ("action-name");
			
			XMLCh* preconditionString = XMLString::transcode ("precondition");
			XMLCh* effectString = XMLString::transcode ("effect");
			XMLCh* probabilisticString = XMLString::transcode ("probabilistic");
			
			XMLCh* predicateString = XMLString::transcode ("predicate");
			XMLCh* andString = XMLString::transcode("and");
			
			XMLCh* outcomeString = XMLString::transcode ("outcome");
			XMLCh* labelString = XMLString::transcode ("label");
			XMLCh* labelValue1String = XMLString::transcode ("label-1");
			XMLCh* labelValue2String = XMLString::transcode("label-2");
			
			XMLCh* probabilityString = XMLString::transcode ("probability");
			XMLCh* probabilityValueString = XMLString::transcode ("0.4");
			
			XMLCh* predicate1String = XMLString::transcode ("predicate1");
			XMLCh* predicate2String = XMLString::transcode ("predicate2");
			
			XMLCh* notString = XMLString::transcode ("not");
			
			XMLCh* delayedEffectString = XMLString::transcode ("delayedEffect");
			
			DOMElement* actionElem = doc->createElement (actionString);
			actionElem->setAttribute(nameString, nameValueString);
			
			DOMElement* preconditionElement = doc->createElement(preconditionString);
			DOMElement* effectElement = doc->createElement(effectString);
			DOMElement* probabilisticElement = doc->createElement(probabilisticString);
			
			actionElement->appendChild(preconditionElement);
			actionElement->appendChild(effectElement);
			actionElement->appendChild(probabilisticElement);
			
			DOMElement* predicateConditionElement = doc->createElement(predicateString);
			predicateConditionElement->setAttribute (nameString, predicate1String);
			
			preconditionElement->appendChild (predicateConditionElement);
			
			DOMElement* delayedEffectElement = doc->createElement(delayedEffectString);
		}
		
		void testSetName ()
		{
			DOMAction* action = new DOMAction("blank", doc);
			action->setName (QString("newname"));
			CPPUNIT_ASSERT (action->getName() == QString("newname"));
		}
};
