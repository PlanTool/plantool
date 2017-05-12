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
 *  DOMOutcomeManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 13/03/06.
 *  
 *
 */


#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMFixedDuration.h"
#include <cppunit/extensions/HelperMacros.h>

#include <stdexcept>

class DOMOutcomeManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMDocument* doc;
		
	public:
		DOMOutcomeManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void testCreation ()
		{
			DOMOutcome* outcome = new DOMOutcome(QString("label"), 0.88, NULL, doc);
			
			CPPUNIT_ASSERT (outcome->getProbability () == 0.88);
			CPPUNIT_ASSERT (outcome->getLabel () == QString("label"));
			
			delete outcome;
		}
		
		void testProbabilityAssignment ()
		{
			
			DOMOutcome* outcome = new DOMOutcome(QString("label"), 0.88,NULL, doc);
			outcome->setProbability (0.9);
			
			CPPUNIT_ASSERT (outcome->getProbability() == 0.9);
			CPPUNIT_ASSERT_THROW(outcome->setProbability (1.1), std::invalid_argument);
			CPPUNIT_ASSERT (outcome->getProbability() == 0.9);
			
			delete outcome;
			
		}
		
		void  testLabelAssignment ()
		{
			
			DOMOutcome* outcome = new DOMOutcome(QString("label"), 0.88, NULL,doc);
			
			outcome->setLabel (QString("newlabel"));
			CPPUNIT_ASSERT (outcome->getLabel() == QString("newlabel"));
			
			delete outcome;
		}
		
		void testEffectAssignment ()
		{
			DOMDuration* duration = new DOMFixedDuration(10, doc);
			DOMDelayedEffect* effect = new DOMDelayedEffect (duration, doc);
			
			DOMOutcome* outcome = new DOMOutcome(QString("label"), 0.88,NULL, doc);
			
			outcome->setEffect(effect);
			CPPUNIT_ASSERT(outcome->getEffect() == effect);
			duration = new DOMFixedDuration(10, doc);
			DOMDelayedEffect* effectTwo = new DOMDelayedEffect (duration, doc);
			outcome->setEffect(effectTwo);
			
		
			CPPUNIT_ASSERT(outcome->getEffect() == effectTwo);
			
			cout << "delete outcome" << endl;
			delete outcome;
			cout << "done test effect assignment" << endl;
		}
};
