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
 *  DOMExponentialDurationManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 13/03/06.
 *  
 *
 */

#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMExponentialDuration.h"
#include <cppunit/extensions/HelperMacros.h>

#include <stdexcept>

class DOMExponentialDurationManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMExponentialDuration* duration;
		DOMDocument* doc;
		
	public:
		DOMExponentialDurationManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void setUp ()
		{
			this->duration = new DOMExponentialDuration (5, doc);
		}
		
		void tearDown ()
		{
			delete this->duration;
		}
		
		void testAssignment ()
		{
			duration->setLambda(10);
			CPPUNIT_ASSERT (duration->getLambda() == 10);
			
			CPPUNIT_ASSERT (duration->getMean() > 0.099 && duration->getMean() < 0.101);
			duration->setMean (10);
			CPPUNIT_ASSERT (duration->getMean() > 9.999 && duration->getMean() < 10.001);
			CPPUNIT_ASSERT (duration->getLambda() > 0.099 && duration->getLambda() < 0.101);
		}
		
		void testZeroAssignment ()
		{
			CPPUNIT_ASSERT_THROW (duration->setLambda(0), std::invalid_argument);
			CPPUNIT_ASSERT_THROW (duration->setMean(0), std::invalid_argument);
		}
		
		void testNegativeAssignment()
		{
			CPPUNIT_ASSERT_THROW (duration->setLambda(-1), std::invalid_argument);
			CPPUNIT_ASSERT_THROW (duration->setMean(-1), std::invalid_argument);
		}
};
