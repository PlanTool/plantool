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
 *  DOMNormalDurationManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 13/03/06.
 *  
 *
 */

#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMNormalDuration.h"
#include <cppunit/extensions/HelperMacros.h>

#include <stdexcept>

class DOMNormalDurationManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMNormalDuration* duration;
		DOMDocument* doc;
		
	public: 
		DOMNormalDurationManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
	
		void setUp ()
		{
			this->duration = new DOMNormalDuration (50, 20, doc);
		}
	
		void tearDown ()
		{
			delete duration;
		}
	
		void testAssignment ()
		{
			this->duration->setMean (100);
			CPPUNIT_ASSERT(this->duration->getMean() == 100);
			this->duration->setStandardDeviation(10);
			CPPUNIT_ASSERT(this->duration->getStandardDeviation() == 10);
		}
	
		void testNegativeAssignment ()
		{
			this->duration->setStandardDeviation(10);
			CPPUNIT_ASSERT_THROW (this->duration->setStandardDeviation(-10), std::invalid_argument);
			CPPUNIT_ASSERT(this->duration->getStandardDeviation() == 10);
		}
};
