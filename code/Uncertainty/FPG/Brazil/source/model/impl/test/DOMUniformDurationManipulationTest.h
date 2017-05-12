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
 *  DOMUniformDurationManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 13/03/06.
 *  
 *
 */

#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMUniformDuration.h"
#include <cppunit/extensions/HelperMacros.h>

#include <stdexcept>

class DOMUniformDurationManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMUniformDuration* duration;
		DOMDocument* doc;
		
	public:
		DOMUniformDurationManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void setUp ()
		{
			this->duration = new DOMUniformDuration (10, 20, doc);
		}
		
		void tearDown ()
		{
			delete this->duration;
		}
		
		void testAssignment ()
		{
			this->duration->setMinDuration (1);
			CPPUNIT_ASSERT (this->duration->getMinDuration() == 1);
			this->duration->setMaxDuration (10);
			CPPUNIT_ASSERT (this->duration->getMinDuration() == 1);
			CPPUNIT_ASSERT (this->duration->getMaxDuration() == 10);
		}
		
		void testBadAssignment ()
		{
			delete duration;
			duration = new DOMUniformDuration (10, 20, doc);
			CPPUNIT_ASSERT_THROW (this->duration->setMinDuration (30), std::invalid_argument);
			CPPUNIT_ASSERT (this->duration->getMinDuration() == 10);
			CPPUNIT_ASSERT (this->duration->getMaxDuration() == 20);
			CPPUNIT_ASSERT_THROW (this->duration->setMaxDuration (5), std::invalid_argument);
			CPPUNIT_ASSERT (this->duration->getMinDuration() == 10);
			CPPUNIT_ASSERT (this->duration->getMaxDuration() == 20);
		}
		
		void testZeroAssignment()
		{
			delete duration;
			duration = new DOMUniformDuration (10, 20, doc);
			
			this->duration->setMinDuration(0);
			CPPUNIT_ASSERT(this->duration->getMinDuration () == 0);
		}
		
		void testEqualAssignment()
		{
			delete duration;
			
			duration = new DOMUniformDuration (10, 20, doc);
			
			CPPUNIT_ASSERT_NO_THROW (duration->setMaxDuration (10));
		}
		
		void testNegativeAssignment()
		{
			delete duration;
			duration = new DOMUniformDuration(10, 20, doc);
			
			CPPUNIT_ASSERT_THROW(duration->setMinDuration(-5), std::invalid_argument);
			CPPUNIT_ASSERT_THROW(duration->setMaxDuration(-2), std::invalid_argument);
			
			CPPUNIT_ASSERT(duration->getMinDuration() == 10);
			CPPUNIT_ASSERT(duration->getMaxDuration() == 20);
		}
};
