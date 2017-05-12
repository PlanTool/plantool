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
 *  DOMFixedDurationManipulationTest.h
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

class DOMFixedDurationManipulationTest : public CppUnit::TestFixture 
{
	private:
		DOMFixedDuration* duration;
		DOMDocument* doc;
		
		
		
	public:
		DOMFixedDurationManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void setUp ()
		{
			this->duration = new DOMFixedDuration (20, doc);
		}
		
		void tearDown ()
		{
			delete this->duration;
		}
		
		void testAssignment ()
		{
			this->duration->setDuration (30);
			CPPUNIT_ASSERT (this->duration->getDuration () == 30);
		}
		
		void testZeroAssignment ()
		{
			this->duration->setDuration (0);
			CPPUNIT_ASSERT(this->duration->getDuration () == 0);
		}
		
		
		void testNegativeAssignment ()
		{
			this->duration->setDuration (30);
			CPPUNIT_ASSERT_THROW( this->duration->setDuration (-10);, std::invalid_argument );
			CPPUNIT_ASSERT (this->duration->getDuration () == 30);
		}
};
