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
 *  DOMAtomicEffectManipulationTest.h
 *  
 *
 *  Created by Owen Thomas on 13/03/06.
 *  
 *
 */

#include <cppunit/TestFixture.h>
#include <xercesc/dom/DOM.hpp>
#include "../DOMAtomicEffect.h"
#include <cppunit/extensions/HelperMacros.h>

#include <stdexcept>

class DOMAtomicEffectManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMDocument* doc;
		
	public:
		
		DOMAtomicEffectManipulationTest(DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void setUp () { }
		void tearDown () { }
		
		void testPredicateCreation ()
		{
			XMLCh* name = XMLString::transcode ("test-name");
			QString nameQString ("test-name");
			QString predicate ("predicate");
			
			DOMAtomicEffect* effect = DOMAtomicEffect::createPredicate (name, doc);
			
			CPPUNIT_ASSERT(effect->getName() == nameQString);
			CPPUNIT_ASSERT(effect->getOperator() == predicate);
			
			XMLString::release(&name);
		}
		
		void testNegatedPredicateCreation()
		{
			XMLCh* name = XMLString::transcode ("test-name");
			QString nameQString ("test-name");
			QString notQString ("not");
			
			DOMAtomicEffect* effect= DOMAtomicEffect::createNegatedPredicate (name, doc);
			
			CPPUNIT_ASSERT(effect->getName() == nameQString);
			CPPUNIT_ASSERT(effect->getOperator() == notQString);
			
			XMLString::release(&name);
		}
};
