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
 *  DOMDelayedEffectManipulationTest.h
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

class DOMDelayedEffectManipulationTest : public CppUnit::TestFixture
{
	private:
		DOMDocument* doc;
		
	public:
		DOMDelayedEffectManipulationTest (DOMDocument* doc)
		{
			this->doc = doc;
		}
		
		void testCreation ()
		{
			DOMDuration* duration = new DOMFixedDuration (10, doc);
			DOMDelayedEffect* effect = new DOMDelayedEffect(duration,doc);
			
			CPPUNIT_ASSERT (effect->getDuration() == duration);
			
			delete effect;
		}
		
		void testDurationAssignment ()
		{
			DOMDuration* duration1 = new DOMUniformDuration (1, 10, doc);
			DOMDuration* duration2 = new DOMFixedDuration (1, doc);
			
			DOMDelayedEffect* effect = new DOMDelayedEffect (duration1, doc);
			effect->setDuration(duration2);
			
			CPPUNIT_ASSERT (effect->getDuration() == duration2);
			
			delete duration1;
			delete effect;
		}
		
		void testChildManipulation ()
		{
			DOMDuration* duration = new DOMFixedDuration (10, doc);
			DOMDelayedEffect* effect = new DOMDelayedEffect(duration,doc);
			
			CPPUNIT_ASSERT (effect->getChildren().size() == 0);
			
			XMLCh* name = XMLString::transcode("name");
			DOMAtomicEffect* atomicEffect = DOMAtomicEffect::createPredicate (name, doc);
			
			effect->addChild(atomicEffect);
			CPPUNIT_ASSERT (effect->getChildren().size() == 1);
			
			effect->addChild(atomicEffect);
			CPPUNIT_ASSERT (effect->getChildren().size() == 1);
			
			DOMAtomicEffect* atomicEffectTwo = DOMAtomicEffect::createPredicate (name, doc);
			effect->addChild(atomicEffectTwo);
			
			CPPUNIT_ASSERT (effect->getChildren().size() == 2);
			effect->removeChild(atomicEffectTwo);
			CPPUNIT_ASSERT(effect->getChildren().size() == 1);
			effect->removeChild(atomicEffect);
			CPPUNIT_ASSERT(effect->getChildren().size() == 0);
			
			effect->addChild(atomicEffect);
			effect->addChild(atomicEffectTwo);
			CPPUNIT_ASSERT (effect->getChildren().size() == 2);
			
			list<DOMAtomicEffect*> children = effect->getChildren();
			list<DOMAtomicEffect*>::iterator it = children.begin();
			CPPUNIT_ASSERT(*it == atomicEffect);
			it ++;
			CPPUNIT_ASSERT(*it == atomicEffectTwo);
			
			XMLString::release(&name);
			delete effect;
			
		}
};
