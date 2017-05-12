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
 *  EffectListener.h
 *  
 *   A Class can implement EffectListener and set itself to be the
 *  listener for a DOMDelayedEffect if it wants to receive notifications about
 *  changes to the effect.
 
 *  Created by Owen Thomas on 21/03/06.
 *  
 *
 */

#ifndef effect_listener
#define effect_listener

class DOMAtomicEffect;


class EffectListener 
{
	public:
		
		virtual ~EffectListener() { }
		
		virtual void effectAdded (DOMAtomicEffect* effect) = 0;
		
		virtual void effectRemoved (DOMAtomicEffect* effect) = 0;
		/**
		 * Signals that the duration has been changed.
		 * i.e. the duration has been replaced with another
		 * duration, not that the duration value has changed.
		 */
		virtual void durationChanged () = 0;
		
};

#endif
