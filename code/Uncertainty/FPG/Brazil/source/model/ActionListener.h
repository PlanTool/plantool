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
 *  ActionListener.h
 *  
 *  A Class can implement ActionListener and set itself to be the
 *  listener for a DOMAction if it wants to receive notifications about
 *  changes to the action.
 *
 *  Created by Owen Thomas on 23/03/06.
 *  
 */
 

#ifndef action_listener
#define action_listener

class DOMProbabilistic;

class ActionListener
{
	public:
		virtual ~ActionListener () {}
		
		/**
		 * Signals that the Action's name
		 * has been changed.
		 */
		virtual void nameChanged() = 0;
		
		/**
		 * Signals that the Action's, deterministic
		 * effect, background colour has changed.
		 * 
		 */
		virtual void backgroundColourChanged () = 0;
		
		/**
		 * Signals that the Action has had a Probabilistic
		 * child added.
		 */
		virtual void probabilisticAdded (DOMProbabilistic*) = 0;
		
		virtual void probabilisticRemoved (DOMProbabilistic*) = 0;
};

#endif
