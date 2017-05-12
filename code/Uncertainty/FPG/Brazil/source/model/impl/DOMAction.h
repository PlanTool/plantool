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

#ifndef dom_action
#define dom_action
#include "../ActionListener.h"
#include "DOMDomain.h"
#include "DOMEffectSet.h"
#include "DOMRootCondition.h"
#include "DOMProbabilistic.h"

class DOMAction : public DOMWrapper {

	private:
		char* typeName;
		DOMDomain* domain;
		list<DOMProbabilistic*> probabilisticChildren;
		DOMEffectSet* effect;
		DOMEffectSet* atStartEffect;
		DOMRootCondition* precondition;
		DOMRootCondition* overallCondition;
		list<ActionListener*> actionListeners;
		
	protected:
		
		/**
		 * Methods used in the construction process.
		 */
		
		DOMRootCondition* createPrecondition ();
		DOMRootCondition* createOverallCondition ();
		DOMEffectSet* createEffect (XMLCh* effectName);
		void createProbabilisticChildren ();
		
	public:


		/** 
		 * [daa] Shortcut types for lists over DOMConditions. Used by planner.
                 */
		typedef list<DOMAction*> ActionList;
		typedef ActionList::iterator ActionIt;
		typedef ActionList::const_iterator ActionCIt;
	
		/**
		 * Create a new DOMAction instance as a member of domain,
		 * from element.
		 *
		 */
		DOMAction (DOMElement& element, DOMDomain& domain);
		
		/**
		 * Create a new DOMAction instance named name, with the deterministic
		 * effect set effect and as a member of domain. This will create a new
		 * dom element to store the data for this dom action.
		 */
		DOMAction (char* name, DOMDomain& domain);
		
		virtual ~DOMAction ();
		
		/**
		 * Return this action's domain.
		 */
		virtual DOMDomain* getDomain ();
		
		/**
		 * Return all the probabilistic children. This is an immutable
		 * list. Use the add / remove methods of this class
		 * to modify the probabilistic children of this DOMAction.
		 */
		virtual  list<DOMProbabilistic*>& getProbabilisticChildren ();
		
		virtual void addProbabilistic (DOMProbabilistic* probabilistic);
		
		virtual void removeProbabilistic (DOMProbabilistic* probabilistic);
		
		/**
		 * Return the deterministic effects of this DOMAction.
		 * This will never return NULL, every DOMAction has a (possibly empty)
		 * effect set.
		 */ 
		virtual DOMEffectSet* getEffectSet ();

		/**
		 * Return the deterministic effects of *starting* this action
		 * This will never return NULL, every DOMAction has a (possibly empty)
		 * effect set.
		 */ 
		virtual DOMEffectSet* getAtStartEffectSet ();
		
		/**
		 * Return the Precondition of this DOMAction. Every action has a 
		 * (possibly empty) precondition, this method will not return NULL.
		 */
		virtual DOMRootCondition* getPrecondition ();
		
		/**
		 * Return the overall condition of this DOMAction. Every action has a 
		 * (possibly empty) precondition, this method will not return NULL.
		 */
		
		virtual DOMRootCondition* getOverallCondition ();
		
		virtual void addActionListener (ActionListener* listener);
		
		virtual void removeActionListener (ActionListener* listener);
		
		virtual char* getBackgroundColour ();
		
		virtual void setBackgroundColour (char* colour);
		
		virtual char* getName ();
		
		virtual void setName (char* name);
		
		virtual char* getTypeName ();
};

#endif
