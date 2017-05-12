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
 *  DOMOutcome.h
 *  
 *  
 *  Created by Owen Thomas on 6/03/06.
 *  
 *
 */

#ifndef inc_dom_outcome
#define inc_dom_outcome

#include "DOMWrapper.h"
#include "../OutcomeListener.h"
#include "DOMEffectSet.h"

#include <map>
#include <set>
class DOMProbabilistic;

/**
 * A DOMOutcome is a single discrete probabilistic outcome, with a 
 * probability, associated Probabilistic parent and label. 
 *
 * A DOMOutcome can have an optional DOMDelayedEffect.
 * 
 */
class DOMOutcome : public DOMWrapper
{
	private:
		
		DOMProbabilistic* parent;
		
		//A pointer to an object that wants to be notified about
		//changes to this Outcome.
		OutcomeListener* listener;

		char* name;
	
	protected:
		list<DOMEffectSet*> effects;

		DOMEffectSet* atStartEffects;

	public:

		/**
		 * Shortcut types
		 * @author doug
		 */
		typedef list<DOMEffectSet*> Effects;
		typedef Effects::iterator EffectsIt;
		typedef Effects::const_iterator EffectsCIt;

		
		/**
		 * Shortcut types
		 * @author doug
		 * modified owen, to reflect changes in Probabilistic
		 * interface.
		 */
		typedef map<DOMOutcome*,double> Outcomes;
		typedef Outcomes::iterator OutcomesIt;
		typedef Outcomes::const_iterator OutcomesCIt;

		/**
		 * Indicates whether element can be interpreted as an Outcome.
		 *
		 * @return true if element represents an Outcome.
		 */
		 
		 static bool isOutcome (DOMElement* element)
		 {
			XMLCh* outcomeTag = XMLString::transcode ("outcome");
			XMLCh* name = XMLString::transcode ("label");
			XMLCh* probability = XMLString::transcode ("probability");
			
			bool returnValue = false;
			
			returnValue = (0 == XMLString::compareIString (outcomeTag, element->getNodeName()))
				&& element->hasAttribute (name)
				&& element->hasAttribute (probability);
			
			XMLString::release(&outcomeTag);
			XMLString::release(&name);
			XMLString::release(&probability);
			
			return returnValue;
		 }
		/**
		 * Creates a DOMOutcome over the input DOMElement, as a part
		 * of the Probabilistic group pointed to by parent and with 
		 * reference to the input DOMDocument that was used to create
		 * node. A DOMDocument is required as a DOMOutcome can add subtrees
		 * to the DOM.
		 *
		 * @param node, the DOMElement representing the Outcome.
		 * @param parent, the Probabilistic group this Outcome belongs to
		 * @param doc, the DOMDocument used to create node.
		 */
		DOMOutcome (DOMElement *node, DOMProbabilistic* parent);
			
		/**
		 * Creates a DOMOutcome and the underlying, internal DOMElement that it represents.
		 * The DOMOutcome has a label, and probability between 0 and 1, and is a part of
		 * the probabilistic group pointed to by parent. The DOMElement representing the 
		 * Outcome is created by doc.
		 *
		 * @parem label, the Label of this DOMOutcome.
		 * @param parent, the Probabilistic group this Outcome belongs to.
		 * @param doc, the DOMDocument that is used to create the underlying DOMElement representing this
		 * Outcome.
		 *
		 * @throw invalid_argument if probability < 0 or probability > 1.
		 */
		DOMOutcome (char* label, DOMProbabilistic* parent, DOMDocument* doc);
		
		/**
		 * Delete the Outcome and release any memory.
		 *
		 * The underlying DOMElement is released if it has
		 * no parent node.
		 *
		 */
		virtual ~DOMOutcome();
				
		virtual char* getBackgroundColour ()
		{
			return getAttribute ("backgroundColour");
		}
		
		virtual void setBackgroundColour (char* colour)
		{
			setAttribute ("backgroundColour", colour);
			if(listener)
				listener->backgroundColourChanged ();
		}
		
		virtual OutcomeListener* getListener()
		{
			return listener;
		}
		
		virtual void setListener(OutcomeListener* listener)
		{
			this->listener = listener;
		}
		
		
		virtual double getProbability ()
		{
			return getAttributeAsDouble ("probability");
		}
		
		
		virtual char* getLabel ()
		{
			return getAttribute ("label");
		}
		
		virtual void setLabel (char* label)
		{
			setAttribute ("label", label);
			
			if(listener)
				listener->labelChanged();
		}
		
		virtual list<DOMEffectSet*>& getEffects ()
		{
			return effects;
		}

		DOMEffectSet* getAtStartEffects ()
		{
			return atStartEffects;
		}
		
		virtual DOMProbabilistic* getProbabilistic ()
		{
			return parent;
		}
		
		virtual void setProbabilistic (DOMProbabilistic* parent);
		
		
		virtual char* getTypeName ()
		{
			return name;
		}
};

#endif
