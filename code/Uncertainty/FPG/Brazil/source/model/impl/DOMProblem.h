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
 *  DOMProblem.h
 *  
 *
 *  Created by Owen Thomas on 18/05/06.
 *  
 *
 */

#ifndef dom_problem
#define dom_problem

#include <set>
#include <map>

#include <xercesc/dom/DOM.hpp>

#include "DOMWrapper.h"
#include "DOMConditionBase.h"
#include "DOMPredicate.h"
#include "DOMFunction.h"
#include "DOMRootCondition.h"
#include "DOMDomain.h"

class BrazilState;
/**
 * A representation of a Brazilian Problem.
 *
 * An initial set of true Predicates,
 * and a goal condition.
 */
class DOMProblem : public DOMWrapper
{
	private:
		DOMDomain* domain;
		
		//Initially valid predicates and initial function
		//values.
		set<DOMPredicate*> initPredicates;
		map<DOMFunction*, double> initFunctions;
		
		//Element pointing to <init><predicates>
		DOMElement* initPredicatesElement;
		
		//pointing to <init><functions>
		DOMElement* initFunctionsElement;
		
		//Goal.
		DOMRootCondition* goal;
		
		//Initial State.
		BrazilState* initialState;
		
		XMLCh* initString;
		XMLCh* assignString;
		XMLCh* predicate;
		XMLCh* predicates;
		XMLCh* function;
		XMLCh* functions;
		XMLCh* value;
		XMLCh* number;
		XMLCh* goalString;
		XMLCh* name;
		
		//Internal construction methods.
		
		void createInit ();
		void createGoal ();
		void createInitialState ();
		
	public:
		/**
		* Create a Brazil Problem with the specified elements
		* under the specified domain.
		*
		* Each predicate defined in the goal and init block must
		* exist domain.
		*
		* A Problem has the following general structure:
		*
		* <problem name>
		*
		*    <init> (1,1)
		*       <predicate name> *
		*
		*    <goal> (1,1)
		*
		* </problem>
		*
		*
		* The DOM tree rooted at <goal> must be understood by
		* DOMRootCondition.
		*/
		DOMProblem (DOMElement* element, DOMDomain& domain);
		
		virtual ~DOMProblem ();
		
		virtual DOMDomain* getDomain ()
		{
			return domain;
		}
		
		virtual char* getName ()
		{
			return getAttribute ("name");
		}
		
		virtual DOMRootCondition* getGoal ()
		{
			return goal;
		}

		virtual set<DOMPredicate*>& initiallyValidPredicates () {
			return initPredicates;
		}
		
		/**
		 * Return the initial Function Values.
		 */
		 
		virtual map<DOMFunction*, double>& initialFunctionValues ();
		
		virtual void setFunctionValue (DOMFunction*, double value);
		
		virtual bool isInitial (DOMPredicate* predicate) {
			return initPredicates.end() != initPredicates.find(predicate);
		}
		
		virtual void setInitial (DOMPredicate* predicate, bool initiallyValid = true) {
			if(initiallyValid && initPredicates.end() == initPredicates.find (predicate)) {
				initPredicates.insert (predicate);
				
				DOMElement* initPredicate = (DOMElement*)
					predicate->getDOMElement()->cloneNode (true);
				
				initPredicatesElement->appendChild (initPredicate);
				
			}
			else if (!initiallyValid && initPredicates.end() != 
				initPredicates.find (predicate)) {
			
				initPredicates.erase (predicate);
				XMLCh* name = XMLString::transcode ("name");
				
				DOMNodeList* initChildren = 
					initPredicatesElement->getElementsByTagName (this->predicate);
				
				for(unsigned int i = 0; i < initChildren->getLength (); i++) {
					DOMElement* current = (DOMElement*)initChildren->item (i);
					
					if(XMLString::equals (current->getAttribute (name), 
						predicate->getDOMElement()->getAttribute (name))) {
						
						initPredicatesElement->removeChild (current);
						current->release ();
					}
				}
			}
		}
		
		virtual BrazilState* getInitialState () {
			return initialState;
		}
};

#endif
