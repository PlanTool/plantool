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
 *  DOMFunctionEffect.h
 *  
 *
 *  Created by Owen Thomas on 14/07/06.
 *  
 *
 */

#ifndef dom_function_effect
#define dom_function_effect

#include "DOMWrapper.h"

#include "DOMFunctionExpression.h"
#include "DOMAtomicEffect.h"

/**
 * An atomic effect modifying a function using a function expression.
 *
 */
class DOMFunctionEffect : public DOMAtomicEffect {
	
	public:
		enum Operator {scaleUp, scaleDown, increase, decrease, assign};

	private:
		DOMFunction* function;
		DOMFunctionExpression* expression;
		Operator op;
		
		DOMElement* functionElement;
		
		/**
		 * Read the operator from element.
		 */
		Operator readOperator ( );
		
		/**
		 * Write the operator op to a character string.
		 */
		char* writeOperator ( );
				
	public:
		
		/**
		 * Construct a function effect from element, as an atomic effect within parent
		 * and drawing function names from domain.
		 */
		DOMFunctionEffect (DOMElement& element, DOMDomain& domain);
		
		/*
		 * Construct a function effect from an existing function and expression.
		 * This creates a new DOMElement to describe the effect from the input domain.
		 */
		DOMFunctionEffect (DOMFunction& function, Operator op,
			DOMFunctionExpression& expression, DOMDocument& doc);
		/**
		 * Deletes the expression and releases the underyling node representing this 
		 * effect.
		 */
		virtual ~DOMFunctionEffect () {
			delete expression;
			if(node->getParentNode()) {
				node->getParentNode()->removeChild (node);
			}
			node->release ();
		}
		
		virtual bool visit (DOMAtomicEffectVisitor* visitor) {
			return visitor->visitFunctionEffect (this);
		}
		virtual Operator getOperator ();
		virtual void setOperator (Operator op);
		
		virtual DOMFunction* getFunction ();
		
		/**
		 * This function effect will not assume ownership of the function.
		 */
		virtual void setFunction (DOMFunction* function);
		
		virtual DOMFunctionExpression* getExpression ();
		
		/** 
		 * This function effect will assume ownership of the expression.
		 */
		virtual void setExpression (DOMFunctionExpression* expression);
		
		/**
		 * Returns true iff element can be read as a function effect.
		 */
		static bool canRead (DOMElement* element);
};

#endif
