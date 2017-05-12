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
 *  DOMFunctionCondition.h
 *  
 *
 *  Created by Owen Thomas on 14/07/06.
 *  
 *
 */

#ifndef dom_function_condition
#define dom_function_condition

#include "DOMFunction.h"
#include "DOMFunctionExpression.h"
#include "DOMConditionBase.h"

/**
 * An implementaiton of DOMCondition for defining a condition over
 * the relationship between a function and function expression.
 */
class DOMFunctionCondition : public DOMCondition {
	public:
		enum Operator {equals, greaterThan, lessThan, greaterThanOrEqualTo, lessThanOrEqualTo};
	
	private:
		char* writeOperator ( );
		Operator readOperator ( );
		DOMElement* functionElement;
		DOMElement* functionExpressionElement;
		
		Operator op;
		DOMFunction* function;
		DOMFunctionExpression* expression;
		
		//Element that defines the function.
		DOMElement* internalFunctionElement;
	
	public:
		DOMFunctionCondition (DOMElement& element, DOMDomain& domain);
		
		DOMFunctionCondition (DOMFunction* function, Operator op, 
			DOMFunctionExpression* expression, DOMDocument* doc);
		
		/**
		 * Deletes the expression and releases the underyling node representing this 
		 * condition.
		 */
		virtual ~DOMFunctionCondition () {
			delete expression;
			if(node->getParentNode()) {
				node->getParentNode()->removeChild (node);
			}
			node->release ();
		}
	
		virtual DOMFunction* getFunction ();
		virtual void setFunction(DOMFunction* function);
		
		virtual Operator getOperator ();
		virtual void setOperator (Operator op);
		
		virtual DOMFunctionExpression* getExpression ();
		virtual void setExpression (DOMFunctionExpression* expression);
		
		//
		//DOMCondition methods
		//
		
		/**
		 * Visit this function condition with an application
		 * specific visitor.
		 */
		virtual bool visit (DOMConditionVisitor* visitor) {
			return visitor->visitFunction (this);
		}
		
		virtual char* getOperatorName () {
			return writeOperator ();
		}
		
		virtual char* getValue () {
			return function->getName ();
		}
		
		virtual bool isInternal () {
			return false;
		}
};

#endif
