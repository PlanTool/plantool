/*
 *  DOMFunctionCondition.h
 *  
 *
 *  Created by Owen Thomas on 14/07/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
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
