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
 *  DOMFunctionExpression.h
 *  
 *
 *  Created by Owen Thomas on 13/07/06.
 *  
 *
 */

#ifndef dom_function_expression
#define dom_function_expression

#include "DOMWrapper.h"
#include "DOMFunction.h"

class DOMDomain;
class DOMFunctionExpressionInternal;
class DOMFunctionExpressionValue;
class DOMFunctionExpressionFunction;

/**
 * Subclass this to provide printing, evaluation or whatever functionality
 * you want... This keeps application dependent functionality out of the
 * the representation.
 */
class DOMFunctionExpressionVisitor {
	public:
                virtual ~DOMFunctionExpressionVisitor() {};
		virtual double visitInternal (DOMFunctionExpressionInternal* internal) = 0;
		virtual double visitValue (DOMFunctionExpressionValue* value) = 0;
		virtual double visitFunction (DOMFunctionExpressionFunction* function) = 0;
};

/**
 * Representation of a function expression, i.e. an expression that given
 * values for functions involved can be evaluated to produce an integer.
 * 
 * For example: 5 + (10 * fuel / fuel-price)
 */
class DOMFunctionExpression : public DOMWrapper {
	
		
	public:
		
		/**
		 * Call a concrete subclass visit implementation on visitor.
		 */
		virtual double visit (DOMFunctionExpressionVisitor& visitor) = 0;
		
		
		/**
		 * Static factory method that returns a new DOMFunctionExpression concrete
		 * subclass instance based on the element and domain, or NULL if no expression
		 * can be created.
		 */
		static DOMFunctionExpression* createExpression (DOMElement& element, DOMDomain& domain);
};


/**
 * An internal, i.e. plus, minus, multiply, divide, function operation.
 */
class DOMFunctionExpressionInternal : public DOMFunctionExpression {
	public:
		enum Operator {add, subtract, multiply, divide};
		
	private:
		DOMFunctionExpression* lhs;
		DOMFunctionExpression* rhs;
		Operator op;
		DOMDomain* domain;
		
	
		/**
		 * Return the operator of this function expression.
		 */
		Operator createOperator ( );
		
		/**
		 * Read a child node, potentially a left hand side or rhs expression
		 * and return true if there is another function expression to be read.
		 */
		bool readChildNode (DOMNode* node);
		
		/**
		 * Return a string representation of the current operator.
		 */
		char* writeOperator ( );
	
	public:
	
		/**
		 * Returns true iff element can be read as an internal function expression.
		 */
		static  bool canRead (DOMElement& element) {
			char* tagName = XMLString::transcode (element.getTagName());
			bool returnValue = XMLString::equals (tagName, "functionExpression");
			XMLString::release (&tagName);
			return returnValue;
		}

		
		/**
		 * Construct an internal function expression represention over element.
		 */
		DOMFunctionExpressionInternal (DOMElement& element, DOMDomain& domain);
		
		/** 
		 * Construct a new internal function expression and the underlying dom
		 * element.
		 */
		DOMFunctionExpressionInternal (DOMDocument& document, Operator op, 
			DOMFunctionExpression& lhs, DOMFunctionExpression& rhs);
		
		virtual ~DOMFunctionExpressionInternal () {
			delete lhs;
			delete rhs;
			if(node->getParentNode()) {
				node->getParentNode()->removeChild (node);
			}
			node->release();
		}
		
		virtual double visit (DOMFunctionExpressionVisitor& visitor) {
			return visitor.visitInternal (this);
		}
		
		virtual void setOperator (Operator);
		virtual Operator getOperator ();
		
		virtual void setLeftHandSide (DOMFunctionExpression& lhs);
		virtual void setRightHandSide (DOMFunctionExpression& rhs);
		
		virtual DOMFunctionExpression* getLeftHandSide ();
		virtual DOMFunctionExpression* getRightHandSide ();
		
};

/**
 * A leaf node in the expression, representing an integer value.
 */
class DOMFunctionExpressionValue : public DOMFunctionExpression {
	public:
		static bool canRead (DOMElement& element) {
			char* tagName = XMLString::transcode (element.getTagName());
			bool returnValue = XMLString::equals (tagName, "number");
			XMLString::release (&tagName);
			return returnValue;
		}
	
		DOMFunctionExpressionValue (DOMElement& element);
		DOMFunctionExpressionValue (DOMDocument& document, double value);
		
		virtual ~DOMFunctionExpressionValue () {
			if(node->getParentNode()) node->getParentNode()->removeChild (node);
			node->release();
		}
		
		virtual double visit (DOMFunctionExpressionVisitor& visitor) {
			return visitor.visitValue (this);
		}
		
		virtual void setValue (double value);
		
		virtual double getValue ();
}; 

/**
 * A leaf node in the epxression, representing a function value.
 */
class DOMFunctionExpressionFunction : public DOMFunctionExpression {
	private:
		DOMFunction* function;
	public:
		static bool canRead (DOMElement& element) {
			char* tagName = XMLString::transcode (element.getTagName());
			bool returnValue = XMLString::equals (tagName, "function");
			XMLString::release (&tagName);
			return returnValue;
		}
	
		DOMFunctionExpressionFunction (DOMElement& element, DOMDomain& domain);
		DOMFunctionExpressionFunction  (DOMFunction& function);
		
		virtual ~DOMFunctionExpressionFunction () {
			if(node->getParentNode()) node->getParentNode()->removeChild (node);
			node->release();
		}
		
		virtual double visit (DOMFunctionExpressionVisitor& visitor) {
			return visitor.visitFunction (this);
		}
		
		virtual void setFunction (DOMFunction& function);
		virtual DOMFunction* getFunction ();
};

#endif
