/*
 *  DOMConditionVisitor.h
 *  
 *
 *  Created by Owen Thomas on 24/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef dom_condition_visitor
#define dom_condition_visitor

class DOMInternalCondition;
class DOMFunctionCondition;
class DOMPredicateCondition;

/**
 * Implementation of visitor design pattern for DOMConditions hierarchy.
 *
 * A class wishing to traverse, evaluate or manipulate a condition hierachy
 * can implement this method and call visit on a DOMCondition.
 * 
 */
class DOMConditionVisitor {

	public:
		
                /**
                 * shut up compiler warnings
		 * @author doug
		 */
                virtual ~DOMConditionVisitor() {};

		/**
		 * An internal condition has been encountered.  
		 * @return true if condition still met
		 */
		virtual bool visitInternal (DOMInternalCondition* internal) = 0;
		
		/**
		 * A function condition has been encountered.
		 * @return true if function true
		 */
		virtual bool visitFunction (DOMFunctionCondition* function) = 0;
		
		/**
		 * A predicate condition has been encountered.
		 * @return true if condition still met
		 */
		virtual bool visitPredicate (DOMPredicateCondition* predicate) = 0;
};
#endif
