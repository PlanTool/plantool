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
 *  DOMConditionVisitor.h
 *  
 *
 *  Created by Owen Thomas on 24/08/06.
 *  
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
