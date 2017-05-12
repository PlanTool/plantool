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
 * $Id: BrazilState.cpp 107 2006-07-27 03:31:12Z owen $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 * 
 */

#ifndef _BrazilStateHelpers__h
#define _BrazilStateHelpers__h

#include"DOMDuration.h"
#include"DOMAtomicEffectVisitor.h"
#include"DOMConditionVisitor.h"
#include"DOMFunctionExpression.h"
#include"DOMOutcome.h"

#include"BrazilState.h"

/**
 * This class has got some slightly higher level routines that use
 * state information, but do not effect the alter the state
 * directly. Could easily be in BrazilState, but separated out just to
 * make it clear what the real elements of state are.
 */
class BrazilStateHelpers : public BrazilState {

 protected:
    
    // Remember what the goal condition is for isGoal()
    DOMCondition* goalCondition;
    
    // See .cpp for comments.
    
    BrazilStateHelpers(DOMProblem* problem);
    
    bool isActionEligible(DOMAction* action);

    bool isGoal();

    time_t sampleTime(DOMDuration* duration, bool median);

    bool checkEffects(DOMEffectSet* e);
    bool checkOutcomeEffects(DOMOutcome::Effects effectSets);
    bool isUsefulAction(DOMAction* a);
    
    /**
     * Visitor implementation for checking whether effects
     * of a task are useful in the current state.
     * An effect is useful if it is not already in place.
     * Any function makes the task useful it's generally
     * not possible to determine if a function is 
     * useful (except in the stupid case where it does nothing).
     */
    class EffectChecker: public DOMAtomicEffectVisitor {
    private:
	BrazilState* state;
    public:
	EffectChecker(BrazilState* bs) : state(bs) {};
	virtual ~EffectChecker() {};
	virtual bool visitFunctionEffect(DOMFunctionEffect*);
	virtual bool visitPredicateEffect(DOMPredicateEffect*);
    } effectChecker;


   /**
     * Visitor implementation for checking whether conditions
     * are met.
     */
    class ConditionChecker : public DOMConditionVisitor {
    private:
	BrazilStateHelpers* state;
    public:
	ConditionChecker(BrazilStateHelpers* bs) : state(bs) {};
	virtual ~ConditionChecker() {};
	virtual bool visitInternal (DOMInternalCondition* internal);
	virtual bool visitFunction (DOMFunctionCondition* function);
	virtual bool visitPredicate (DOMPredicateCondition* predicate);
    } conditionChecker;


 public:
    
    /**
     * Visitor implementation for checking whether conditions
     * are met.
     */
    class ExpressionEvaluator : public DOMFunctionExpressionVisitor {
    private:
	BrazilStateHelpers* state;
    public:
	ExpressionEvaluator(BrazilStateHelpers* bs) : state(bs) {};
	virtual ~ExpressionEvaluator() {};
	virtual double visitFunction(DOMFunctionExpressionFunction* resource);
	virtual double visitInternal(DOMFunctionExpressionInternal* internal);
	virtual double visitValue(DOMFunctionExpressionValue* value);
    } expressionEvaluator;

};

#endif
