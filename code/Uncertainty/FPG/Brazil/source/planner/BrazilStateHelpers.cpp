

#include"BrazilSetup.h"

DBG_MODULE(DBG_HELPERS);

#include"DOMProblem.h"
#include"DOMAction.h"
#include"DOMInternalCondition.h"
#include"DOMRootCondition.h"
#include"DOMFunctionCondition.h"
#include"DOMPredicateCondition.h"
#include"DOMOutcome.h"
#include"DOMAtomicEffect.h"
#include"DOMFunctionEffect.h"
#include"DOMPredicateEffect.h"

#include"BrazilEvent.h"
#include"BrazilState.h"
#include"BrazilStateHelpers.h"
#include"PlanningException.h"

/**
 * Just remeber the goal state and pass onto BrazilState constructor.
 * Also initialise the effectChecker embedded class.
 */
BrazilStateHelpers::BrazilStateHelpers(DOMProblem* problem) : 
    BrazilState(problem), 
    effectChecker(this), 
    conditionChecker(this), 
    expressionEvaluator(this) {
    
    goalCondition = problem->getGoal();
    // Planning isn't going to get very far if there isn't a goal.
    if (goalCondition == NULL) throw PlanningException("No goal state defined\n");
}


/**
 * Is the current state a goal state?
 */
bool BrazilStateHelpers::isGoal() {

    success = goalCondition->visit(&conditionChecker);

    return success;

}


/**
 * Figure out if an action is eligible to start given current state
 */
bool BrazilStateHelpers::isActionEligible(DOMAction* action) {
    
    // Start with preconditions
    if (DBG(4)) BOUT<<"action="<<action->getName()<<endl;

    
    // Check that this action hasn't been manually scheduled for the future
    if (queuedToStart.find(action) != queuedToStart.end()) {
	cout << "queued to start: " << action->getName() << endl;
	return false;
    }
	
	
    
    // Check standard preconditions
    assert(action->getPrecondition() != NULL);    
    if (!action->getPrecondition()->visit(&conditionChecker)) return false;
    
    // Precondition test passed, now try overall conditions
    if (!action->getOverallCondition()->visit(&conditionChecker)) return false;

    // Now check that the action does something useful
    if (!isUsefulAction(action)) return false;
    
    return true;
    
}


/**
 * Recursive part of condition test, used for eligibility and goal state test
 * @return true if conditions met.
 */
bool BrazilStateHelpers::ConditionChecker::visitInternal(DOMInternalCondition* condition) {
    
    // Empty condition passes as true.
    bool result=true;

    // Empty condition used to be NULL. Could still be for overall.
    if (condition == NULL) return result;
    
    for (DOMCondition::ConditionCIt i=condition->getChildren().begin(); i != condition->getChildren().end(); i++) {
	
	result = (*i)->visit(this);
	if (condition->isConjunctive()) {
	    if (!result) break;
	}
	else if (result) break;
	
    }
    
    if (DBG(5)) BOUT<<"result before negation="<<result<<endl;
    
    // Test for negation at this level
    if (condition->isNegated()) return !result;
    return result;
    
} 


/**
 * Leaf in condition tree, must be 
 * @param c DOMPredicateCondition
 * @return true if predicate condition matches
 */
bool BrazilStateHelpers::ConditionChecker::visitPredicate(DOMPredicateCondition* c) {

    
    return (state->getPredicateValue(c->getPredicate()) == !c->isNegated());
       
}


/**
 * Accept node in a condition that represents a function condition and evaluate it.
 */
bool BrazilStateHelpers::ConditionChecker::visitFunction(DOMFunctionCondition* fc) {

    double lhs = state->getResourceLevel(fc->getFunction());
    double rhs = fc->getExpression()->visit(state->expressionEvaluator);

    if (DBG(5)) BOUT<<"checking functional condition "<<lhs<<","<<rhs<<endl;

    switch (fc->getOperator()) {
        case DOMFunctionCondition::equals:
	    return (lhs == rhs);
        case DOMFunctionCondition::greaterThan:
	    return (lhs > rhs);
        case DOMFunctionCondition::lessThan:
	    return (lhs < rhs);
        case DOMFunctionCondition::greaterThanOrEqualTo:
	    return (lhs >= rhs);
        case DOMFunctionCondition::lessThanOrEqualTo:
	    return (lhs <= rhs);
        default:
	    throw PlanningException("Unknown operator in BrazilStateHelpers::visitFunction()");
    }

    return false; // unreachable
}



/**
 * All the durations know how to sample themselves.
 * @param duration duration object to sample from
 * @param median if true, just return means.
 */
time_t BrazilStateHelpers::sampleTime(DOMDuration* duration, bool median) {

    time_t timeToOccur = getTime();
    time_t sample;

    // Comments in docs say duration can be NULL if no delay
    if (duration != NULL) {
	sample = median?duration->getMean():duration->getSample();
	if (DBGM(5, DBG_SAMPLER)) BOUT<<"Sample: "<<sample<<endl;
	timeToOccur += sample;
    }
    return timeToOccur;

}


/**
 * Check all instant effects, task effects, and outcome effects for something 
 * useful. If there's nothing useful return false and this action will not
 * be considered eligible. The presence of any function change will be considered
 * useful irrespective of current function levels.
 * @return false if this task has no useful effects right now.
 */
bool BrazilStateHelpers::isUsefulAction(DOMAction* a) {

    // TODO perform check on instant effects

    // Perform check on task effects
    if (checkEffects(a->getEffectSet())) return true;

    // Perform check on all outcome effects
    if(a->getProbabilisticChildren().size() > 0) {
	DOMOutcome::Outcomes outcomes = (*(a->getProbabilisticChildren().begin()))->getOutcomes();
	for (DOMOutcome::OutcomesIt o = outcomes.begin(); o != outcomes.end(); o++) {
	    if (checkOutcomeEffects(o->first->getEffects())) return true;
	}
    }


    if (DBG(4)) BOUT<<"Action "<<a->getName()<<" not useful\n";
    return false;
}


/**
 * Scan over all the EffectSets in an outcome, checking for usefulness.
 * Outcomes have an effectSet for effects at each delay time.
 * @return Return false if all effect sets are useless.
 */
bool BrazilStateHelpers::checkOutcomeEffects(DOMOutcome::Effects effectSets) {

    for (DOMOutcome::EffectsIt it = effectSets.begin(); it != effectSets.end(); it++) {
	if (checkEffects(*it)) return true;
    }

    return false;

}


/**
 * Scan over all the effects of a task or outcome. Return false if all predicates are already in place.
 * True if there is some predicate not already in place.
 */
bool BrazilStateHelpers::checkEffects(DOMEffectSet* e) {

    DOMEffectSet::AtomicEffects ae = e->getAtomicEffects();

    for (DOMEffectSet::AtomicEffectsIt it=ae.begin(); it != ae.end(); it++) {
	if ((*it)->visit(&effectChecker)) return true;
    }
    return false;

}


/**
 * Returns false if desired predicate effect already in place.
 */
bool BrazilStateHelpers::EffectChecker::visitPredicateEffect(DOMPredicateEffect* pe) {
    return !(state->getPredicateValue(pe->getPredicate()) == !pe->isNegated());
}


/**
 * Functions are always considered useful.
 * @return true, always.
 */
bool BrazilStateHelpers::EffectChecker::visitFunctionEffect(DOMFunctionEffect*) {
    return true;
}


/**
 * Return the value of visiting an internal node in a function expression.
 * @return intermediate value from expression
 */
double BrazilStateHelpers::ExpressionEvaluator::visitInternal(DOMFunctionExpressionInternal* internal) {

    double lhs = internal->getLeftHandSide()->visit(state->expressionEvaluator);
    double rhs = internal->getRightHandSide()->visit(state->expressionEvaluator);

    switch(internal->getOperator()) {
        case DOMFunctionExpressionInternal::add:
	    return lhs + rhs;
        case DOMFunctionExpressionInternal::subtract:
	    return lhs - rhs;
        case DOMFunctionExpressionInternal::multiply:
	     return lhs * rhs;
        case DOMFunctionExpressionInternal::divide:
	    return lhs / rhs;
        default:
	    throw PlanningException("Unknown operator in BrazilStateHelpers::ExpressionEvaluator::visitInternal()");
    }	
}


/**
 * Return the value of visiting a named function leaf in a function expression.
 * @return current resource levels for function name
 */
double BrazilStateHelpers::ExpressionEvaluator::visitFunction(DOMFunctionExpressionFunction* function) {

    return state->getResourceLevel(function->getFunction());

}


/**
 * Return the value of visiting a constant.
 * @return returns the constant
 */
double BrazilStateHelpers::ExpressionEvaluator::visitValue(DOMFunctionExpressionValue* value) {

    return value->getValue();

}
