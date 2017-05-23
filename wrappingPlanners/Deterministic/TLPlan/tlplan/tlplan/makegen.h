#ifndef __MAKEGEN_H
#define __MAKEGEN_H

/* makegen.h */

BOOL GenerateDescPredicate
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateDefGenerator
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
//BOOL GenerateFunction
//(
//	CELLP pcGenLit,
//	void **ppvContext,
//	CELLP pcVars,
//	LINEARPLANP plpLinearPlan,
//	BINDINGP pbBindings
//);
BOOL GenerateGoal
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateCurrent
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GeneratePrevious
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateEq
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GeneratePermute
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

#endif /* __MAKEGEN_H */
