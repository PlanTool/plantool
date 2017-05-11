/* adl.h */

#ifndef __ADL_H
#define __ADL_H

/* global data */

extern LINEARPLANP plpSuccessors;		/* successor states for an operator */
extern LINEARPLANP plpSuccessorWorld;	/* current successor state */

//extern double dfSuccessorTime;

/* global function prototypes */

BOOL EvalDefADLOperator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
void ADLSplitFormulas
(
	CELLP pcModifiers,					/* list of modifiers to split */
	CELLP *pfAdd,						/* returned list of add modifiers */
	CELLP *pfDel						/* returned list of del modifiers */
);
CELLP MakeModifyWorldForm
(
	BOOL bCopy,							/* copy flag */
	OPERATORP poOperator,				/* operator */
	CELLP pcMod							/* modifier formula */
);
BOOL EvalModifyWorld
(
	CELLP pcFormula,					/* modify world formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current variable bindings */
);
CELLP ProgressModifyWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
int CurrentModifyWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
LINEARPLANP GenerateSuccessors
(
	LINEARPLANP plpStart,				/* initial plan */
	BINDINGP pbBindings
);
//LINEARPLANP OperatorSuccessor
//(
//	OPERATORP po,						/* operator */
//	LINEARPLANP plpLinearPlan,			/* linear plan */
//	BINDINGP pbBindings					/* bindings */
//);

#endif /* __ADL_H */
