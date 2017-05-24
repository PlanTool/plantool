/* adl.h */

#ifndef __ADL_H
#define __ADL_H

#include <values.h>


/* global data */

extern LINEARPLANP plpSuccessors;		/* successor states for an operator */
extern LINEARPLANP plpLastSuccessor;			/* pointer to end of successor list */
extern LINEARPLANP plpSuccessorWorld;	/* current successor state */

extern LINEARPLANP plpNegPlan;  /* dummy plan for world of negated facts */
extern LINEARPLANP plpLevel0Plan;  /* dummy plan for world of negated facts */
extern BTREEP *apbtNegWorld; /* pointer to the negated world */
extern HBTREEP *aphbtNegWorld; /* pointer to the h-negated world */

extern BOOL bRelaxedModeNeg; /* whether or not we are in relaxed considering negations */
extern BOOL bUnderNot; /* whether or not while evaluating a formula, we've seen a not */
extern BOOL bUnderRelaxedModifyWorld;   /* true whenever evaluating under a relaxed modify world operator */
extern BOOL bUseHWorld;
extern BOOL bUseHWorldFF;
extern BOOL bMutexMode;
extern BOOL bHSPIgnoreOperator; /* tells whether the operator currently beign evaluated should be ignored by the HSP heuristic */
extern BOOL bExtractingRelaxedPlan; /* whether or not we are extracting a relaxed plan, this variable 
			      affects the behaviour of EvalDescPredicate */
extern BOOL bVerifyingPreservedProperty; /* whether or not we are verifying a preserved property */

extern BOOL nOcclusionPenalty; // the occlusion penalty when computing occlusions
extern BOOL nMutexPenalty; // the occlusion penalty when computing occlusions
extern BOOL bComputingOcclusions;  /* whether or not we are computing occlusions */
extern BOOL bOcclusions;   // whether or not occlusions will be computed


/* FF variables */
extern BOOL bFFSimplifyRelaxedPlan;      // whether or not to do relaxed plan simplification
extern int nFFDepth;       /* depth at which a fact was added  */
extern int nFFOperNumber;  /* number of the operator that added a fact */
extern int nFFOperInstance;/* number of the instantiation of an operator that added a fact */
extern CELLP pcFFActionInstance; /* pointer to the actual action instance that added a fact */
extern BINDINGP pbCurrentActionBindings; /* Bindings for the current action in execution */
extern OPERINSTANCEP poiCurrentInstance;  // current instance

extern double dMetricMultiplier;
extern double dBestMetricMultiplier;
extern double dDiscountedMetricMultiplier;
extern double dOptimisticMetricMultiplier; 
extern double dPreferenceMultiplier; 
extern double dGoalOcclusionMultiplier;   // Goal occlusion penalty multiplier
extern double dPrecondOcclusionMultiplier;   // Penalty occlusion penalty multiplier
extern double dGoalMultiplier;                             
extern double dHeuristicExponent;          // heuristic exponent 
extern double dMetricDiscountFactor;             // metric discount factor

extern int nFixPoints;   // number of world fixed points (i.e. not including unrelaxed fluents) seen so far in the computation of the heuristic
extern int nRealFixPoints;   // number of fixed points (i.e. including relaxed fluents) seen so far in the computation of the heuristic

extern int nDiscountedMetricPriority;       // discounted metric priority
extern int nOptimisticMetricPriority;       // optimistic metric priority
extern int nBestRelaxedMetricPriority;      // best relaxed metric priority
extern int nPreferenceDistancePriority;     // preference distance priority
extern int nMetricPriority;                 // priority of the world metric

/* Qualitative preferences stuff */

extern int nQualPreferenceNumber;




//extern double dfSuccessorTime;

/* global function prototypes */

BOOL EvalDefADLOperator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefADLHOperator
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
BOOL EvalModifyWorldRelaxed
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


CELLP ComputeGoalDistance
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

CELLP ComputeGoalDistanceNeg
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

CELLP ComputeGoalDistanceQual
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);


CELLP ComputeGoalDistanceHspUnreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGoalDistanceFFUnreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGoalDistanceFFPDDLPrefs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDescPredicate_Extract_Relaxed_Plan 
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

void ResetGoalDistanceStatics();

extern int njbEval;  // for debugging. consider deleting me.
extern int njbCallsEvalAdd;

#define INFTY MAXDOUBLE

#endif /* __ADL_H */
