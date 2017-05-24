#ifndef __INTERNAL_H
#define __INTERNAL_H

/* Internal.h */

/* global structure definitions */

typedef struct rng
{
	char **ppsName;						/* generator name */
	int (*pfRNG)(void);					/* random number generator */
	void (*pfSeed)(int);				/* seed routine */
	double dfMaxValue;					/* maximum seed/value */
}RNG, *RNGP;

/* global data */

extern RNG arRNGTable[];				/* random number generator table */
extern RNGP prCurrentRNG;				/* pointer to current random number generator */

//extern double dfGeneratorTime;

/* global function prototypes */

CELLP ComputePlus
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMinus
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMultiply
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDivide
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMod
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMax
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMin
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeExpt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeSqrt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeAbs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeExp
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeLog
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeRand
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeRandom
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSeed
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeRound
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeInt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeFloor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeCeil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

BOOL EvalGt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateGt
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL EvalGe
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateGe
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL EvalLt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateLt
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL EvalLe
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateLe
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);

BOOL GeneratePosInt
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan, 			/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateLtPosInt
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL EvalIsBetween
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateIsBetween
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateNearestFirst
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateNearestFirstEx
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateClosestFirst
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateClosestFirstEx
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateLowestFirst
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL GenerateAllPairsShortestPath
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);
BOOL EvalInTheSet
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL GenerateInTheSet
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
);

#endif /* __INTERNAL_H */
