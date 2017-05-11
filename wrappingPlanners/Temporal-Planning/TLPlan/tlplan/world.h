#ifndef __WORLD_H
#define __WORLD_H

/* world.h */

/* Accessors */

#define LinearPlanWorld(pwa)					((pwa)->apbtWorld)
#define LinearPlanHWorld(pwa)					((pwa)->aphbtWorld)
#define LinearPlanActionName(pwa)				((pwa)->pcActionName)
#define LinearPlanActionDuration(pwa)			((pwa)->dfActionDuration)
#define LinearPlanActionCost(pwa)				((pwa)->dfActionCost)
#define LinearPlanActionPriority(pwa)			((pwa)->dfActionPriority)

/* Modifiers */

#define SetLinearPlanWorld(pwa,val)				((pwa)->apbtWorld=(val))
#define SetLinearPlanActionName(pwa,val)		((pwa)->pcActionName=(val))
#define SetLinearPlanActionDuration(pwa,val)	((pwa)->dfActionDuration=(val))
#define SetLinearPlanActionCost(pwa,val)		((pwa)->dfActionCost=(val))
#define SetLinearPlanActionPriority(pwa,val)	((pwa)->dfActionPriority=(val))

#define GetSymbolArgs(apbt,n)					(apbt[n])
#define PutSymbolArgs(apbt,n,val)				(apbt[n]=(val))

/* global data */

extern int nWorldNumber;				/* world counter */
extern LINEARPLANP plpGoalWorld;		/* the goal world */

extern double dEvalTotalCost; /* the cost of evaluating a formulae,
				 only used by heuristics */

/* global function prototypes */

BTREEP *MakeWorld(void);
HBTREEP *MakeWorld_H(void);
LINEARPLANP MakeWorldAction
(
	BTREEP *plWorld,
	CELLP pcActionName,
	double dfActionDuration,
	double dfActionCost,
	double dfActionPriority
);
LINEARPLANP MakeWorldAction_H
(
	HBTREEP *plWorld,
	CELLP pcActionName,
	double dfActionDuration,
	double dfActionCost,
	double dfActionPriority
);
void MarkWorld
(
	BTREEP *apbtWorld
);
LINEARPLANP CreateInitialWorldAction
(
	CELLP pcDescription,				/* list description of world */
	CELLP pcActionName,					/* the action that got us here */
	double dfActionDuration,			/* total time so far */
	double dfActionCost,				/* total cost so far */
	double dfActionPriority,			/* this world's priority */
	BINDINGP pbBindings					/* bindings */
);
LINEARPLANP CreateGoalWorldAction
(
	CELLP pcDescription,				/* list description of world */
	CELLP pcActionName,					/* the action that got us here */
	double dfActionDuration,			/* total time so far */
	double dfActionCost,				/* total cost so far */
	double dfActionPriority,			/* this world's priority */
	BINDINGP pbBindings					/* bindings */
);
BTREEP *CopyWorld
(
	BTREEP *aplWorld
);
HBTREEP *CopyWorld_H
(
	BTREEP *apbtWorld
);
BOOL PrintWorld
(
	int nFile,							/* file handle */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings					/* bindings */
);
BOOL PrintHWorld
(
	int nFile,							/* file handle */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings					/* bindings */
);
BOOL WorldEqQ
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
);
BOOL EvalAdd
(
	CELLP pcFormula,					/* formula to add */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalAdd_H
(
	CELLP pcFormula,					/* formula to add */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalAdd_FF
(
	CELLP pcFormula,					/* formula to add */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalDel
(
	CELLP pcFormula,					/* formula to delete */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalDel_H
(
	CELLP pcFormula,					/* formula to delete */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalDel_FF
(
	CELLP pcFormula,					/* formula to delete */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalNODel
(
	CELLP pcFormula,					/* formula to delete */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
BOOL EvalUpdateWorld
(
	CELLP pcFormula,					/* formula to delete */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
);
void UpdateWorld(void);
int AddsFromUpdateWorld(void);
int AddsFromUpdateExecute(double,int *);
int AddsFromUpdateWorld_H(double);
int AddsFromUpdateWorld_H_unreleffs(double,int *);
int AddsFromUpdateWorld_FF_unreleffs(double,int *);

BOOL EvalDescPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

BOOL EvalDescPredicateStd /* this is the original version of EvalDescPredicate */
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDescPredicateRelaxedNeg
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDescPredicateStd_H /* this is the original version of EvalDescPredicate */
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDescPredicateRelaxedNeg_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDescPredicateStd_FF /* this is the original version of EvalDescPredicate */
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDescPredicateRelaxedNeg_FF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDescFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDescFunction_Std
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDescFunction_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
LINEARPLANP GetGoalWorld
(
	BOOL bMessage						/* print message flag */
);
void WorldSignature
(
	LINEARPLANP plpPlan					/* plan to signature */
);
CELLP ArgTreeToFormulaList
(
	BTREEP *apbtWorld,					/* array of btrees */
	int nIndex							/* array index */
);
void WorldSizeOf
(
	BTREEP *apbtWorld,
	int *pnSize
);
void InitializeMacroWorld
(
	LINEARPLANP plpLinearPlan,			/* our starting world for macro expansion */
	BINDINGP pbBindings
);


extern CELLP pcAddEnd;
extern CELLP pcDelEnd;


extern LISTP pcFFSupFacts;

#endif /* __WORLD_H */
