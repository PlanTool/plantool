/* iface.h */

#ifndef __IFACE_H
#define __IFACE_H

/* global structures and definitions */

typedef BOOL (*FINALWORLDGOALQP)(CELLP, LINEARPLANP);

/* global data */

extern BOOL (*pfPlanEqQ)(LINEARPLANP, LINEARPLANP);
extern BOOL bVerbose;
extern BOOL bVerboseShowExpandedPlans;
extern BOOL bVerboseShowExpandedWorlds;
extern BOOL bVerboseFFShowRelaxedPlans;
extern BOOL bVerboseFFShowRelaxedWorlds;
extern DECLSPEC FILE *pfTraceStream;
extern int nSearchLimit;
extern double dPlanTimeout;
extern FINALWORLDGOALQP pfFinalWorldGoalQ;
extern CELLP pcTLForm;					/* control formula */
extern CELLP pcGoalFormula;
extern CELLP pcPreferenceFormula;
extern int nTrace;						/* Trace level */
extern CELLP pcInitialFacts;			/* initial facts */
extern CELLP pcInitializationSequence;	/* initialization sequence */
extern CELLP pcPostActionSequence;	/* Post action  sequence */
extern CELLP pcBDF;	/* BDF formula */
extern CELLP pcGoalSequence;			/* goal sequence */
extern CELLP pcHeuristicFn;				/* Heuristic cost function */
extern CELLP pcGoalAddendum;			/* Goal addendum */
extern CELLP pcPriorityFn;				/* priority function */
extern CELLP pcHPreserved;   // heuristically preserved formula
extern CELLP pcMetricFn;    /* the metric function */
extern CELLP pcQualpreferenceValueFn;    /* the qualitative preference value computor for states */
extern CELLP pcOptimisticMetricFn;    /* the optimistic metric function */
extern CELLP pcCountSatPrefsFn;  /* the satisfied preference counting function */
extern CELLP pcTotalPrefsFn;  /* the satisfied preference counting function */
extern CELLP pcPrintWorld;				/* print function */
extern void (*pfProcessGoal)(CELLP, LINEARPLANP, BINDINGP);
extern DECLSPEC jmp_buf *pjbCurrentJump;	/* pointer to current longjmp context */
extern char *psStatisticsFile;			/* statistics output file name */
extern CELLP pcSSForm;					/* search-strategy formula */
extern SEARCHP pfSearchStrategy;		/* search strategy */
extern jmp_buf jbTLPlannerJump;			/* planner longjmp context */
extern LINEARPLANP plpInitialWorld;		/* the initial world */
extern LINEARPLANP plpFinalPlan;		/* the final world (and plan) */
extern CELLP pcPredefinedPlan;			/* predefined plan for follow-predefined-plan */

extern BOOL bAtemporalControl;			/* use atemporal control formula */
extern BOOL bConcurrentPlanning;		/* plan in concurrent mode */
extern BOOL bCycleChecking;				/* enable cycle checking */
extern BOOL bTimingStatistics;			/* enable timing statistics */
extern BOOL bBackTracking;				/* search sibling worlds only */
extern BOOL bPruningAllSuccessors;		/* prune worlds as they are created */
extern int nPddlSupport;				/* enable PDDL (kludge) support */

/* global function prototypes */

void FunctionTracerProlog
(
	LINEARPLANP plpParent,	 			/* parent world */
	BINDINGP pbBindings					/* bindings list */
);
void FunctionTracer
(
	LINEARPLANP plpParent,	 			/* parent world */
	CELLP pcProgressedTLForm,			/* progressed temporal control formula */
	CELLP pcTLForm,						/* unprogressed (parent) temporal control formula */
	CELLP pcCCForm,						/* current control formula */
	LINEARPLANP plpSuccessors,			/* successor worlds */
	BINDINGP pbBindings					/* bindings list */
);
CELLP GetGoal(void);
LINEARPLANP GetInitialPlan(void);
void ResetPrintWorldFn(void);
void ResetTLControl(void);
void MarkDomains(void);
void ProcessExtendedGoal
(
	CELLP pcFormula,					/* temporal goal formula */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL ClassicGoalQ
(
	CELLP pcTLForm,						/* ignored formula */
	LINEARPLANP plpLinearPlan
);
BOOL ExtendedGoalQ
(
	CELLP pcTLForm,
	LINEARPLANP plpLinearPlan
);

DECLSPEC void TermError
(
	char *psMessage,					/* message string */
	CELLP pcFormula,					/* formula to display */
	BINDINGP pbBindings					/* bindings list */
);
BOOL NoEvaluator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL NoIdler
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP NoProgressor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
int NoCurrentor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP NoComputor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL NoGenerator
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP NoCCForm
(
	BOOL bType,
	CELLP pcForm,
	BOOL *pbImmutable
);
CELLP CCNotImplemented
(
	BOOL bType,
	CELLP pcForm,
	BOOL *pbImmutable
);

#endif /* __IFACE_H */
