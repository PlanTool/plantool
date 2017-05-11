/* plan.h */

#ifndef __PLAN_H
#define __PLAN_H

/* Selectors */

#define LinearPlanTLForm(lp)		((lp)?(lp)->pcTLForm:NULL)
#define LinearPlanTime(lp)			((lp)?(lp)->dfTime:0.0)
#define LinearPlanCost(lp)			((lp)?(lp)->dfCost:0.0)
#define LinearPlanLength(lp)		((lp)?(lp)->nLength:0)
#define LinearPlanParent(lp)		((lp)?(lp)->plpParent:NULL)

/* Modifiers */

#define SetLinearPlanTLForm(lp,val)			((lp)->pcTLForm=(val))
#define SetLinearPlanTime(lp,val)			((lp)->dfTime=(val))
#define SetLinearPlanCost(lp,val)			((lp)->dfCost=(val))
#define SetLinearPlanLength(lp,val)			((lp)->nLength=(val))
#define SetLinearPlanPrevious(lp,val)		((lp)->plpParent=(val))

/* global data */

extern SEARCHRESULT srSearchResult;		/* result of last planning session */
extern double dfPddlTime;				/* pddl planning time */
extern LINEARPLANP plpSuffixTail;		/* pointer to final world (macro expansion) */
extern double dPlanStopTime;                           /* Planner Stop Time */


/* global function prototypes */

BOOL TLPlanner
(
	LINEARPLANP plpStartLinearPlan,
	CELLP pcTLForm,
	SEARCHP pfSearch,
	CELLP pcHeuristic,
	CELLP pcPriority,
	BINDINGP pbBindings
);

BOOL PlanClassicEqQ
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
);
BOOL PlanExtendedEqQ
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
);
LINEARPLANP PlanExtend
(
	LINEARPLANP plpNew,					/* new plan */
	CELLP pcTLNew,						/* new temporal control formula */
	CELLP pcCCNew,						/* new current control formula */
	LINEARPLANP plpPlan,				/* progenitor plan */
	BINDINGP pbBindings					/* bindings list */
);
LINEARPLANP PlanExtendNoHeuristic
(
	LINEARPLANP plpNew,					/* new plan */
	CELLP pcTLNew,						/* new temporal control formula */
	CELLP pcCCNew,						/* new current control formula */
	LINEARPLANP plpPlan,				/* progenitor plan */
	BINDINGP pbBindings					/* bindings list */
);
int PlanCount
(
	LINEARPLANP plpPlan
);
LINEARPLANP CopyLinearPlan
(
	LINEARPLANP plp
);
void MarkLinearPlan
(
	LINEARPLANP plpPlan
);
void PrintPddlPlan
(
	LINEARPLANP plpSuccessors			// plan list
);
void PrintPddl3Plan
(
	LINEARPLANP plpSuccessors,			// plan list
	BINDINGP plpBindings, 
	double dExtraTime,                               // time to be added to the total time
	double dParseTime                               // parsing time
);

LINEARPLANP FollowLinearPlan
(
	LINEARPLANP plpPlan					/* list of plans */
);

BOOL PlanGoalQ
(
	LINEARPLANP plpPlan
);

#endif /* __PLAN_H */






