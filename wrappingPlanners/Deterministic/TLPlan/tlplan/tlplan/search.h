/* search.h */

#ifndef __SEARCH_H
#define __SEARCH_H

/* global data */

extern BOOL bUserAbort;					/* user abort */
extern TIMESTAT tsTimers;				/* timer statistics */

extern LINEARPLANP plpOpen;				/* list of worlds to be searched */
extern LINEARPLANP plpClosed;			/* list of worlds already searched */

/* global function prototypes */

char *BreadthFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),
	CELLP pcCost,
	CELLP pcPriority,
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *BreadthFirstPriority
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),
	CELLP pcCost,
	CELLP pcPriority,
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *BestFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),
	CELLP pcCost,
	CELLP pcPriority,
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *DepthFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *DepthFirstNoBacktracking
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparitor */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *DepthFirstPriority
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),
	CELLP pcCost,
	CELLP pcPriority,
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *DepthBestFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* search terminator */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
	CELLP pcHeuristic,					/* heuristic cost function */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);
char *FollowPredefinedPlan
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparitor */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
);

double CalculateHeuristic
(
	CELLP pcHeuristic,					/* heuristic cost function */
	LINEARPLANP plpPlan,				/* plan */
	BINDINGP pbBindings					/* bindings list */
);
double CalculatePriority
(
	CELLP pcPriority,					/* cost function */
	LINEARPLANP plpPlan,				/* plan */
	BINDINGP pbBindings
);
LINEARPLANP PlanSuccessorFn
(
	CELLP pcTLForm,
	CELLP pcCCForm,
	LINEARPLANP plpPlan,
	BINDINGP pbBindings
);
LINEARPLANP CycleFilter
(
	LINEARPLANP plpNewState
);
void PrintTimers
(
	FILE *fs							/* file stream */ 
);
int MergeCompareFn
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
);

#endif /* __SEARCH_H */
