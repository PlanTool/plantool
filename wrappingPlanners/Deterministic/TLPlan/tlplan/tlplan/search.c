/* search.c

Copyright C, 1996 - 2001	F. Bacchus

Generic Search Algorithms

	a. graph searchers
	b. space efficient depth first search.
	c. iterative deepening search.
*/

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "crchash.h"
#include "eval.h"
#include "formula.h"
#include "heap.h"
#include "iface.h"
#include "makeform.h"
#include "msort.h"
#include "plan.h"
#include "progress.h"
#include "queue.h"
#include "save.h"
#include "search.h"
#include "tlparse.h"
#include "util.h"
#include "world.h"
#include "zone.h"

/* local structures and definitions */

typedef struct PlanList
{
	struct PlanList *pplNext;			/* pointer to next */
	LINEARPLANP *plpPlan;				/* pointer to plan */
}PLANLIST, *PLANLISTP;

/* local function prototypes --------------------------------------------------- */

static LINEARPLANP FilterOldStates
(
	LINEARPLANP plpNewStates,			/* list of new states to filter */
	LINEARPLANP plpOpen,				/* list of open states */
	LINEARPLANP plpClosed,				/* list of closed states */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* test two terminal states */
	CELLP pcHeuristic					/* ignored heuristic cost function! */
);
static LINEARPLANP FilterWorstOldStates
(
	LINEARPLANP plpNewStates,			/* list of new states to filter */
	LINEARPLANP plpOpen,				/* list of open states */
	LINEARPLANP plpClosed,				/* list of closed states */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* test two terminal states */
	CELLP pcHeuristic					/* heuristic cost function */
);
static LINEARPLANP AppendPlans
(
	LINEARPLANP plpX,
	LINEARPLANP plpY
);
//static LINEARPLANP PrependPlans
//(
//	LINEARPLANP plpX,
//	LINEARPLANP plpY,
//	CELLP pcHeuristic
//);
static LINEARPLANP MergePlans
(
	LINEARPLANP plpX,
	LINEARPLANP plpY,
	CELLP pcHeuristic
);
static LINEARPLANP PrioritySuccessorFn
(
	CELLP pcTLForm,
	CELLP pcCCForm,
	LINEARPLANP plpList,
	BINDINGP pbBindings
);
static BOOL PriorityCompareFn
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
);
static LINEARPLANP HeuristicSuccessorFn
(
	CELLP pcTLForm,
	CELLP pcCCForm,
	LINEARPLANP plpList,
	BINDINGP pbBindings
);
static BOOL HeuristicCompareFn
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
);
static void DiscardWorld
(
	LINEARPLANP plp						/* world to discard */
);
//static void PruneOpenList
//(
//	BINDINGP pbBindings
//);
static void PruneList
(
	LINEARPLANP *pplpList,
	BINDINGP pbBindings
);
static BOOL PlanHasCycleQ
(
	LINEARPLANP plpPlan
);
static void ResetTimers(void);
static LINEARPLANP NoBacktrackingSuccessorFn
(
	LINEARPLANP plpPlan,
	BINDINGP pbBindings
);

/* local data ------------------------------------------------------------------ */

/* Data used to augment HeuristicSuccessorFn and PrioritySuccessorFn */

static CELLP pcSortValueFn;

/* global data ----------------------------------------------------------------- */

BOOL bUserAbort;						/* user abort */
TIMESTAT tsTimers;						/* timer statistics */

LINEARPLANP plpOpen;					/* list of worlds to be searched */
LINEARPLANP plpClosed;					/* list of worlds already searched */

/* GraphSearch --------------------------------------------------------------

Note:
	This routine is not called directly from TLPlanner!
	Instead, it is called by various specialized search routines.
Description:
	GraphSearch can realize various search strategies dependent on
	the "combinerFn" and the "newStateFilter" function.
Algorithm:
	Find a state that satisfies pfGoalQ.
	Start from the start state, and search according to pfSuccessor
	and pfCombiner.
	Don't try the same state twice.
	Only expand a maximum of limit states.
	Return the a status string, the goal state and the lengths of plOpen
	and plClosed in a list, i.e.:
		(psStatus,plGoalState,Length(plOpen),Length(plClosed))
	The status string is NULL on success.
	It's "NOPLAN" if we run out of states.
	It's "RESOURCES" if we search past our limit.
Notes:
	If we terminate because
	we find a plan we ensure that we don't return "RESOURCES" so that we can
	properly distinguish the rare case where a plan has been found
	just as the resources run out. Also we return (length closed)
	+ 1 as the number of states examined, as we need to also count
	the current state.

Scheme:

(define (graph-search start goal? successor-fn combiner-fn state= limit
	. optional-args)

	(do (
			(CLOSED '())
			(OPEN '())
			(new-states-filter
				(if (null? optional-args)
					filter-old-states
					(first optional-args)))
		   (current-state start)
		   (children-states '()))

	  ;;In the termination condition below, if we terminate because
	  ;;we find a plan we ensure that limit is not 0 so that we can
	  ;;properly distinguish the rare case where a plan has been found
	  ;;just as the resources run out. Also we return (length closed)
	  ;;+ 1 as the number of states examined, as we need to also count
	  ;;the current state.

		((or (null? current-state)
				(and (goal? current-state)
					(set! limit (if (>= 0 limit) 1 limit)))
				(>= 0 limit))
			(list
				(if (>= 0 limit)
					'RESOURCES
					(if (null? current-state)
						'NO-PLAN
						current-state))
				(length OPEN)
				(+ 1 (length CLOSED))))
		(set! limit (- limit 1))
		(set! children-states (successor-fn current-state))
		(set! CLOSED (cons current-state CLOSED))
		(if children-states
			(set! children-states
				(new-states-filter children-states OPEN CLOSED state=)))
		(if children-states
			(set! OPEN (combiner-fn children-states OPEN)))
		(set! current-state (if (null? OPEN) '() (first OPEN)))
		(set! OPEN (rest OPEN))))
*/

//static char *GraphSearch
//(
//	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
//	LINEARPLANP (*pfSuccessor)(CELLP, LINEARPLANP, BINDINGP),	/* find next state */
//	LINEARPLANP (*pfCombiner)(LINEARPLANP, LINEARPLANP, CELLP),
//	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
//	CELLP pcHeuristic,					/* heuristic cost function */
//	int nLimit,							/* maximum number of states to search */
//	LINEARPLANP (*pfNewStatesFilter)(LINEARPLANP, LINEARPLANP, LINEARPLANP,
//		BOOL (*)(LINEARPLANP, LINEARPLANP), CELLP),	/* passed state filter */
//	BINDINGP pbBindings					/* bindings list */
//)
//{
//	LINEARPLANP plpCurrentState;		/* our current state */
//	LINEARPLANP plpChildrenStates;		/* next available state */
//	LINEARPLANP (*pfStatesFilter)(LINEARPLANP, LINEARPLANP, LINEARPLANP,
//		BOOL (*)(LINEARPLANP, LINEARPLANP), CELLP);	/* state filter pointer */
//	CELLP pcProgressedTLForm;			/* progress control formula */
//	int immutable;						/* ignored */
//	int i;								/* loop index */
//	CELLP pcTLForm;						/* temporal control formula */
//	CELLP pcCCForm;						/* current control formula */
//	int nLength;						/* current plan length (search depth) */
//
//	ENTER("GraphSearch",TRUE);
//	pfStatesFilter=pfNewStatesFilter?pfNewStatesFilter:FilterOldStates;
//
//	if(bPruningAllSuccessors)			/* make sure initial state is checked */
//		PruneList(&plpOpen,pbBindings);
//
//	nLength=0;
//	bUserAbort=FALSE;
//	pcTLForm=NULL;
//	pcCCForm=NULL;
//
//	if(bTimingStatistics)
//		ResetTimers();
//
//	plpCurrentState=plpOpen;
//	if(plpOpen)
//		plpOpen=plpOpen->plpNext;
//	CRCHashInsert(plpCurrentState);		// make sure initial world is matchable
//
//	// prune the first world if necessary
//	
//	if(bPruningAllSuccessors)
//	{
//		if(nTrace)
//			FunctionTracerProlog(plpCurrentState,pbBindings);
//		StartTimer(tsTimers.adfProgress);
//		pcTLForm=LinearPlanTLForm(plpCurrentState);
//		SetZone(&zScratch);
//		pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
//			(pcTLForm,plpCurrentState,pbBindings,&immutable);
//		SetZone(&zPermanent);
//		ZoneCopyFormula(pcProgressedTLForm);
//		ZoneReloc((void **)&pcProgressedTLForm);
//		ZoneRelocFormula(pcProgressedTLForm);
//		ZoneClear(&zScratch);
//		StopTimer(tsTimers.adfProgress);
//
//		if(FalseFormQ(pcProgressedTLForm))
//		{
//			plpCurrentState=NULL;		// prune bad world
//			srSearchResult.nWorldsPruned++;
//			if(nTrace)
//				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
//		}
//	}
//
//	for(i=0;plpCurrentState&&i<nLimit&&!bUserAbort;i++)
//	{
//		// move current state to front of closed list
//		
//		plpCurrentState->plpNext=plpClosed;
//		plpClosed=plpCurrentState;
//
//		// progress the current state
//		
//		if(!bPruningAllSuccessors)
//		{
//			if(nTrace)
//				FunctionTracerProlog(plpCurrentState,pbBindings);
//			StartTimer(tsTimers.adfProgress);
//			pcTLForm=LinearPlanTLForm(plpCurrentState);
//			SetZone(&zScratch);
//			pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
//				(pcTLForm,plpCurrentState,pbBindings,&immutable);
//			SetZone(&zPermanent);
//			ZoneCopyFormula(pcProgressedTLForm);
//			ZoneReloc((void **)&pcProgressedTLForm);
//			ZoneRelocFormula(pcProgressedTLForm);
//			ZoneClear(&zScratch);
//			StopTimer(tsTimers.adfProgress);
//		}
//		else
//			pcProgressedTLForm=plpCurrentState->pcTLForm;
//
//		// continue processing if world hasn't been pruned
//
//		if(bPruningAllSuccessors||!FalseFormQ(pcProgressedTLForm))
//		{
//			// check for goal state... handle goal addendum
//			
//			if((*pfGoalQ)(plpCurrentState))
//			{
//				if(pcGoalAddendum)
//				{
//					if((*pcGoalAddendum->pfForm->paAction->pfEval)
//						(pcGoalAddendum,plpCurrentState,pbBindings))
//						break;
//				}
//				else
//					break;
//			}
//
//			// generate successor worlds
//
//			if(plpCurrentState->dfHeuristic>srSearchResult.dfSearchMaxHeuristic)
//				srSearchResult.dfSearchMaxHeuristic=plpCurrentState->dfHeuristic;
//			if((!nSearchDepthLimit||plpCurrentState->nLength<=nSearchDepthLimit)&&
//				(!dfSearchHeuristicLimit||plpCurrentState->dfHeuristic<=dfSearchHeuristicLimit))
//			{
//				plpChildrenStates=(*pfSuccessor)(pcProgressedTLForm,plpCurrentState,pbBindings);
//				if(plpChildrenStates&&plpChildrenStates->nLength>srSearchResult.nSearchMaxDepth)
//					srSearchResult.nSearchMaxDepth=plpChildrenStates->nLength;
//				if(nTrace)
//					FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,plpChildrenStates,pbBindings);
//				
//				if(plpChildrenStates)
//					plpChildrenStates=(*pfStatesFilter)(plpChildrenStates,
//						plpOpen,plpClosed,pfStateEqQ,pcHeuristic);
//				if(bPruningAllSuccessors&&plpChildrenStates)
//					PruneList(&plpChildrenStates,pbBindings);
//				if(plpChildrenStates)
//					plpOpen=(*pfCombiner)(plpChildrenStates,plpOpen,pcHeuristic);
//			}
//		}
//		else
//		{
//			srSearchResult.nWorldsPruned++;
//			if(nTrace)
//				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
//		}
//
//		/* Collect garbage */
//
//		if(pzCurrent->nTotal>nZoneLimit)
//		{
//			StartTimer(tsTimers.adfGarbageCollect);
//#ifdef _DEBUG
//			CommandPrintf(stderr,"\nCollecting garbage at world %d\n",nWorldNumber);
//#else // _DEBUG
//			CommandPrintf(stderr,"\n");
//#endif // _DEBUG
//			MarkBindings(pbBindings);
//			MarkDomainData();
//			MarkPlanList(plpClosed);
//			MarkPlanList(plpOpen);
//			if(bConcurrentPlanning)
//				MarkTimeQueue();
//			CollectGarbage();
//			StopTimer(tsTimers.adfGarbageCollect);
//		}
//
//#ifdef _DEBUG
//		if(plpCurrentState->nLength>nLength)	// show search depth on screen
//		{
//			nLength=plpCurrentState->nLength;
//			CommandPrintf(stderr,"%d",nLength);
//		}
//#endif // _DEBUG
//
//		// select next world
//		
//		plpCurrentState=plpOpen;
//		if(plpOpen)
//			plpOpen=plpOpen->plpNext;
//	}
//
//	if(i==nLimit&&plpCurrentState&&!bUserAbort)
//	{
//		if((*pfGoalQ)(plpCurrentState))
//		{
//			if(pcGoalAddendum)
//			{
//				if((*pcGoalAddendum->pfForm->paAction->pfEval)
//					(pcGoalAddendum,plpCurrentState,pbBindings))
//					i=nLimit-1;
//			}
//			else
//				i=nLimit-1;
//		}
//	}	
//
//	srSearchResult.plpState=(i<nLimit&&!bUserAbort)?plpCurrentState:NULL;
//	srSearchResult.nOpenLength=PlanCount(plpOpen);
//	srSearchResult.nClosedLength=PlanCount(plpClosed);
//	EXIT("GraphSearch");
//	if(bUserAbort)
//		return apsStringTab[STRING_USER_ABORT];
//	if(i>=nLimit)
//		return apsStringTab[STRING_RESOURCES];
//	if(!plpCurrentState)
//		return apsStringTab[STRING_NO_PLAN];
//	return (char *)NULL;
//}

/* Search routines implemented as specializations of GraphSearch --------------- */

/* BreadthFirst

Description:
	Search old states first until goal is reached.
Scheme:

(define (BREADTH-FIRST start goal? successor-fn state= cost-fn priority-fn
		   limit)
  (graph-search start goal? successor-fn prepend state= limit))
*/

char *BreadthFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plpCurrentState;		/* our current state */
	LINEARPLANP plpChildrenStates;		/* next available state */
	CELLP pcProgressedTLForm;			/* progress control formula */
	int immutable;						/* ignored */
	int i;								/* loop index */
	CELLP pcTLForm;						/* temporal control formula */
	CELLP pcCCForm;						/* current control formula */
	int nLength;						/* current plan length (search depth) */
	LINEARPLANP plpLast;				// debug

	ENTER("BreadthFirst",TRUE);

	if(bPruningAllSuccessors)			/* make sure initial state is checked */
		PruneList(&plpOpen,pbBindings);

	nLength=0;
	bUserAbort=FALSE;
	pcTLForm=NULL;
	pcCCForm=NULL;

	if(bTimingStatistics)
		ResetTimers();

	plpCurrentState=plpOpen;
	if(plpOpen)
		plpOpen=plpOpen->plpNext;
	plpLast=(LINEARPLANP)&plpOpen;		// initialize tail pointer
	CRCHashInsert(plpCurrentState);		// make sure initial world is matchable

	// prune the first world if necessary
	
	if(bPruningAllSuccessors)
	{
		if(nTrace)
			FunctionTracerProlog(plpCurrentState,pbBindings);
		StartTimer(tsTimers.adfProgress);
		pcTLForm=LinearPlanTLForm(plpCurrentState);
		SetZone(&zScratch);
		pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
			(pcTLForm,plpCurrentState,pbBindings,&immutable);
		SetZone(&zPermanent);
		ZoneCopyFormula(pcProgressedTLForm);
		ZoneReloc((void **)&pcProgressedTLForm);
		ZoneRelocFormula(pcProgressedTLForm);
		ZoneClear(&zScratch);
		StopTimer(tsTimers.adfProgress);

		if(FalseFormQ(pcProgressedTLForm))
		{
			plpCurrentState=NULL;		// prune bad world
			srSearchResult.nWorldsPruned++;
			if(nTrace)
				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
		}
	}

	for(i=0;plpCurrentState&&i<nLimit&&!bUserAbort;i++)
	{
		// move current state to front of closed list
		
		plpCurrentState->plpNext=plpClosed;
		plpClosed=plpCurrentState;

		// progress the current state
		
		if(!bPruningAllSuccessors)
		{
			if(nTrace)
				FunctionTracerProlog(plpCurrentState,pbBindings);
			StartTimer(tsTimers.adfProgress);
			pcTLForm=LinearPlanTLForm(plpCurrentState);
			SetZone(&zScratch);
			pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
				(pcTLForm,plpCurrentState,pbBindings,&immutable);
			SetZone(&zPermanent);
			ZoneCopyFormula(pcProgressedTLForm);
			ZoneReloc((void **)&pcProgressedTLForm);
			ZoneRelocFormula(pcProgressedTLForm);
			ZoneClear(&zScratch);
			StopTimer(tsTimers.adfProgress);
		}
		else
			pcProgressedTLForm=plpCurrentState->pcTLForm;

		// continue processing if world hasn't been pruned

		if(bPruningAllSuccessors||!FalseFormQ(pcProgressedTLForm))
		{
			// check for goal state... handle goal addendum
			
			if((*pfGoalQ)(plpCurrentState))
			{
				if(pcGoalAddendum)
				{
					if((*pcGoalAddendum->pfForm->paAction->pfEval)
						(pcGoalAddendum,plpCurrentState,pbBindings))
						break;
				}
				else
					break;
			}

			// generate successor worlds

			if(plpCurrentState->dfHeuristic>srSearchResult.dfSearchMaxHeuristic)
				srSearchResult.dfSearchMaxHeuristic=plpCurrentState->dfHeuristic;
			if((!nSearchDepthLimit||plpCurrentState->nLength<=nSearchDepthLimit)&&
				(!dfSearchHeuristicLimit||plpCurrentState->dfHeuristic<=dfSearchHeuristicLimit))
			{
				plpChildrenStates=(*pfSuccessor)(pcProgressedTLForm,pcCCForm,plpCurrentState,pbBindings);
				if(plpChildrenStates&&plpChildrenStates->nLength>srSearchResult.nSearchMaxDepth)
					srSearchResult.nSearchMaxDepth=plpChildrenStates->nLength;
				if(nTrace)
					FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,plpChildrenStates,pbBindings);
				
				if(plpChildrenStates)
					plpChildrenStates=FilterOldStates(plpChildrenStates,
						plpOpen,plpClosed,pfStateEqQ,pcHeuristic);
				if(bPruningAllSuccessors&&plpChildrenStates)
					PruneList(&plpChildrenStates,pbBindings);
				if(plpChildrenStates)
				{
					if(!plpOpen)
						plpLast=(LINEARPLANP)&plpOpen;
					plpLast->plpNext=plpChildrenStates;
					while(plpLast->plpNext)
						plpLast=plpLast->plpNext;
				}
			}
		}
		else
		{
			srSearchResult.nWorldsPruned++;
			if(nTrace)
				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
		}

		/* Collect garbage */

		if(pzCurrent->nTotal>nZoneLimit)
		{
			StartTimer(tsTimers.adfGarbageCollect);
#ifdef _DEBUG
			CommandPrintf(stderr,"\nCollecting garbage at world %d\n",nWorldNumber);
#else // _DEBUG
			CommandPrintf(stderr,"\n");
#endif // _DEBUG
			MarkBindings(pbBindings);
			MarkDomainData();
			MarkPlanList(plpClosed);
			MarkPlanList(plpOpen);
			if(bConcurrentPlanning)
				MarkTimeQueue();
			CollectGarbage();
			StopTimer(tsTimers.adfGarbageCollect);
		}

#ifdef _DEBUG
		if(plpCurrentState->nLength>nLength)	// show search depth on screen
		{
			nLength=plpCurrentState->nLength;
			CommandPrintf(stderr,"%d",nLength);
		}
#endif // _DEBUG

		// select next world
		
		plpCurrentState=plpOpen;
		if(plpOpen)
			plpOpen=plpOpen->plpNext;
	}

	if(i==nLimit&&plpCurrentState&&!bUserAbort)
	{
		if((*pfGoalQ)(plpCurrentState))
		{
			if(pcGoalAddendum)
			{
				if((*pcGoalAddendum->pfForm->paAction->pfEval)
					(pcGoalAddendum,plpCurrentState,pbBindings))
					i=nLimit-1;
			}
			else
				i=nLimit-1;
		}
	}	

	srSearchResult.plpState=(i<nLimit&&!bUserAbort)?plpCurrentState:NULL;
	srSearchResult.nOpenLength=PlanCount(plpOpen);
	srSearchResult.nClosedLength=PlanCount(plpClosed);
	EXIT("BreadthFirst");
	if(bUserAbort)
		return apsStringTab[STRING_USER_ABORT];
	if(i>=nLimit)
		return apsStringTab[STRING_RESOURCES];
	if(!plpCurrentState)
		return apsStringTab[STRING_NO_PLAN];
	return (char *)NULL;
}

/* BreadthFirstPriority

Description:
	Do breadthFirst search but expand highest priority nodes first.
Notes:
	The original scheme code generated functions on the fly.  This allowed
	the code to provide extra parameters where needed.  Ofcourse we can't
	do the same in C.  In some cases, we have simply added the needed extra
	parameters to all functions concerned, and have either used or ignored
	them as required.  In other cases, where the code would have been
	excessively messy, we have made use of static variables, causing the
	affected functions to be NON-REENTRANT!
Scheme:

(define (BREADTH-FIRST-PRIORITY start goal? successor-fn state=
				cost-fn priority-fn limit)
  (breadth-first start goal? (sorted successor-fn priority-fn)
				 state= cost-fn priority-fn limit))
*/

char *BreadthFirstPriority
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* search terminator? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function (ignored) */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
	CELLP pcHeuristic,					/* heuristic cost function */
	CELLP pcPriority,					/* priority function */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	char *ps;

	ENTER("BreadthFirstPriority",TRUE);
	pcSortValueFn=pcPriority;
	ps=BreadthFirst(pfGoalQ,PrioritySuccessorFn,
		pfStateEqQ,pcHeuristic,NULL,nLimit,pbBindings);
	EXIT("BreadthFirstPriority");
	return ps;
}

/* BestFirst -------------------------------------------------------------------

Description:
	Search lowest cost states first until goal is reached.
Scheme:

(define (BEST-FIRST start goal? successor-fn state= cost-fn priority-fn limit)
 (graph-search start goal? successor-fn (merger cost-fn) state= limit
				(filter-worst-old-states cost-fn)))
*/

//char *BestFirstOld
//(
//	BOOL (*pfGoalQ)(LINEARPLANP),		/* search terminator */
//	LINEARPLANP (*pfSuccessor)(CELLP, LINEARPLANP, BINDINGP),	/* find next state */
//	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
//	CELLP pcHeuristic,					/* Heuristic cost function */
//	CELLP pcPriority,					/* priority function (ignored) */
//	int nLimit,							/* maximum number of states to search */
//	BINDINGP pbBindings					/* bindings list */
//)
//{
//	char *ps;
//
//	ENTER("BestFirst",TRUE);
//	ps=GraphSearch(pfGoalQ,pfSuccessor,MergePlans,
//		pfStateEqQ,pcHeuristic,nLimit,FilterWorstOldStates,pbBindings);
//	EXIT("BestFirst");
//	return ps;
//}

// This new version of BestFirst uses a heap to keep the open list sorted.
// It runs much faster than the old version.

char *BestFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* search terminator */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
	CELLP pcHeuristic,					/* Heuristic cost function */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plpCurrentState;		/* our current state */
	LINEARPLANP plpChildrenStates;		/* next available state */
	LINEARPLANP plp;
	CELLP pcProgressedTLForm;			/* progress control formula */
	int immutable;						/* ignored */
	int i;								/* loop index */
	CELLP pcTLForm;						/* temporal control formula */
	CELLP pcCCForm;						/* current control formula */
	int nLength;						/* current plan length (search depth) */

	ENTER("BestFirst",TRUE);
	CreateHeap();						/* create a new heap table for open list */

	if(bPruningAllSuccessors)			/* make sure initial state is checked */
		PruneList(&plpOpen,pbBindings);

	nLength=0;
	bUserAbort=FALSE;
	pcTLForm=NULL;
	pcCCForm=NULL;

	if(bTimingStatistics)
		ResetTimers();

	plpCurrentState=plpOpen;
	CRCHashInsert(plpCurrentState);		// make sure initial world is matchable

	// prune the first world if necessary
	
	if(bPruningAllSuccessors)
	{
		if(nTrace)
			FunctionTracerProlog(plpCurrentState,pbBindings);
		StartTimer(tsTimers.adfProgress);
		pcTLForm=LinearPlanTLForm(plpCurrentState);
		SetZone(&zScratch);
		pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
			(pcTLForm,plpCurrentState,pbBindings,&immutable);
		SetZone(&zPermanent);
		ZoneCopyFormula(pcProgressedTLForm);
		ZoneReloc((void **)&pcProgressedTLForm);
		ZoneRelocFormula(pcProgressedTLForm);
		ZoneClear(&zScratch);
		StopTimer(tsTimers.adfProgress);

		if(FalseFormQ(pcProgressedTLForm))
		{
			plpCurrentState=NULL;		// prune bad world
			srSearchResult.nWorldsPruned++;
			if(nTrace)
				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
		}
	}

	for(i=0;plpCurrentState&&i<nLimit&&!bUserAbort;i++)
	{
		// move current state to front of closed list
		
		plpCurrentState->plpNext=plpClosed;
		plpClosed=plpCurrentState;

		// progress the current state
		
		if(!bPruningAllSuccessors)
		{
			if(nTrace)
				FunctionTracerProlog(plpCurrentState,pbBindings);
			StartTimer(tsTimers.adfProgress);
			pcTLForm=LinearPlanTLForm(plpCurrentState);
			SetZone(&zScratch);
			pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
				(pcTLForm,plpCurrentState,pbBindings,&immutable);
			SetZone(&zPermanent);
			ZoneCopyFormula(pcProgressedTLForm);
			ZoneReloc((void **)&pcProgressedTLForm);
			ZoneRelocFormula(pcProgressedTLForm);
			ZoneClear(&zScratch);
			StopTimer(tsTimers.adfProgress);
		}
		else
			pcProgressedTLForm=plpCurrentState->pcTLForm;

		// continue processing if world hasn't been pruned

		if(bPruningAllSuccessors||!FalseFormQ(pcProgressedTLForm))
		{
			// check for goal state... handle goal addendum
			
			if((*pfGoalQ)(plpCurrentState))
			{
				if(pcGoalAddendum)
				{
					if((*pcGoalAddendum->pfForm->paAction->pfEval)
						(pcGoalAddendum,plpCurrentState,pbBindings))
						break;
				}
				else
					break;
			}

			// generate successor worlds

			if(plpCurrentState->dfHeuristic>srSearchResult.dfSearchMaxHeuristic)
				srSearchResult.dfSearchMaxHeuristic=plpCurrentState->dfHeuristic;
			if((!nSearchDepthLimit||plpCurrentState->nLength<=nSearchDepthLimit)&&
				(!dfSearchHeuristicLimit||plpCurrentState->dfHeuristic<=dfSearchHeuristicLimit))
			{
				plpChildrenStates=(*pfSuccessor)(pcProgressedTLForm,pcCCForm,plpCurrentState,pbBindings);
				if(plpChildrenStates&&plpChildrenStates->nLength>srSearchResult.nSearchMaxDepth)
					srSearchResult.nSearchMaxDepth=plpChildrenStates->nLength;
				if(nTrace)
					FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,plpChildrenStates,pbBindings);
				
				if(plpChildrenStates)
					plpChildrenStates=FilterWorstOldStates(plpChildrenStates,
						plpOpen,plpClosed,pfStateEqQ,pcHeuristic);
				if(bPruningAllSuccessors&&plpChildrenStates)
					PruneList(&plpChildrenStates,pbBindings);
				if(plpChildrenStates)
				{
					for(plp=plpChildrenStates;plp;plp=plp->plpNext)
						HeapInsert(plp);
				}
			}
		}
		else
		{
			srSearchResult.nWorldsPruned++;
			if(nTrace)
				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
		}

		/* Collect garbage */

		if(pzCurrent->nTotal>nZoneLimit)
		{
			StartTimer(tsTimers.adfGarbageCollect);
#ifdef _DEBUG
			CommandPrintf(stderr,"\nCollecting garbage at world %d\n",nWorldNumber);
#else // _DEBUG
			CommandPrintf(stderr,"\n");
#endif // _DEBUG
			MarkBindings(pbBindings);
			MarkDomainData();
			MarkPlanList(plpClosed);
			MarkHeap();
			if(bConcurrentPlanning)
				MarkTimeQueue();
			CollectGarbage();
			StopTimer(tsTimers.adfGarbageCollect);
		}

#ifdef _DEBUG
		if(plpCurrentState->nLength>nLength)	// show search depth on screen
		{
			nLength=plpCurrentState->nLength;
			CommandPrintf(stderr,"%d",nLength);
		}
#endif // _DEBUG

		// select next world
		
		plpCurrentState=HeapExtractMin();
	}

	if(i==nLimit&&plpCurrentState&&!bUserAbort)
	{
		if((*pfGoalQ)(plpCurrentState))
		{
			if(pcGoalAddendum)
			{
				if((*pcGoalAddendum->pfForm->paAction->pfEval)
					(pcGoalAddendum,plpCurrentState,pbBindings))
					i=nLimit-1;
			}
			else
				i=nLimit-1;
		}
	}	

	srSearchResult.plpState=(i<nLimit&&!bUserAbort)?plpCurrentState:NULL;
	srSearchResult.nOpenLength=PlanCount(plpOpen);
	srSearchResult.nClosedLength=PlanCount(plpClosed);
	EXIT("BestFirst");
	if(bUserAbort)
		return apsStringTab[STRING_USER_ABORT];
	if(i>=nLimit)
		return apsStringTab[STRING_RESOURCES];
	if(!plpCurrentState)
		return apsStringTab[STRING_NO_PLAN];
	return (char *)NULL;
}

/* Space efficient Depth First Search ------------------------------------------ */

/* DepthFirst

Description:
	Search new states first until goal is reached.
	Stores only the current path, but checks for cycles on that path.
	For other applications there may be different ways for cycle checking.
Scheme:

(define (DEPTH-FIRST start goal? successor-fn state= cost-fn priority-fn limit
		. optional-args)
	(do ((current-state start)
			(children-states '())
			(OPEN '())
			(cycle-filter-fn (if (null? optional-args)
					cycle-filter
					(first optional-args)))
			(states-expanded 0))

		((or (null? current-state)
				(and (goal? current-state)
					(set! limit (if (>= 0 limit) 1 limit)))
				(>= 0 limit))
			(list
				(if (>= 0 limit) 'RESOURCES
					(if (null? current-state) 'NO-PLAN
						current-state))
				(length OPEN) (+ 1 states-expanded)))
		(set! limit (- limit 1))
		(set! states-expanded (+ 1 states-expanded))
		(set! children-states (successor-fn current-state))
		(if children-states
			(set! children-states
				(cycle-filter-fn children-states)))
		(if children-states
			(set! OPEN (append! children-states OPEN)))
		(set! current-state (if (null? OPEN) '() (first OPEN)))
		(set! OPEN (rest OPEN))))
*/

char *DepthFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparitor */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plpCurrentState;		/* our current state */
	LINEARPLANP plpChildrenStates;		/* next available state */
	CELLP pcProgressedTLForm;			/* progressed control formula */
	CELLP pcTLForm;						/* temporal formula */
	CELLP pcCCForm;						// current condition formula
	BOOL bImmutable;					/* ignored */
	int nStatesExpanded;				/* number of states expanded */
	int i;								/* loop index */
	BOOL bTLForm;						/* temporal control formula evaluated in current context */
	BOOL bCCForm;						/* current control formula evaluated in current context */
	int p;

	ENTER("DepthFirst",TRUE);

	if(bPruningAllSuccessors)			/* make sure initial state is checked */
		PruneList(&plpOpen,pbBindings);
	plpCurrentState=plpOpen;
	if(plpOpen)
		plpOpen=plpOpen->plpNext;

	bUserAbort=FALSE;
	nStatesExpanded=0;
	pcTLForm=NULL;
	pcCCForm=NULL;
	bTLForm=FALSE;
	bCCForm=FALSE;

	if(bTimingStatistics)
		ResetTimers();

	if(bAtemporalControl)
		pcTLForm=plpInitialWorld->pcTLForm;

	p=0;
	for(i=0;plpCurrentState&&i<nLimit&&!bUserAbort;i++)
	{
		// handle action-promises
		
		if(!plpCurrentState->apbtWorld)
			(*plpCurrentState->pcPromise->pfForm->paAction->pfEval)
				(plpCurrentState->pcPromise,plpCurrentState,pbBindings);

		if(nTrace)
			FunctionTracerProlog(plpCurrentState,pbBindings);

		if(bAtemporalControl)
		{
			if(!bCycleChecking||CycleFilter(plpCurrentState))
			{		
				nStatesExpanded++;
				if(!bPruningAllSuccessors)
				{
					SetZone(&zScratch);
					bTLForm=(*pcTLForm->pfForm->paAction->pfEval)
						(pcTLForm,plpCurrentState,pbBindings);
					SetZone(&zPermanent);
					ZoneClear(&zScratch);
				}
				if(bPruningAllSuccessors||bTLForm)
				{
					if((*pfGoalQ)(plpCurrentState))
					{
						if(pcGoalAddendum)
						{
							if((*pcGoalAddendum->pfForm->paAction->pfEval)
								(pcGoalAddendum,plpCurrentState,pbBindings))
								break;
						}
						else
							break;
					}
		
					if(plpCurrentState->dfHeuristic>srSearchResult.dfSearchMaxHeuristic)
						srSearchResult.dfSearchMaxHeuristic=plpCurrentState->dfHeuristic;
					if((!nSearchDepthLimit||plpCurrentState->nLength<=nSearchDepthLimit)&&
						(!dfSearchHeuristicLimit||plpCurrentState->dfHeuristic<=dfSearchHeuristicLimit))
					{
						plpChildrenStates=(*pfSuccessor)(NULL,NULL,plpCurrentState,pbBindings);
						if(plpChildrenStates&&plpChildrenStates->nLength>srSearchResult.nSearchMaxDepth)
							srSearchResult.nSearchMaxDepth=plpChildrenStates->nLength;
						if(nTrace)
							FunctionTracer(plpCurrentState,(CELLP)TRUE,(CELLP)TRUE,0,plpChildrenStates,pbBindings);

						if(plpChildrenStates)
						{
							if(bPruningAllSuccessors)
								PruneList(&plpChildrenStates,pbBindings);
							if(bBackTracking)
								plpOpen=AppendPlans(plpChildrenStates,plpOpen);
							else
								plpOpen=plpChildrenStates;
						}
					}
				}
				else
				{
					srSearchResult.nWorldsPruned++;
					if(nTrace)
						FunctionTracer(plpCurrentState,FALSE,FALSE,0,NULL,pbBindings);
				}
			}
		}
		else							// temporal control
		{
			if(!bCycleChecking||CycleFilter(plpCurrentState))
			{	
				nStatesExpanded++;
				if(!bPruningAllSuccessors)
				{
					// test out current formula

					pcCCForm=plpCurrentState->pcCCForm;
					if(pcCCForm)
						bCCForm=(*pcCCForm->pfForm->paAction->pfEval)(pcCCForm,plpCurrentState,pbBindings);
					else
						bCCForm=TRUE;	// a NULL formula can't fail

					// if the current formula succeeds,
					// progress the control formula for our child states

					if(bCCForm)
					{
						p++;
						StartTimer(tsTimers.adfProgress);
						pcTLForm=LinearPlanTLForm(plpCurrentState);
						SetZone(&zScratch);
						pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
							(pcTLForm,plpCurrentState,pbBindings,&bImmutable);

						SetZone(&zPermanent);
						ZoneCopyFormula(pcProgressedTLForm);
						ZoneReloc((void **)&pcProgressedTLForm);
						ZoneRelocFormula(pcProgressedTLForm);
						ZoneClear(&zScratch);
						StopTimer(tsTimers.adfProgress);

						// report a miss (for now)

						if(FalseFormQ(pcProgressedTLForm))
						{
							CommandPrintf(pfTraceStream,
								"Current formula failure (recoverable) in world %d\n",
								plpCurrentState->nWorldNumber);
							bCCForm=FALSE;	// show failure with current flag
						}
					}

//#if _DEBUG
//					else
//					{
//						StartTimer(tsTimers.adfProgress);
//						pcTLForm=LinearPlanTLForm(plpCurrentState);
//						SetZone(&zScratch);
//						pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
//							(pcTLForm,plpCurrentState,pbBindings,&bImmutable);
//
//						SetZone(&zPermanent);
//						ZoneCopyFormula(pcProgressedTLForm);
//						ZoneReloc(&pcProgressedTLForm);
//						ZoneRelocFormula(pcProgressedTLForm);
//						ZoneClear(&zScratch);
//						StopTimer(tsTimers.adfProgress);
//
//						// report a failure
//
//						if(!FalseFormQ(pcProgressedTLForm))
//						{
//							CommandPrintf(pfTraceStream,
//								"Current formula failure (unrecoverable) in world %d\n",
//								plpCurrentState->nWorldNumber);
//							bCCForm=TRUE;	// show success with current flag
//						}
//					}
//#endif // _DEBUG
				}
				else
					pcProgressedTLForm=plpCurrentState->pcTLForm;		/* test!!! */

				// if we've gotten here and we're pruning all successors, then
				// we are guaranteed that our control formula will not progress to false!

//				if(bPruningAllSuccessors||!FalseFormQ(pcProgressedTLForm))
				if(bPruningAllSuccessors||bCCForm)
				{
					// generate the current formula for our children states

					StartTimer(tsTimers.adfProgress);
					SetZone(&zScratch);
					pcCCForm=(*pcProgressedTLForm->pfForm->paAction->pfMakeCCForm)
						(FALSE,pcProgressedTLForm,&bImmutable);
					SetZone(&zPermanent);
					if(pcCCForm)
					{
						ZoneCopyFormula(pcCCForm);
						ZoneReloc((void **)&pcCCForm);
						ZoneRelocFormula(pcCCForm);
					}
					ZoneClear(&zScratch);
					StopTimer(tsTimers.adfProgress);

					if((*pfGoalQ)(plpCurrentState))
					{
						if(pcGoalAddendum)
						{
							if((*pcGoalAddendum->pfForm->paAction->pfEval)
								(pcGoalAddendum,plpCurrentState,pbBindings))
								break;
						}
						else
							break;
					}
	
					if(plpCurrentState->dfHeuristic>srSearchResult.dfSearchMaxHeuristic)
						srSearchResult.dfSearchMaxHeuristic=plpCurrentState->dfHeuristic;
					if((!nSearchDepthLimit||plpCurrentState->nLength<=nSearchDepthLimit)&&
						(!dfSearchHeuristicLimit||plpCurrentState->dfHeuristic<=dfSearchHeuristicLimit))
					{
						plpChildrenStates=(*pfSuccessor)(pcProgressedTLForm,pcCCForm,plpCurrentState,pbBindings);
						if(plpChildrenStates&&plpChildrenStates->nLength>srSearchResult.nSearchMaxDepth)
							srSearchResult.nSearchMaxDepth=plpChildrenStates->nLength;
						if(nTrace)
							FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,plpChildrenStates,pbBindings);

						if(plpChildrenStates)
						{
							if(bPruningAllSuccessors)
								PruneList(&plpChildrenStates,pbBindings);
							if(bBackTracking)
								plpOpen=AppendPlans(plpChildrenStates,plpOpen);
							else
								plpOpen=plpChildrenStates;
						}
					}
				}
				else
				{
					srSearchResult.nWorldsPruned++;
					if(nTrace)
						FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
				}
			}
		}

		/* Collect garbage */

		if(pzCurrent->nTotal>nZoneLimit)
		{
			StartTimer(tsTimers.adfGarbageCollect);
#ifdef _DEBUG
			CommandPrintf(stderr,"\nCollecting garbage at world %d\n",nWorldNumber);
#else // _DEBUG
			CommandPrintf(stderr,"\n");
#endif // _DEBUG
			MarkBindings(pbBindings);
			MarkDomainData();
			MarkParentPlans(plpCurrentState);
			MarkPlanList(plpOpen);
			if(bConcurrentPlanning)
				MarkTimeQueue();
			CollectGarbage();
			StopTimer(tsTimers.adfGarbageCollect);
		}

		plpCurrentState=plpOpen;
		if(plpOpen)
			plpOpen=plpOpen->plpNext;
	}

	// if we fell out of the main loop due to the loop limit,
	// we haven't checked the last world to see if it matches the goal,
	// so we do it here, and flag success by decrementing the loop index.
	
	if(i==nLimit&&plpCurrentState&&!bUserAbort)
	{
		// handle action-promises
		
		if(!plpCurrentState->apbtWorld)
			(*plpCurrentState->pcPromise->pfForm->paAction->pfEval)
				(plpCurrentState->pcPromise,plpCurrentState,pbBindings);

		if((*pfGoalQ)(plpCurrentState))
		{
			if(pcGoalAddendum)
			{
				if((*pcGoalAddendum->pfForm->paAction->pfEval)
					(pcGoalAddendum,plpCurrentState,pbBindings))
					i=nLimit-1;
			}
			else
				i=nLimit-1;
		}
	}

	srSearchResult.plpState=(i<nLimit&&!bUserAbort)?plpCurrentState:NULL;
	srSearchResult.nOpenLength=PlanCount(plpOpen);
	srSearchResult.nClosedLength=nStatesExpanded+srSearchResult.nWorldsPruned;

	EXIT("DepthFirst");
	if(bUserAbort)
		return apsStringTab[STRING_USER_ABORT];
	if(i>=nLimit)
		return apsStringTab[STRING_RESOURCES];
	if(!plpCurrentState)
		return apsStringTab[STRING_NO_PLAN];
#if _DEBUG
	CommandPrintf(stderr,"P %d\n",p);
#endif // _DEBUG
	return (char *)NULL;				// success
}

/* DepthFirstPriority

Description:
	Do depthFirst search but explore highest priority children first.
Scheme:

(define (DEPTH-FIRST-PRIORITY start goal? successor-fn state= cost-fn
				  priority-fn limit)
  (depth-first start goal? (sorted successor-fn priority-fn)
		   state= cost-fn priority-fn limit))
*/

char *DepthFirstPriority
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* search terminator */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	char *ps;

	ENTER("DepthFirstPriority",TRUE);
	pcSortValueFn=pcPriority;
	ps=DepthFirst(pfGoalQ,PrioritySuccessorFn,pfStateEqQ,
		pcHeuristic,pcPriority,nLimit,pbBindings);
	EXIT("DepthFirstPriority");
	return ps;
}

/* DepthBestFirst

Description:
	Do depthFirst search but explore lowest cost children first.
*/

char *DepthBestFirst
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* search terminator */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparison function */
	CELLP pcHeuristic,					/* heuristic cost function */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	char *ps;

	ENTER("DepthBestFirst",TRUE);
	pcSortValueFn=pcHeuristic;
	ps=DepthFirst(pfGoalQ,HeuristicSuccessorFn,pfStateEqQ,
		pcHeuristic,pcPriority,nLimit,pbBindings);
	EXIT("DepthBestFirst");
	return ps;
}

/* DepthFirstNoBacktracking ----------------------------------------------------

Description:
	Search new states first until goal is reached.
	Cycle checking and pruning are handled by modify-world, so that we can
	terminate early.
	Since we can't backtrack, each time we expand a world, we can terminate
	expansion as soon as we generate a viable world.  
*/

char *DepthFirstNoBacktracking
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparitor */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plpCurrentState;		/* our current state */
	LINEARPLANP plpChildState;			/* next available state */
	CELLP pcProgressedTLForm;			/* progressed control formula */
	int immutable;						/* ignored */
	int nStatesExpanded;				/* number of states expanded */
	int i;								/* loop index */
	CELLP pcTLForm;						/* temporal control formula */
	CELLP pcCCForm;						/* current control formula */

	ENTER("DepthFirstNoBacktracking",TRUE);

	plpCurrentState=plpOpen;
	plpOpen=NULL;

	bUserAbort=FALSE;
	nStatesExpanded=0;
	pcTLForm=NULL;
	pcCCForm=NULL;

	if(bTimingStatistics)
		ResetTimers();

	// cycle check and prune the initial world

	if(!bCycleChecking||CycleFilter(plpCurrentState))
	{	
		srSearchResult.nWorldsSearched++;
		StartTimer(tsTimers.adfProgress);
		pcTLForm=LinearPlanTLForm(plpCurrentState);
		SetZone(&zScratch);
		pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
			(pcTLForm,plpCurrentState,pbBindings,&immutable);
		SetZone(&zPermanent);
		ZoneCopyFormula(pcProgressedTLForm);
		ZoneReloc((void **)&pcProgressedTLForm);
		ZoneRelocFormula(pcProgressedTLForm);
		ZoneClear(&zScratch);
		StopTimer(tsTimers.adfProgress);
		if(FalseFormQ(pcProgressedTLForm))
		{
			plpCurrentState=NULL;		// prune bad world
			srSearchResult.nWorldsPruned++;
			if(nTrace)
			{
				FunctionTracerProlog(plpCurrentState,pbBindings);
				FunctionTracer(plpCurrentState,plpCurrentState->pcTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
			}
		}
	}
	else
		plpCurrentState=NULL;			// discard cyclic world

	if(plpCurrentState)
		plpCurrentState->pcTLForm=pcProgressedTLForm;

	for(i=0;plpCurrentState&&i<nLimit&&!bUserAbort;i++)
	{
		// this world has already survived cycle checking and pruning
		// so we simply goal check and expand it

		if((*pfGoalQ)(plpCurrentState))
		{
			if(pcGoalAddendum)
			{
				if((*pcGoalAddendum->pfForm->paAction->pfEval)
					(pcGoalAddendum,plpCurrentState,pbBindings))
					break;
			}
			else
				break;
		}

		if(nTrace)
			FunctionTracerProlog(plpCurrentState,pbBindings);
		plpChildState=NoBacktrackingSuccessorFn(plpCurrentState,pbBindings);
		if(plpChildState)
			srSearchResult.nSearchMaxDepth=plpChildState->nLength;
		if(nTrace)
			FunctionTracer(plpCurrentState,plpCurrentState->pcTLForm,pcTLForm,plpCurrentState->pcCCForm,plpChildState,pbBindings);
		plpOpen=plpChildState;

		// Collect garbage

		if(pzCurrent->nTotal>nZoneLimit)
		{
			StartTimer(tsTimers.adfGarbageCollect);
#ifdef _DEBUG
			CommandPrintf(stderr,"\nCollecting garbage at world %d\n",nWorldNumber);
#else // _DEBUG
			CommandPrintf(stderr,"\n");
#endif // _DEBUG
			MarkBindings(pbBindings);
			MarkDomainData();
			MarkParentPlans(plpCurrentState);
			MarkPlanList(plpOpen);
			if(bConcurrentPlanning)
				MarkTimeQueue();
			CollectGarbage();
			StopTimer(tsTimers.adfGarbageCollect);
		}

		plpCurrentState=plpOpen;
		if(plpOpen)
			plpOpen=plpOpen->plpNext;
	}

	// if we fell out of the main loop due to the loop limit,
	// we haven't checked the last world to see if it matches the goal,
	// so we do it here, and flag success by decrementing the loop index.
	
	if(i==nLimit&&plpCurrentState&&!bUserAbort)
	{
		if((*pfGoalQ)(plpCurrentState))
		{
			if(pcGoalAddendum)
			{
				if((*pcGoalAddendum->pfForm->paAction->pfEval)
					(pcGoalAddendum,plpCurrentState,pbBindings))
					i=nLimit-1;
			}
			else
				i=nLimit-1;
		}
	}

	srSearchResult.plpState=(i<nLimit&&!bUserAbort)?plpCurrentState:NULL;
	srSearchResult.nOpenLength=PlanCount(plpOpen);
	srSearchResult.nClosedLength=srSearchResult.nWorldsSearched+srSearchResult.nWorldsPruned;

	EXIT("DepthFirstNoBacktracking");
	if(bUserAbort)
		return apsStringTab[STRING_USER_ABORT];
	if(i>=nLimit)
		return apsStringTab[STRING_RESOURCES];
	if(!plpCurrentState)
		return apsStringTab[STRING_NO_PLAN];
	return (char *)NULL;
}

/* FollowPredefinedPlan --------------------------------------------------------

Description:
	Follow a predefined plan for load-plan.
Notes:
	This routine is modelled after DepthFirst since a plan provides a direct
	path from the initial state to the goal state.
	Since we are following a plan, there is no need to check for cycles.
	We progress the temporal control formula as a side effect.
*/

char *FollowPredefinedPlan
(
	BOOL (*pfGoalQ)(LINEARPLANP),		/* are we done yet? */
	LINEARPLANP (*pfSuccessor)(CELLP, CELLP, LINEARPLANP, BINDINGP),	/* successor function */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* state comparitor */
	CELLP pcHeuristic,					/* heuristic cost function (ignored) */
	CELLP pcPriority,					/* priority function (ignored) */
	int nLimit,							/* maximum number of states to search */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plpCurrentState;		/* our current state */
	CELLP pcProgressedTLForm;			/* progressed control formula */
	int nStatesExpanded;				/* number of states expanded */
	int immutable;						/* ignored */
	CELLP pcTLForm;						/* temporal control formula */
	CELLP pcCCForm;						/* current control formula */
	CELLP pc;							/* operation pointer */
	LINEARPLANP plp;

	ENTER("FollowPredefinedPlan",TRUE);

	plpCurrentState=plpOpen;
	if(plpOpen)
		plpOpen=plpOpen->plpNext;

	bUserAbort=FALSE;
	nStatesExpanded=0;
	pcTLForm=NULL;
	pcCCForm=NULL;
	
	if(bTimingStatistics)
		ResetTimers();

	for(pc=pcPredefinedPlan;pc;pc=pc->pcNext)
	{
		if(nTrace)
			FunctionTracerProlog(plpCurrentState,pbBindings);
		if(!bCycleChecking||CycleFilter(plpCurrentState))
		{	
			nStatesExpanded++;

			StartTimer(tsTimers.adfProgress);
			pcTLForm=LinearPlanTLForm(plpCurrentState);
			SetZone(&zScratch);
			pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
				(pcTLForm,plpCurrentState,pbBindings,&immutable);
			SetZone(&zPermanent);
			ZoneCopyFormula(pcProgressedTLForm);
			ZoneReloc((void **)&pcProgressedTLForm);
			ZoneRelocFormula(pcProgressedTLForm);
			ZoneClear(&zScratch);
			StopTimer(tsTimers.adfProgress);

			if(!FalseFormQ(pcProgressedTLForm))
			{
				if((*pfGoalQ)(plpCurrentState))
				{
					if(pcGoalAddendum)
					{
						if((*pcGoalAddendum->pfForm->paAction->pfEval)
							(pcGoalAddendum,plpCurrentState,pbBindings))
							break;
					}
					else
						break;
				}
				if(plpCurrentState->dfHeuristic>srSearchResult.dfSearchMaxHeuristic)
					srSearchResult.dfSearchMaxHeuristic=plpCurrentState->dfHeuristic;
				if((!nSearchDepthLimit||plpCurrentState->nLength<=nSearchDepthLimit)&&
					(!dfSearchHeuristicLimit||plpCurrentState->dfHeuristic<=dfSearchHeuristicLimit))
				{
					plpOpen=(*pfSuccessor)(pcProgressedTLForm,pcCCForm,plpCurrentState,pbBindings);
					if(plpOpen&&plpOpen->nLength>srSearchResult.nSearchMaxDepth)
						srSearchResult.nSearchMaxDepth=plpOpen->nLength;
					if(nTrace)
						FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,plpOpen,pbBindings);
				}
			}
		}
		else
		{
			srSearchResult.nWorldsPruned++;
			if(nTrace)
				FunctionTracer(plpCurrentState,pcProgressedTLForm,pcTLForm,pcCCForm,NULL,pbBindings);
		}

		/* Collect garbage */

		if(pzCurrent->nTotal>nZoneLimit)
		{
			StartTimer(tsTimers.adfGarbageCollect);
#ifdef _DEBUG
			CommandPrintf(stderr,"\nCollecting garbage at world %d\n",nWorldNumber);
#else // _DEBUG
			CommandPrintf(stderr,"\n");
#endif // _DEBUG
			MarkBindings(pbBindings);
			MarkDomainData();
			MarkParentPlans(plpCurrentState);
			MarkPlanList(plpOpen);
			if(bConcurrentPlanning)
				MarkTimeQueue();
			CollectGarbage();
			StopTimer(tsTimers.adfGarbageCollect);
		}

		/* find the state that matches the current action */

		for(plp=plpOpen;plp;plp=plp->plpNext)
		{
			if(FormulaEqQ(plp->pcActionName,pc))
				break;
		}
		plpCurrentState=plp;
		plpOpen=NULL;
		if(!plp)
		{	
			CommandPrintf(pfTraceStream,"load-plan:  Invalid action, step %d.\n",nStatesExpanded+1);
			PrintFormula(pfTraceStream,pc,0);
			break;
		}
	}

	// if we got here and this isn't the goal state, something is wrong!

	srSearchResult.plpState=plpCurrentState;
	srSearchResult.nOpenLength=PlanCount(plpOpen);
	srSearchResult.nClosedLength=nStatesExpanded;
	EXIT("FollowPredefinedPlan");
	return (!plpCurrentState?apsStringTab[STRING_NO_PLAN]:(char *)NULL);
}

/* Successor Functions --------------------------------------------------------- */

/* PlanSuccessorFn

Description:
	This is the default successor function.
	Grow a plan in all possible ways.  
	It generates worlds in the same order as the operators have been defined in.
Scheme:

(define (plan-successor-fn plan)
	(let ((progressed-tlform
				(progress-formula
					(plan-tlform  plan)
					(plan-world/action plan)
					'()))
			(new-plans '()))
		(if progressed-tlform
			(do ((successors (generate-successors (plan-world/action plan)))
					(successor '()))
				((null? successors))
				(set! successor (first successors))
				(set! successors (rest successors))
				(set! new-plans (cons (plan-extend successor progressed-tlform plan)
						new-plans)))
			(set! *pruned-worlds* (+ *pruned-worlds* 1)))
		;; TRACE, pass it all of the things you might want to trace.
		(tl-tracer 'plan-successors (list plan progressed-tlform new-plans))
		new-plans))
*/

LINEARPLANP PlanSuccessorFn
(
	CELLP pcTLForm,
	CELLP pcCCForm,
	LINEARPLANP plpPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plpNewPlans,plpNewEnd;
	LINEARPLANP plpSuccessor,plpSuccessors;

	ENTER("PlanSuccessorFn",TRUE);
	StartTimer(tsTimers.adfSuccessor);

	plpNewPlans=NULL;
	plpNewEnd=(LINEARPLANP)&plpNewPlans;

	plpSuccessors=GenerateSuccessors(plpPlan,pbBindings);
	for(plpSuccessor=plpSuccessors;plpSuccessor;plpSuccessor=plpSuccessor->plpNext)
	{
		plpNewEnd=plpNewEnd->plpNext=PlanExtend(plpSuccessor,pcTLForm,pcCCForm,plpPlan,pbBindings);
	}
	StopTimer(tsTimers.adfSuccessor);
	EXIT("PlanSuccessorFn");
	return plpNewPlans;
}

/* NoBacktrackingSuccessorFn

Description:
	This is the successor function for depth-first-no-backtracking.
	Grow a plan in all possible ways.  
	It generates worlds in the same order as the operators have been defined in.
*/

LINEARPLANP NoBacktrackingSuccessorFn
(
	LINEARPLANP plpPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plpNewPlan;
	LINEARPLANP plpSuccessor;

	ENTER("NoBacktrackingSuccessorFn",TRUE);
	StartTimer(tsTimers.adfSuccessor);

	plpNewPlan=NULL;
	plpSuccessor=GenerateSuccessors(plpPlan,pbBindings);
	if(plpSuccessor)
		plpNewPlan=PlanExtend(plpSuccessor,plpSuccessor->pcTLForm,plpSuccessor->pcCCForm,plpPlan,pbBindings);

	StopTimer(tsTimers.adfSuccessor);
	EXIT("NoBacktrackingSuccessorFn");
	return plpNewPlan;
}

/* PrioritySuccessorFn

Description:
	PrioritySuccessorFn produces a list of successors sorted by 
	PriorityCompareFn.
Scheme:

(define (sorted successor-fn priority-fn)
  (lambda (state)
	(sort (successor-fn state)
	  (lambda (x1 x2) (> (priority-fn x1) (priority-fn x2))))))
*/

static LINEARPLANP PrioritySuccessorFn
(
	CELLP pcTLForm,
	CELLP pcCCForm,
	LINEARPLANP plpState,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp;

	ENTER("PrioritySuccessorFn",TRUE);

	plp=PlanSuccessorFn(pcTLForm,pcCCForm,plpState,pbBindings);
	StartTimer(tsTimers.adfSuccessor);
	plp=(LINEARPLANP)MergeSort((MERGELISTP)plp,(MERGECOMPAREP)PriorityCompareFn);
	StopTimer(tsTimers.adfSuccessor);
	EXIT("PrioritySuccessorFn");
	return plp;
}

/* PriorityCompareFn

Description:
	Sort by priority, so that the highest priority ends up at the front of the
	list.
*/

static int PriorityCompareFn
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
)
{
	if(plpPlan1->dfPriority>plpPlan2->dfPriority)	// backwards!
		return -1;
	if(plpPlan1->dfPriority<plpPlan2->dfPriority)	// backwards!
		return 1;
	return 0;
}

/* HeuristicSuccessorFn

Description:
	HeuristicSuccessorFn produces a list of successors sorted by 
	HeuristicCompareFn.
*/

static LINEARPLANP HeuristicSuccessorFn
(
	CELLP pcTLForm,						// progressed control formula
	CELLP pcCCForm,						// current control formula
	LINEARPLANP plpState,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp;

	ENTER("HeuristicSuccessorFn",TRUE);

	plp=PlanSuccessorFn(pcTLForm,pcCCForm,plpState,pbBindings);
	StartTimer(tsTimers.adfSuccessor);
	plp=(LINEARPLANP)MergeSort((MERGELISTP)plp,(MERGECOMPAREP)HeuristicCompareFn);
	EXIT("HeuristicSuccessorFn");
	StopTimer(tsTimers.adfSuccessor);
	return plp;
}

/* HeuristicCompareFn

Description:
	Sort by heuristic, so that the lowest heuristic ends up at the front of the
	list.
*/

static BOOL HeuristicCompareFn
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
)
{
	ENTER("HeuristicCompareFn",TRUE);
	if(plpPlan1->dfHeuristic<plpPlan2->dfHeuristic)
		return -1;
	if(plpPlan1->dfHeuristic>plpPlan2->dfHeuristic)
		return 1;
	return 0;
}

/* Combiner Functions ---------------------------------------------------------- */

/* PrependPlans

Description:
	Prepend a second list to the start of the first.
Notes:
	This is the combiner function for BreadthFirst.
Scheme:

(define (prepend x y)
  (append! y x))
*/

//static LINEARPLANP PrependPlans
//(
//	LINEARPLANP plp1,
//	LINEARPLANP plp2,
//	CELLP pcHeuristic
//)
//{
//	LINEARPLANP plp;
//
//	ENTER("PrependPlans",TRUE);
//	StartTimer(tsTimers.adfUpdateOpenClose);
//
//	if(!plp2)
//		plp2=plp1;
//	else
//	{
//		for(plp=plp2;plp->plpNext;plp=plp->plpNext);	/* find end of second list */
//		plp->plpNext=plp1;
//	}
//	StopTimer(tsTimers.adfUpdateOpenClose);
//	EXIT("PrependPlans");
//	return plp2;
//}

/* AppendPlans

Description:
	Append x to the start of y.
Notes:
	This is the combiner function for DepthFirst.
Scheme:

(define (prepend x y)
  (append! y x))
*/

static LINEARPLANP AppendPlans
(
	LINEARPLANP plp1,
	LINEARPLANP plp2
)
{
	LINEARPLANP plp;

	ENTER("AppendPlans",TRUE);
	StartTimer(tsTimers.adfUpdateOpenClose);
	if(!plp1)
		plp1=plp2;
	else
	{
		for(plp=plp1;plp->plpNext;plp=plp->plpNext);	/* find end of first list */
		plp->plpNext=plp2;
	}
	StopTimer(tsTimers.adfUpdateOpenClose);
	EXIT("AppendPlans");
	return plp1;
}

/* MergePlans

Description:
	A combiner function that merges newStates onto OPEN
	so that minimum heuristic cost states are explored first.
	It also sorts the list of new states.
	This combiner function is DESTRUCTIVE.
Notes:
	This is the combiner function for BestFirst.
Scheme:

(define (merger cost-fn)
  (lambda (new old)
	(merge-sorted-lists (sort new (lambda (x1 x2)
					(< (cost-fn x1) (cost-fn x2))))
			old
			<
			:key cost-fn)))
*/

static LINEARPLANP MergePlans
(
	LINEARPLANP plpNew,
	LINEARPLANP plpOld,
	CELLP pcHeuristic
)
{
	LINEARPLANP plp;

	ENTER("MergePlans",TRUE);
	StartTimer(tsTimers.adfUpdateOpenClose);

	plp=(LINEARPLANP)MergeSort((MERGELISTP)plpNew,(MERGECOMPAREP)MergeCompareFn);
	plp=(LINEARPLANP)MergeSortedLists((MERGELISTP)plpOld,(MERGELISTP)plp,
		(MERGECOMPAREP)MergeCompareFn);
	StopTimer(tsTimers.adfUpdateOpenClose);
	EXIT("MergePlans");
	return plp;
}

/* MergeCompareFn

Description:
	This routine is used to compare worlds for best-first search.
*/

int MergeCompareFn
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
)
{
	ENTER("MergeCompareFn",TRUE);

	/* first test the heuristic cost... cheaper worlds sort earlier */

	if(plpPlan1->dfHeuristic<plpPlan2->dfHeuristic)
	{
		EXIT("MergeCompareFn");
		return -1;
	}
	if(plpPlan1->dfHeuristic>plpPlan2->dfHeuristic)
	{
		EXIT("MergeCompareFn");
		return 1;
	}

	/* on a tie, test the plan length... "deeper" worlds sort earlier */

	if(plpPlan1->nLength>plpPlan2->nLength)
	{
		EXIT("MergeCompareFn");
		return -1;
	}
	if(plpPlan1->nLength<plpPlan2->nLength)
	{
		EXIT("MergeCompareFn");
		return 1;
	}

	/* on a tie, test the world number... "later" worlds sort earlier */

	if(plpPlan1->nWorldNumber<plpPlan2->nWorldNumber)
	{
		EXIT("MergeCompareFn");
		return -1;
	}
	if(plpPlan1->nWorldNumber>plpPlan2->nWorldNumber)
	{
		EXIT("MergeCompareFn");
		return 1;
	}

	/* we can't get here unless the world's are the same (keep the compiler happy). */

	EXIT("MergeCompareFn");
	return 0;
}

/* Filter Functions ------------------------------------------------------------ */

/* CycleFilter

Description:
	Check if a state contains a cycle.
Scheme:

(define (cycle-filter new-states)
  (remove-if plan-has-cycle new-states))
*/

LINEARPLANP CycleFilter
(
	LINEARPLANP plpNewState
)
{
	LINEARPLANP plp;

	ENTER("CycleFilter",TRUE);
	StartTimer(tsTimers.adfCycleCheck);
	plp=plpNewState;
	WorldSignature(plp);
	if(PlanHasCycleQ(plp))
	{
		DiscardWorld(plp);
		plp=NULL;
	}
	StopTimer(tsTimers.adfCycleCheck);
	EXIT("CycleFilter");
	return plp;
}

/* PlanHasCycleQ ---------------------------------------------------------------

Description:
	Plan cycle test used in depthFirst search routine.
	Plan has cycle if its final world appears in the previous subplan
Scheme:

(define (plan-has-cycle plan)
	(cond
		((null? plan)
			#f)
		((null? (plan-previous plan))
			#f)
		(else
			(plan-has-cycle-recurse plan (plan-previous plan)))))

(define (plan-has-cycle-recurse plan previous)
	(cond
		((plan= plan previous)
			#t)
		((null? (plan-previous previous))
			#f)
		(else
			(plan-has-cycle-recurse plan (plan-previous previous)))))
*/

static BOOL PlanHasCycleQ
(
	LINEARPLANP plpPlan
)
{
	LINEARPLANP plpParent;

	ENTER("PlanHasCycleQ",TRUE);
	if(!plpPlan||!LinearPlanParent(plpPlan))
	{
		EXIT("PlanHasCycleQ");
		return FALSE;
	}
	for(plpParent=LinearPlanParent(plpPlan);plpParent;plpParent=LinearPlanParent(plpParent))
	{
		if((*pfPlanEqQ)(plpPlan,plpParent))
		{
			if(nTrace>=3)
				CommandPrintf(pfTraceStream,"\nWorld %d is equivalent to world %d\n",plpPlan->nWorldNumber,plpParent->nWorldNumber);
			EXIT("PlanHasCycleQ");
			return TRUE;
		}
	}
	EXIT("PlanHasCycleQ");
	return FALSE;
}

/* Filters on the successor state allow us to implement A* search using
GraphSearch.

Here we define two filters:

FilterOldStates:
	The default. Always remove successor states that
	have been seen before. Won't find optimal solutions.

FilterWorstOldStates:
	A filter function that keeps successor states that have been seen
	before but which have a lower cost than the previous time they were seen.

To explain further:
	We assume here that the search states have two components
	<path><terminal state>, that "state=" tests for equality of the
	<terminal state>, that "costFn" evaluates cost of <path>, and that
	"successorFn" works on these two component states. That is, the
	burden of maintaining path information is put on the user of these
	search routines.

	FilterWorstOldStates will effectively cause a reSearch of
	certain states, ones to which we have found a better path.
*/

/* FilterOldStates

Description:
	Remove successor states that have been seen before. I.e., are on
	either of the lists OPEN or CLOSED.
Scheme:

(define (filter-old-states new-states OPEN CLOSED state=)
  (remove-if
   (lambda (state)
	 (or (find state OPEN   :test state=)
	 (find state CLOSED :test state=)))
   new-states))
*/

static LINEARPLANP FilterOldStates
(
	LINEARPLANP plpNewStates,			/* list of new states to filter */
	LINEARPLANP plpOpen,				/* list of open states */
	LINEARPLANP plpClosed,				/* list of closed states */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* test two terminal states */
	CELLP pcHeuristic					/* ignored heuristic cost function! */
)
{
	LINEARPLANP plpStart,plpEnd;		/* result listhead */
	LINEARPLANP plpS;					/* current new state */
	LINEARPLANP plpNext;				/* next state */

	ENTER("FilterOldStates",TRUE);
	StartTimer(tsTimers.adfCycleCheck);
	plpStart=NULL;						/* intialize listhead */
	plpEnd=(LINEARPLANP)&plpStart;

	for(plpS=plpNewStates;plpS;plpS=plpNext)
	{
		plpNext=plpS->plpNext;
		WorldSignature(plpS);
		if(!CRCHashSearch(plpS))		// if no match
		{
			CRCHashInsert(plpS);
			plpEnd=plpEnd->plpNext=plpS;
		}
		else
			DiscardWorld(plpS);
	}
	plpEnd->plpNext=NULL;				/* terminate open list */
	StopTimer(tsTimers.adfCycleCheck);
	EXIT("FilterOldStates");
	return plpStart;
}

//static LINEARPLANP FilterOldStatesX
//(
//	LINEARPLANP plpNewStates,			/* list of new states to filter */
//	LINEARPLANP plpOpen,				/* list of open states */
//	LINEARPLANP plpClosed,				/* list of closed states */
//	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* test two terminal states */
//	CELLP pcHeuristic					/* ignored heuristic cost function! */
//)
//{
//	LINEARPLANP plpStart,plpEnd;		/* result listhead */
//	LINEARPLANP plpS;					/* current new state */
//	LINEARPLANP plpO;					/* current open state */
//	LINEARPLANP plpC;					/* current closed state */
//	LINEARPLANP plpNext;				/* next state */
//
//	ENTER("FilterOldStates",TRUE);
//	StartTimer(tsTimers.adfCycleCheck);
//	plpStart=NULL;						/* intialize listhead */
//	plpEnd=(LINEARPLANP)&plpStart;
//
//	for(plpS=plpNewStates;plpS;plpS=plpNext)
//	{
//		plpNext=plpS->plpNext;
//		WorldSignature(plpS);
//		for(plpO=plpOpen;plpO;plpO=plpO->plpNext)	/* check open list */
//			if((*pfStateEqQ)(plpS,plpO))
//				break;
//		plpC=NULL;
//		if(!plpO)						/* else check closed list */
//		{
//			for(plpC=plpClosed;plpC;plpC=plpC->plpNext)
//				if((*pfStateEqQ)(plpS,plpC))
//					break;
//		}
//		if(!plpO&&!plpC)				/* if no match, copy state (discard matching states) */
//			plpEnd=plpEnd->plpNext=plpS;
//		else
//			DiscardWorld(plpS);
//	}
//	plpEnd->plpNext=NULL;				/* terminate open list */
//	StopTimer(tsTimers.adfCycleCheck);
//	EXIT("FilterOldStates");
//	return plpStart;
//}

/* FilterWorstOldStates

Description:
	A filter function that removes successors states that have
	been seen before unless they are lower cost than all previous instances
	according to pfHeuristic.
Scheme:

(define (filter-worst-old-states cost-fn)
	(lambda (new-states OPEN CLOSED state=)
		(do
			((result '())
				(new-state '()))
			((null? new-states) result)
			(set! new-state (first new-states))
			(set! new-states (rest new-states))
			(let ((old-copies (append! (find-all new-state OPEN state=)
							(find-all new-state CLOSED state=)))
					(new-cost (cost-fn new-state)))
			(if (every (lambda (old-copy) (> (cost-fn old-copy) new-cost))
					old-copies)
				(set! result (cons new-state result)))))))
*/

static LINEARPLANP FilterWorstOldStates
(
	LINEARPLANP plpNewStates,			/* list of new states to filter */
	LINEARPLANP plpOpen,				/* list of open states */
	LINEARPLANP plpClosed,				/* list of closed states */
	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* test two terminal states */
	CELLP pcHeuristic					/* heuristic cost function */
)
{
	LINEARPLANP plpStart,plpEnd;		/* result listhead */
	LINEARPLANP plpS;					/* current new state */
	LINEARPLANP plpOld;
	LINEARPLANP plpNext;				/* next state */
	double dfNewCost;

	ENTER("FilterWorstOldStates",TRUE);
	StartTimer(tsTimers.adfCycleCheck);
	plpStart=NULL;						/* intialize listhead */
	plpEnd=(LINEARPLANP)&plpStart;

	for(plpS=plpNewStates;plpS;plpS=plpNext)
	{
		plpNext=plpS->plpNext;
		WorldSignature(plpS);
		dfNewCost=plpS->dfHeuristic;

		// if we want to support dynamic heuristic calculations, then
		// we need save all world in the crc hash table and check every
		// matching world's heuristic value.

		plpOld=CRCHashSearch(plpS);
		if(!plpOld||plpOld->dfHeuristic>dfNewCost)
		{
			if(plpOld)
			{
//				HeapDelete(plpOld);		/* we may be faster without this */
				CRCHashDelete(plpOld);
			}
			CRCHashInsert(plpS);
			plpEnd=plpEnd->plpNext=plpS;
		}
		else
			DiscardWorld(plpS);
	}
	plpEnd->plpNext=NULL;				/* terminate open list */
	StopTimer(tsTimers.adfCycleCheck);
	EXIT("FilterWorstOldStates");
	return plpStart;
}

//static LINEARPLANP FilterWorstOldStatesX
//(
//	LINEARPLANP plpNewStates,			/* list of new states to filter */
//	LINEARPLANP plpOpen,				/* list of open states */
//	LINEARPLANP plpClosed,				/* list of closed states */
//	BOOL (*pfStateEqQ)(LINEARPLANP, LINEARPLANP),	/* test two terminal states */
//	CELLP pcHeuristic					/* heuristic cost function */
//)
//{
//	LINEARPLANP plpStart,plpEnd;		/* result listhead */
//	LINEARPLANP plpS;					/* current new state */
//	LINEARPLANP plpOld;
//	LINEARPLANP plpNext;				/* next state */
//	double dfNewCost;
//
//	ENTER("FilterWorstOldStates",TRUE);
//	StartTimer(tsTimers.adfCycleCheck);
//	plpStart=NULL;						/* intialize listhead */
//	plpEnd=(LINEARPLANP)&plpStart;
//
//	for(plpS=plpNewStates;plpS;plpS=plpNext)
//	{
//		plpNext=plpS->plpNext;
//		WorldSignature(plpS);
//		dfNewCost=plpS->dfHeuristic;
//		for(plpOld=plpOpen;plpOld;plpOld=plpOld->plpNext)
//		{
//			if((*pfStateEqQ)(plpS,plpOld)&&plpOld->dfHeuristic<=dfNewCost)
//				break;
//		}
//		if(plpOld)
//			DiscardWorld(plpS);
//		else
//		{
//			for(plpOld=plpClosed;plpOld;plpOld=plpOld->plpNext)
//			{
//				if((*pfStateEqQ)(plpS,plpOld)&&plpOld->dfHeuristic<=dfNewCost)
//					break;
//			}
//			if(plpOld)
//				DiscardWorld(plpS);
//			else
//				plpEnd=plpEnd->plpNext=plpS;	/* state is cheaper, keep it */
//		}
//	}
//	plpEnd->plpNext=NULL;				/* terminate open list */
//	StopTimer(tsTimers.adfCycleCheck);
//	EXIT("FilterWorstOldStates");
//	return plpStart;
//}

/* CalculateHeuristic

Description:
	Calculate the heuristic cost of a plan.
*/

double CalculateHeuristic
(
	CELLP pcHeuristic,					/* heuristic cost function */
	LINEARPLANP plpPlan,				/* plan */
	BINDINGP pbBindings
)
{
	double df;

	StartTimer(tsTimers.adfHeuristic);
	if(pcHeuristic)
		FormulaToDouble(pcHeuristic,plpPlan,pbBindings,&df);
	else
		df=plpPlan->dfCost;				/* default is optimal-cost */
	StopTimer(tsTimers.adfHeuristic);
	return df;
}

/* CalculatePriority

Description:
	Calculate the priority of a plan.
*/

double CalculatePriority
(
	CELLP pcPriority,					/* cost function */
	LINEARPLANP plpPlan,				/* plan */
	BINDINGP pbBindings
)
{
	double df;

	StartTimer(tsTimers.adfPriority);
	if(pcPriority)
		FormulaToDouble(pcPriority,plpPlan,pbBindings,&df);
	else
		df=LinearPlanActionPriority(plpPlan);	/* default is best-action */
	StopTimer(tsTimers.adfPriority);
	return df;
}

/* DiscardWorld

Description:
	Discard a world, print a message if tracing is enabled.
Notes:
	The discarded world is freed... it may not be accessed again.
*/

static void DiscardWorld
(
	LINEARPLANP plp						/* world to discard */
)
{
	if(nTrace)
	{
		CommandPrintf(pfTraceStream,"-----------------------------------------"
			"---------------------------------------\n");
		CommandPrintf(pfTraceStream,"World %d at depth %d is redundant:  World discarded.\n\n",
			plp->nWorldNumber,LinearPlanLength(plp));
	}
//	CommandPrintf(stderr,"|");
	srSearchResult.nWorldsDiscarded++;
}

/* PruneOpenList

Description:
	We are about to garbage collect, so progress all worlds on the open
	list, so we can discard all non-viable worlds.
Note:
	We work in the scratch zone, because we're already out of memory.
*/

//static void PruneOpenList
//(
//	BINDINGP pbBindings
//)
//{
//	LINEARPLANP plp,plp1;
//	BOOL bTLForm;
//	CELLP pcTLForm;
//
//	ENTER("PruneOpenList",TRUE);
//	StartTimer(tsTimers.adfPruneOpenList);
//	pcTLForm=NULL;
//
//	plp1=(LINEARPLANP)&plpOpen;
//	for(plp=plpOpen;plp;plp=plp->plpNext)
//	{
//		SetZone(&zScratch);
//		if(bAtemporalControl)
//			bTLForm=(*pcTLForm->pfForm->paAction->pfEval)
//				(pcTLForm,plp,pbBindings);
//		else
//		{
//			pcTLForm=plp->pcTLForm;
//			bTLForm=(*pcTLForm->pfForm->paAction->pfCurrent)
//				(pcTLForm,plp,pbBindings);
//		}
//		SetZone(&zPermanent);
//		ZoneClear(&zScratch);
//
//		if(!bTLForm)
//		{
//			plp1->plpNext=plp->plpNext;	/* discard this world */
//			if(nTrace)
//			{
//				CommandPrintf(pfTraceStream,"-----------------------------------------"
//					"---------------------------------------\n");
//				CommandPrintf(pfTraceStream,"World %d at depth %d is non-viable:  World pruned.\n\n",
//					plp->nWorldNumber,LinearPlanLength(plp));
//			}
//			srSearchResult.nWorldsPruned++;
//		}
//		else
//			plp1=plp;
//	}
//	StopTimer(tsTimers.adfPruneOpenList);
//	EXIT("PruneOpenList");
//}

/* PruneList

Description:
	Prune a list of child worlds.
Note:
	We work in the scratch zone.
*/

static void PruneList
(
	LINEARPLANP *pplpList,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp,plp1;
//	BOOL bTLForm;
//	CELLP pcTLForm;
	CELLP pcProgressedTLForm;
	int immutable;						/* ignored */
//	int nWorlds;
//	int nOpen;

	ENTER("PruneList",TRUE);
//	nOpen=0;
//	nWorlds=nWorldsPruned;
	plp1=(LINEARPLANP)pplpList;
	if(bAtemporalControl)
		pcTLForm=plpInitialWorld->pcTLForm;
	for(plp=*pplpList;plp;plp=plp->plpNext)
	{
//		nOpen++;

//		StartTimer(tsTimers.adfProgress);
//		SetZone(&zScratch);
//		if(bAtemporalControl)
//			bTLForm=(*pcTLForm->pfForm->paAction->pfEval)
//				(pcTLForm,plp,pbBindings);
//		else
//		{
//			pcTLForm=plp->pcTLForm;
//			bTLForm=(*pcTLForm->pfForm->paAction->pfCurrent)
//				(pcTLForm,plp,pbBindings);
//		}
//		SetZone(&zPermanent);
//		ZoneClear(&zScratch);
//		StopTimer(tsTimers.adfProgress);

		StartTimer(tsTimers.adfProgress);
		pcTLForm=LinearPlanTLForm(plp);
		SetZone(&zScratch);
		pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
			(pcTLForm,plp,pbBindings,&immutable);
		SetZone(&zPermanent);
		ZoneCopyFormula(pcProgressedTLForm);
		ZoneReloc((void **)&pcProgressedTLForm);
		ZoneRelocFormula(pcProgressedTLForm);
		ZoneClear(&zScratch);
		StopTimer(tsTimers.adfProgress);
		
		if(FalseFormQ(pcProgressedTLForm))
//		if(!bTLForm)
		{
			plp1->plpNext=plp->plpNext;	/* discard this world */
			if(nTrace)
			{
				CommandPrintf(pfTraceStream,"-----------------------------------------"
					"---------------------------------------\n");
				CommandPrintf(pfTraceStream,"World %d at depth %d is non-viable:  World pruned.\n\n",
					plp->nWorldNumber,LinearPlanLength(plp));
			}
			srSearchResult.nWorldsPruned++;
		}
		else
		{
			plp1=plp;
			plp->pcTLForm=pcProgressedTLForm;		/* test!!! */
		}
	}
//	CommandPrintf(stderr,"%d of %d Worlds pruned from successor list.\n",
//		nWorldsPruned-nWorlds,nOpen);
	EXIT("PruneList");
}

/* ResetTimers -----------------------------------------------------------------

Description:
	Reset all of the timer statistics.
*/

static void ResetTimers(void)
{
	tsTimers.adfProgress[1]=0.0;
	tsTimers.adfProgressAtomic[1]=0.0;
	tsTimers.adfSuccessor[1]=0.0;
	tsTimers.adfHeuristic[1]=0.0;
	tsTimers.adfPriority[1]=0.0;
	tsTimers.adfBTreeLookUp[1]=0.0;
	tsTimers.adfBTreeModify[1]=0.0;
	tsTimers.adfCycleCheck[1]=0.0;
	tsTimers.adfUpdateOpenClose[1]=0.0;
	tsTimers.adfPruneOpenList[1]=0.0;
	tsTimers.adfGarbageCollect[1]=0.0;
}

/* PrintTimers

Description:
	Print the timing statistics for the available timers.
*/

void PrintTimers
(
	FILE *fs							/* file stream */ 
)
{
	CommandPrintf(fs,"Time progressing .............. %.3f\n",
		tsTimers.adfProgress[1]-tsTimers.adfProgressAtomic[1]);
	CommandPrintf(fs,"Time progressing atomics ...... %.3f\n",
		tsTimers.adfProgressAtomic[1]);
	CommandPrintf(fs,"Total time progressing ........ %.3f\n",
		tsTimers.adfProgress[1]);
	CommandPrintf(fs,"Time generating successors .... %.3f\n",
		tsTimers.adfSuccessor[1]);
	CommandPrintf(fs,"Time calculating heuristics ... %.3f\n",
		tsTimers.adfHeuristic[1]);
	CommandPrintf(fs,"Time calculating priorities ... %.3f\n",
		tsTimers.adfPriority[1]);
	CommandPrintf(fs,"Time searching btrees ......... %.3f\n",
		tsTimers.adfBTreeLookUp[1]);
	CommandPrintf(fs,"Time modifying btrees ......... %.3f\n",
		tsTimers.adfBTreeModify[1]);
	CommandPrintf(fs,"Time cycle checking ........... %.3f\n",
		tsTimers.adfCycleCheck[1]);
	CommandPrintf(fs,"Time updating open/closed lists %.3f\n",
		tsTimers.adfUpdateOpenClose[1]);
	CommandPrintf(fs,"Time pruning open list ........ %.3f\n",
		tsTimers.adfPruneOpenList[1]);
	CommandPrintf(fs,"Time collecting garbage ....... %.3f\n",
		tsTimers.adfGarbageCollect[1]);
	CommandPrintf(fs,"Time allocating memory ........ %.3f\n\n",
		tsTimers.adfMemoryAllocation[1]);
}

