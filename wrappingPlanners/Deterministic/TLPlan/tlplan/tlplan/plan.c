/* plan.c

Copyright C, 1996 - 2001	F. Bacchus

Plan

 The planner itself.

 a. Data structures for Planner. Access Functions.
 b. Plan successor function. Search occurs among the space of totally
		ordered forward chaining plans. Hence the world state that exists
		at plan termination is fully known.
 c. Plan equality test (equality if terminal state is identical).
 d. Plan termination test (terminate if terminal state satisfies goal).

-----------------------------------------------------------------------
 A plan is a linear sequence of plan "states"
 Each plan "state" consists of the triple world, tlFormula, action
 A World is a STRIPS state.
 A temporalFormula is a temporal formula used to prune the search.
 An action is an action structure defined by the actionInterface code

 Later we might want to explore nonLinear plans
 set up a layer of abstraction to facilitate this
 We need a lot of dynamically created plan steps so use a more efficient
 constructor.
*/

#include <assert.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "btree.h"
#include "crchash.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "oper.h"
#include "plan.h"
#include "progress.h"
#include "queue.h"
#include "save.h"
#include "search.h"
#include "tllex.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "world.h"
#include "zone.h"

//#include "adl.h"
//#include "internal.h"

/* local function prototypes */

LINEARPLANP FollowLinearPlan
(
	LINEARPLANP plpPlan					/* list of plans */
);
static BOOL PlanGoalQ
(
	LINEARPLANP plpPlan
);
static LINEARPLANP MakeLinearPlan
(
	LINEARPLANP plpWorldAction,			/* partially filled in linear plan */
	CELLP pcTLForm,						/* temporal control formula */
	CELLP pcCCForm,						/* current control formula */
	double dfDuration,					/* total duration */
	double dfCost,						/* total cost */
	int nLength,						/* plan depth */
	LINEARPLANP plpParent,				/* pointer to parent plan */
	BINDINGP pbBindings					/* bindings list */
);
static void LogStatistics
(
	char *psStatus,						/* status string */
	int nWorldsGenerated,				/* number of worlds generated */
	int nWorldsSearched,				/* number of worlds searched */
	int nWorldsPruned,					/* number of worlds pruned */
	int nWorldsDiscarded,				/* number of worlds discarded */
	int nWorldsUnexamined,				/* number of worlds unexamined */
	int nPlanLength,					/* total plan length in steps */
	double dfPlanCost,					/* total plan cost */
	double dfCPUTime					/* elapsed cpu time */
);
#ifdef _DEBUG
static void PlanSizeOf
(
	LINEARPLANP plpList					/* list of linear plans */ 
);
#endif // _DEBUG
static void InitMacroExpandPlan(void);
static BOOL MacroExpandPlan
(
	LINEARPLANP plpFinal,				/* pointer to final world of current plan or subplan */
	SEARCHP *ppfSearch,					/* returned search strategy */
	BINDINGP pbBindings					/* global bindings list */
);
//static void PrintPddlPlan
//(
//	LINEARPLANP plpSuccessors			// plan list
//);
static BOOL BeforePlanning(void);
static BOOL FormulaCallsQ
(
	CELLP pcFormulaList,				/* formula-list to check */
	ACTIONP paAction					/* action to find */
);

/* local data */

/* global data */

SEARCHRESULT srSearchResult;			/* result of last planning session */
double dfPddlTime;						/* pddl planning time */

/* -----------------------------------------------------------------------------
 The planner itself.
 You don't want to call this directly unless you know what you are
 doing.

 It takes the following arguments.
 startWorldAction --- a worldAction structure (as set up by worlds.cl)
 temporalControl a temporal formula to be used for search control
 It assumes that "finalWorldGoal?" has been properly configured
 to detect the achievement of the particular goal being searched for

 The caller must also set up the functions
 searchFn	--- return the search mode used.
 priorityFn	--- return the depthFirst priority function.
 costFn		--- return the bestFirst cost function.

 Returns a SOLUTION information structure.
*/

/* TLPlanner

Description:
Scheme:

(define (tlplanner start-world/action tlform search-fn priority-fn cost-fn)
	(let* ((initial-plan
				(make-plan start-world/action
					tlform 0 0 0 '()))
			(search-result '())
			(plan-time 0))
		(if *verbose*
			(begin
				(format *trace-stream* "~%----------------------------------~
					------------------------------------")
				(format *trace-stream* "~%Planner invoked. Initial world:")
				(print-world start-world/action *trace-stream*)
				(format *trace-stream* "~%Goal is:")
				(format *trace-stream* "~%~S" (get-goal))))
		(set! plan-time (get-internal-run-time))
		(set! *pruned-worlds* 0)
		(set! search-result
			(search-fn initial-plan plan-goal? plan-successor-fn plan=
				cost-fn priority-fn (get-search-limit)))
		(set! plan-time (- (get-internal-run-time) plan-time))

		(if *verbose*
			(begin
				(format *trace-stream*
					"~%Planning completed. CPU time taken ~%~S sec."
					(round (/ plan-time internal-time-units-per-second)))
				(cond
					((eq? (first search-result) 'RESOURCES)
						(format *trace-stream*
							"~%Resource bounds exhausted, no plan found."))
					((eq? (first search-result) 'NO-PLAN)
						(format *trace-stream* "~%All possible plans exhaused. ~
							Goal is unachievable."))
					(else
						(format *trace-stream* "~%~%Found plan, final world:")
						(print-world (plan-world/action (first search-result))
						*trace-stream*)))

			(format *trace-stream* "~%Worlds searched  = ~S"
				(- (third search-result) *pruned-worlds*))
			(format *trace-stream* "~%Worlds pruned by temporal control = ~S"
				*pruned-worlds*)
			(format *trace-stream* "~%Worlds unexamined  = ~S"
				(second search-result))
			(if (not (or (eq? (first search-result) 'RESOURCES)
						(eq? (first search-result) 'NO-PLAN)))
				(begin
					(format *trace-stream* "~%Plan length = ~A"
						(plan-length (first search-result)))
					(do ((time (get-initial-time))
							(successors (rest (plan-world/actions
										(first search-result))))
							(successor '()))
						((null? successors)
							(format *trace-stream* "~%At time ~A: Plan completed"
								time))
						(set! successor (first successors))
						(set! successors (rest successors))
						(format *trace-stream* "~%At time ~A: ~A"
							time (world/action-action-name successor))
						(set! time (+ time (world/action-action-duration
							successor))))
					;;Return the planning time, and search-result.
					(format *trace-stream* "~%Plan cost: ~A"
						(plan-cost (first search-result)))))
				(format *trace-stream* "~%-----------------------------------------~
					-----------------------------")))
		(list plan-time (first search-result)
			(- (third search-result) *pruned-worlds*)
			*pruned-worlds* (second search-result))))
*/

BOOL TLPlanner
(
	LINEARPLANP plpStartWorldAction,
	CELLP pcTLForm,
	SEARCHP pfSearch,
	CELLP pcHeuristic,
	CELLP pcPriority,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcCCForm;						// initial current control formula
	double dfPlanTime;					// time spent searching
	double dfTime;
	int nState;
	LINEARPLANP plpSuccessor;
	LINEARPLANP plpSuccessors;
	char *psStatus;						/* search status */
	BOOL bImmutable;					// ignored

	ENTER("TLPlanner",FALSE);

	/* let's issue some diagnostics */

	if(!BeforePlanning())
		return FALSE;
	
	/* prolog */

	dfPlanTime=GetInternalRunTime();	/* start our timer */

	/* garbage collect before we get started */
	
//	MarkBindings(pbBindings);
//	MarkDomainData();
//	if(bConcurrentPlanning)
//		MarkTimeQueue();
//	CollectGarbage();

	// initialize search result (statistics)
	
	srSearchResult.plpState=NULL;		/* final state */
	srSearchResult.psPlanStatus=apsStringTab[STRING_NO_PLAN];	/* plan status */
	srSearchResult.nWorldsGenerated=0;	/* worlds generated */
	srSearchResult.nWorldsSearched=0;	/* worlds searched */
	srSearchResult.nWorldsPruned=0;		/* worlds pruned */
	srSearchResult.nWorldsDiscarded=0;	/* worlds discarded */
	srSearchResult.nWorldsUnexamined=0;	/* worlds unexamined */
	srSearchResult.nSearchMaxDepth=0;	/* maximum search depth */
	srSearchResult.dfSearchMaxHeuristic=0.0;	/* maximum heuristic cost */
	srSearchResult.dfPlanCPUTime=0.0;	/* plan search time */
	srSearchResult.dfPlanCost=0.0;		/* plan cost */
	srSearchResult.nPlanLength=0;		/* plan length */
	srSearchResult.nOpenLength=0;		/* open length */
	srSearchResult.nClosedLength=0;		/* closed length */

	InitMacroExpandPlan();				/* initialize machinery for macro expansions */

	/* calculate the initial current control formula */

	StartTimer(tsTimers.adfProgress);
	SetZone(&zScratch);
	pcCCForm=(pcTLForm->pfForm->paAction->pfMakeCCForm)(FALSE,pcTLForm,&bImmutable);
	SetZone(&zPermanent);
	if(pcCCForm)
	{
		ZoneCopyFormula(pcCCForm);
		ZoneReloc((void **)
			&pcCCForm);
		ZoneRelocFormula(pcCCForm);
	}
	ZoneClear(&zScratch);
	StopTimer(tsTimers.adfProgress);
	
	/* initialize the open and closed lists */

	plpOpen=MakeLinearPlan(plpStartWorldAction,pcTLForm,pcCCForm,0,0,0,NULL,pbBindings);	/* associate the control formula with the initial world */
	plpOpen->plpNext=NULL;				/* discard any previous plan! */
	plpClosed=NULL;
	CRCHashClear();

	do
	{
		if(bVerbose)
		{
			CommandPrintf(pfTraceStream,"-----------------------------------------"
				"---------------------------------------\n");
			if(!nTrace)
			{
				CommandPrintf(pfTraceStream,"Initial world: \n");
				PrintWorld(STDLOG,plpStartWorldAction,pbBindings);
				CommandPrintf(pfTraceStream,"\n");
			}
			if(pfProcessGoal==ProcessExtendedGoal)
			{
				CommandPrintf(pfTraceStream,"Goal formula is: \n");
				PrintFormula(pfTraceStream,pcTLForm,0);
			}
			else
			{
				CommandPrintf(pfTraceStream,"Initial control formula is: \n");
				PrintFormula(pfTraceStream,pcTLForm,0);
				if(pcGoalFormula)
				{
					CommandPrintf(pfTraceStream,"\nGoal formula is: \n");
					PrintFormula(pfTraceStream,pcGoalFormula,0);
				}
			}
			CommandPrintf(pfTraceStream,"\n");
		}

		/* initialize any search global variables */

		for(pc=pcSearchGlobalInit;pc;pc=pc->pcNext)
			(*pc->pfForm->paAction->pfEval)(pc,plpOpen,pbGlobalVariables);

		/* execute the plan */

		psStatus=(*pfSearch)(
			PlanGoalQ,						// goal obtained
			PlanSuccessorFn,				// successor function
			pfPlanEqQ,						// state comparison function
			pcHeuristic,					// heuristic formula
			pcPriority,						// priority formula
			nSearchLimit,					// search limit
			pbBindings);					// bindings list

	}
	while(!psStatus&&MacroExpandPlan(srSearchResult.plpState,	// loop to expand macros
		&pfSearch,pbBindings));

	/* epilog */

	dfPlanTime=GetInternalRunTime()-dfPlanTime;
//	CommandPrintf(pfTraceStream,"Successor Time %f\n",dfSuccessorTime);
//	CommandPrintf(pfTraceStream,"Generator Time %f\n",dfGeneratorTime);
	if(bVerbose)
	{
		CommandPrintf(pfTraceStream,"-----------------------------------------"
			"---------------------------------------\n");
		if(psStatus)
		{
			CommandPrintf(stderr,"\nPlanning terminated.  No plan found.  Elapsed CPU time %.3f sec.\n",
				dfPlanTime);
			CommandPrintf(pfTraceStream,"Planning terminated.  No plan found.  Elapsed CPU time %.3f sec.\n",
				dfPlanTime);
			if(StringEqQ(psStatus,apsStringTab[STRING_RESOURCES]))
			{
				CommandPrintf(stderr,"Resource bounds exhausted.\n\n");
				CommandPrintf(pfTraceStream,"Resource bounds exhausted.\n\n");
			}
			else if(StringEqQ(psStatus,apsStringTab[STRING_NO_PLAN]))
			{
				CommandPrintf(stderr,"All possible plans exhausted.  Goal is unachievable.\n\n");
				CommandPrintf(pfTraceStream,"All possible plans exhausted.  Goal is unachievable.\n\n");
			}
			else if(StringEqQ(psStatus,apsStringTab[STRING_USER_ABORT]))
			{
				CommandPrintf(stderr,"Planner aborted by user.\n\n");
				CommandPrintf(pfTraceStream,"Planner aborted by user.\n\n");
			}
			else
			{
				CommandPrintf(stderr,"Unknown status: %s.\n\n",psStatus);
				CommandPrintf(pfTraceStream,"Unknown status: %s.\n\n",psStatus);
			}
		}
		else
		{
			CommandPrintf(stderr,"\nPlanning completed.  Plan found.  Elapsed CPU time %.3f sec.\n\n",
				dfPlanTime);
			CommandPrintf(pfTraceStream,"Plan found.  Elapsed CPU time %.3f sec.\n",
				dfPlanTime);
			if(bTimingStatistics)
				PrintTimers(pfTraceStream);
//			CommandPrintf(stdout,"Final world:\n\n");
//			debug_world(srSearchResult.plpState);
//			PrintWorld(STDOUT,srSearchResult.plpState,pbBindings);
			CommandPrintf(pfTraceStream,"Final world:\n\n");
			PrintWorld(STDLOG,srSearchResult.plpState,pbBindings);
		}
		CommandPrintf(pfTraceStream,"Worlds generated = %d\n",
			pfFinalWorldGoalQ==ClassicGoalQ?nWorldNumber-1:nWorldNumber);	/* don't count goal world */
		CommandPrintf(pfTraceStream,"Worlds searched = %d\n",
			srSearchResult.nClosedLength-srSearchResult.nWorldsPruned);
		CommandPrintf(pfTraceStream,"Worlds pruned by temporal control = %d\n",
			srSearchResult.nWorldsPruned);
		CommandPrintf(pfTraceStream,"Worlds discarded by cycle check = %d\n",
			srSearchResult.nWorldsDiscarded);
		CommandPrintf(pfTraceStream,"Worlds unexamined = %d\n",
			srSearchResult.nOpenLength);
	}

	if(!psStatus)					/* if success */
	{
		plpSuccessors=FollowLinearPlan(srSearchResult.plpState);
		if(nPddlSupport)
			PrintPddlPlan(plpSuccessors);
		if(bVerbose)
		{
			CommandPrintf(pfTraceStream,"Plan length = %d\n",
				LinearPlanLength(srSearchResult.plpState));
#ifdef _DEBUG
			PlanSizeOf(plpSuccessors);
#endif // _DEBUG
			if(bConcurrentPlanning)
			{
				nState=1;
				if(plpSuccessors)
				{
					for(plpSuccessor=plpSuccessors->plpNext;plpSuccessor;
						plpSuccessor=plpSuccessor->plpNext)
					{
						CommandPrintf(pfTraceStream,"% 3d At time %f: %d ",
							nState++,plpSuccessor->dfTime,
							plpSuccessor->nWorldNumber);
						PrintFlatFormula(pfTraceStream,plpSuccessor->pcActionName);
						CommandPrintf(pfTraceStream,"\n");
					}
					CommandPrintf(pfTraceStream,"Plan completed\n");
				}
			}
			else
			{
				dfTime=0.0;
				if(plpSuccessors)
				{
					for(plpSuccessor=plpSuccessors->plpNext;plpSuccessor;
						plpSuccessor=plpSuccessor->plpNext)
					{
						CommandPrintf(pfTraceStream,"At time %f: %d ",
							dfTime,plpSuccessor->nWorldNumber);
						PrintFormula(pfTraceStream,plpSuccessor->pcActionName,0);
						dfTime+=LinearPlanActionDuration(plpSuccessor);
					}
					CommandPrintf(pfTraceStream,"At time %f: Plan completed\n",dfTime);
				}
			}

			/* Display the planning cost. */

			CommandPrintf(pfTraceStream,"Plan cost: %f\n",
				srSearchResult.plpState->dfCost);
			CommandPrintf(pfTraceStream,"-----------------------------------------"
				"---------------------------------------\n");
		}
	}

	/* fill in the rest of the search result structure */

	srSearchResult.psPlanStatus=psStatus?psStatus:apsStringTab[STRING_OK];	/* plan status */
	srSearchResult.nWorldsGenerated=nWorldNumber-1;	/* worlds generated */
	srSearchResult.nWorldsSearched=srSearchResult.nClosedLength-srSearchResult.nWorldsPruned;	/* worlds searched */
	srSearchResult.nWorldsUnexamined=srSearchResult.nOpenLength;	/* worlds unexamined */
	srSearchResult.nPlanLength=LinearPlanLength(srSearchResult.plpState);	/* plan length */
	srSearchResult.dfPlanCost=LinearPlanCost(srSearchResult.plpState);	/* plan cost */
	srSearchResult.dfPlanCPUTime=dfPlanTime;	/* plan search time */
	
	LogStatistics(
		srSearchResult.psPlanStatus,	/* plan status */
		srSearchResult.nWorldsGenerated,	/* worlds generated */
		srSearchResult.nWorldsSearched,	/* worlds searched */
		srSearchResult.nWorldsPruned,	/* worlds pruned */
		srSearchResult.nWorldsDiscarded,	/* worlds discarded */
		srSearchResult.nWorldsUnexamined,	/* worlds unexamined */
		srSearchResult.nPlanLength,		/* plan length */
		srSearchResult.dfPlanCost,		/* plan cost */
		srSearchResult.dfPlanCPUTime);	/* plan search time */

	/* save the final plan for later interrogation */
	
	if(!psStatus)						/* on success */
		plpFinalPlan=srSearchResult.plpState;

	// clean up
	
//	dfGCTime=GetInternalRunTime();		// debug
	MarkBindings(pbBindings);
	MarkDomainData();
	MarkPlanList(plpClosed);
	MarkPlanList(plpOpen);
	if(bConcurrentPlanning)
		MarkTimeQueue();
//	dfGCTime=GetInternalRunTime()-dfGCTime;	// debug
//	CommandPrintf(stderr,"Memory Marking Time %.3f\n",dfGCTime);	// debug
//	dfGCTime=GetInternalRunTime();		// debug
	CollectGarbage();
//	dfGCTime=GetInternalRunTime()-dfGCTime;	// debug
//	CommandPrintf(stderr,"Garbage Collection Time %.3f\n",dfGCTime);	// debug

//	CommandPrintf(stderr,"Delayed action time %f\n",dfDelayedActionTime);
//	CommandPrintf(stderr,"Delayed action count %d\n",nDelayedAction);
//	CommandPrintf(stderr,"Wait for next event time %f\n",dfWaitEventTime);
//	CommandPrintf(stderr,"Wait for next event count %d\n",nWaitEvent);

	nWorldNumber=0;						/* reset this for next time */
	EXIT("TLPlanner");
	return !psStatus;					/* true if plan created */
}

/* BeforePlanning

Description:
	Do some sanity checks before planning to save the user some grief.
*/

static BOOL BeforePlanning(void)
{
	MACROOPERATORP pmo;
	OPERATORP po;
	char ac[128];

	if(pcHeuristicFn)					/* heuristic cost function */
	{
		if(!pcHeuristicFn->pfForm->uValue.psiSymbolInfo->pcFormula)
		{
			ErrorMessage("Heuristic function declared, but has no definition (no body): %s\n",
				pcHeuristicFn->pfForm->psName);
			return FALSE;
		}
	}
	if(pcPriorityFn)					/* priority function */
	{
		if(!pcPriorityFn->pfForm->uValue.psiSymbolInfo->pcFormula)
		{
			ErrorMessage("Priority function declared, but has no definition (no body): %s\n",
				pcPriorityFn->pfForm->psName);
			return FALSE;
		}
	}
	if(pcPrintWorld)					/* print world predicate */
	{
		if(!pcPrintWorld->pfForm->uValue.psiSymbolInfo->pcFormula)
		{
			ErrorMessage("Print world predicate declared, but has no definition (no body): %s\n",
				pcPrintWorld->pfForm->psName);
			return FALSE;
		}
	}
	if(pcGoalAddendum)					/* goal addendum predicate */
	{
		if(!pcGoalAddendum->pfForm->uValue.psiSymbolInfo->pcFormula)
		{
			ErrorMessage("Goal addendum predicate declared, but has no definition (no body): %s\n",
				pcGoalAddendum->pfForm->psName);
			return FALSE;
		}
	}
		
	// make sure all macro operators have bodies
	
	for(pmo=pmoMacroOperators;pmo;pmo=pmo->pmoNext)
	{
		for(po=poOperators;po;po=po->poNext)
		{
			if(StringEqQ(pmo->psName,GetName(po->pcName->pfForm,ac)))
				break;
		}
		if(!po)
		{
			ErrorMessage("Macro operator declared, but has no definition (no body): %s\n",
				pmo->psName);
			return FALSE;
		}
	}

	// make sure we have a wait-for-next-event if we're concurrent planning

	if(bConcurrentPlanning)
	{
		for(po=poOperators;po;po=po->poNext)
		{
			if(FormulaCallsQ(po->pcSuccessor,&aWaitForNextEventAction))
				break;
		}
		if(!po)
		{
			ErrorMessage("Concurrent domain does not call wait-for-next-event inside any operator\n");
			return FALSE;
		}
	}
	return TRUE;
}
	
/* InitMacroExpandPlan

Description:
	Initialize the macro-expansion local variables.
*/

static LINEARPLANP plpPrefixTail;		/* pointer to plan prefix final world */
static LINEARPLANP plpMacroWorld;		/* pointer to world being expanded */
static LINEARPLANP plpSuffix;			/* pointer to plan suffix */
LINEARPLANP plpSuffixTail;				/* pointer to final world (macro expansion) */
static char *psMacroDomain;				/* current macro expansion domain */

static void InitMacroExpandPlan(void)
{
	plpPrefixTail=0;
	plpMacroWorld=0;
	plpSuffix=0;
	plpSuffixTail=0;
	psMacroDomain=0;
}

/* MacroExpandPlan

Description:
	Scan a plan, looking for macro-actions.  For each macro-action,
	make the macro-action's parent the initial micro-world, and the macro-action's
	successor the goal micro-world.  Release the open and closed lists, load the 
	associated domain and goal formula.  Run the planner.
Notes:
	We use a lot of (confusing) global pointers to maintain the state of the 
	plan when we split it apart for macro expansion.  The following diagram 
	shows what the pointers point to after we have split the plan but before 
	macro expansion.  The world pointed to by plpPrefixTail is our start world, 
	and plpMacroWorld points to our goal world.  The goal world is discarded 
	after macro expansion.

	X  X  X  X  X  X      X  X  X  X  X  X  X  X
	               ^      ^  ^                 ^
                   PT     MW S                 ST
*/

static BOOL MacroExpandPlan
(
	LINEARPLANP plpFinal,				/* pointer to final world of current plan or subplan */
	SEARCHP *ppfSearch,					/* returned search strategy */
	BINDINGP pbBindings					/* global bindings list */
)
{
	LINEARPLANP plp;
	MACROOPERATORP pmo;

	/* we don't belong here if there aren't any macros to expand */
	
	if(!pmoMacroOperators)
		return FALSE;

	/* if we've split open the plan, put it back together */

	if(plpPrefixTail)
	{
		if(plpPrefixTail==plpFinal)
			Message("Failed to expand macro action\n");
		else
		{
			if(plpSuffix)				/* if we have a suffix, append it to the new subplan */
				plpSuffix->plpParent=plpFinal;
			else
				plpSuffixTail=plpFinal;	/* our previous end of plan was this macro operator, we have a new end of plan */

			for(plpSuffix=plpFinal;plpSuffix->plpParent!=plpPrefixTail;plpSuffix=plpSuffix->plpParent);	/* new suffix is start of subplan */

		}
	}
	else								/* we're just getting started */
	{
		plpPrefixTail=plpFinal;			/* start search for next macro action here */
		plpSuffixTail=plpFinal;			/* tentative end of plan */
		plpSuffix=NULL;					/* final action may be a macro */
	}

	/* search backwards for the (next) operator to macro expand */

	pmo=NULL;
	for(plp=plpPrefixTail;plp;plp=plp->plpParent)
	{
		for(pmo=pmoMacroOperators;pmo;pmo=pmo->pmoNext)
		{
			if(!plp->pcActionName)		/* if initial world, we're done */
			{
				srSearchResult.plpState=plpSuffixTail;	/* return actual final world */
				return FALSE;
			}
			if(StringEqQ(pmo->psName,IdentName(plp->pcActionName->pfForm)))
			{
				plpMacroWorld=plp;
				plpPrefixTail=plp->plpParent;
				goto endloop;
			}
		}
		plpSuffix=plp;					/* suffix is world following macro world */
	}
endloop:;

	assert(pmo);
	assert(plpPrefixTail);

	if(nTrace>=3)
	{
		CommandPrintf(pfTraceStream,"\nMacro expanding world %d: ",plp->nWorldNumber);
		PrintFlatFormula(pfTraceStream,plpMacroWorld->pcActionName);
		CommandPrintf(pfTraceStream,"\n");
//		CommandPrintf(pfTraceStream,"\nParent world %d: ",plpPrefixTail->nWorldNumber);
//		PrintFlatFormula(pfTraceStream,plpPrefixTail->pcActionName);
//		CommandPrintf(pfTraceStream,"\n");

		CommandPrintf(stderr,"\nMacro expanding world %d: ",plp->nWorldNumber);
		PrintFlatFormula(stderr,plpMacroWorld->pcActionName);
		CommandPrintf(stderr,"\n");
//		CommandPrintf(stderr,"\nParent world %d: ",plpPrefixTail->nWorldNumber);
//		PrintFlatFormula(stderr,plpPrefixTail->pcActionName);
//		CommandPrintf(stderr,"\n");
	}

	/* load the domain if it has changed */

	if(!StringEqQ(pmo->psDomain,psMacroDomain))
	{
		LoadFile(pmo->psDomain);
		psMacroDomain=pmo->psDomain;
	}

	/* set up the initial and goal worlds */

	plpInitialWorld=plpPrefixTail;
	plpGoalWorld=plpMacroWorld;
	pcGoalFormula=pmo->pcGoal;

	/* handle any initialization sequence */

	InitializeMacroWorld(plpInitialWorld,pbBindings);

/* we need to handle pcTLForm and pcCCForm issues here! */
	
	/* initialize the open and closed lists */

	plpOpen=plpInitialWorld;			/* discard any previous context */
	plpOpen->plpNext=NULL;
	plpClosed=NULL;
	CRCHashClear();

	*ppfSearch=pfSearchStrategy;		/* search strategy may have changed */
	return TRUE;
}

/* MakeLinearPlan

Description:
	Fill in the remaining plan fields (of a world/action)
Scheme:

(define (make-linear-plan world/action tlform duration
			  cost length user previous)
  (vector 'linear-plan world/action tlform duration
	  cost length user previous))
*/

static LINEARPLANP MakeLinearPlan
(
	LINEARPLANP plpWorldAction,
	CELLP pcTLForm,						/* temporal control formula */
	CELLP pcCCForm,						/* current control formula */
	double dfTime,						/* total time */
	double dfCost,						/* total cost */
	int nLength,						/* plan depth */
	LINEARPLANP plpParent,				/* pointer to parent plan */
	BINDINGP pbBindings					/* bindings list */
)
{
	ENTER("MakeLinearPlan",FALSE);
	plpWorldAction->pcTLForm=pcTLForm;
	plpWorldAction->pcCCForm=pcCCForm;
	if(!bConcurrentPlanning)			/* don't overwrite concurrent duration */
		plpWorldAction->dfTime=dfTime;
	plpWorldAction->dfCost=dfCost;
	plpWorldAction->nLength=nLength;
	plpWorldAction->plpParent=plpParent;

	if(pfSearchStrategy==BestFirst||pfSearchStrategy==DepthBestFirst)
		plpWorldAction->dfHeuristic=CalculateHeuristic(pcHeuristicFn,plpWorldAction,pbBindings);
	if(pfSearchStrategy==BreadthFirstPriority||pfSearchStrategy==DepthFirstPriority)
		plpWorldAction->dfPriority=CalculatePriority(pcPriorityFn,plpWorldAction,pbBindings);
	
	EXIT("MakeLinearPlan");
	return plpWorldAction;
}

/* MarkLinearPlan

Description:
	Mark a plan (for garbage collection)
*/

void MarkLinearPlan
(
	LINEARPLANP plpLinearPlan
)
{
	ENTER("MarkLinearPlan",TRUE);
	if(plpLinearPlan)
	{
		if(plpLinearPlan->apbtWorld)
		{
			MarkWorld(plpLinearPlan->apbtWorld);
			MarkFormula(plpLinearPlan->pcActionName);
		}
		else							// handle action promises
		{
			MarkBindings((BINDINGP)(plpLinearPlan->nSignature));	/* promises bindings list stored here! */
			MarkFormula(plpLinearPlan->pcPromise);
			if(nTrace>=3)				// successor worlds have labels for the trace
				MarkFormula(plpLinearPlan->pcActionName);
		}
		MarkFormula(plpLinearPlan->pcTLForm);
		if(plpLinearPlan->pcCCForm)
			MarkFormula(plpLinearPlan->pcCCForm);
		ZoneMark(plpLinearPlan);
	}
	EXIT("MarkLinearPlan");
}

/* CopyLinearPlan

Description:
	Copier
Scheme:

(define copy-linear-plan vector-copy)
*/

LINEARPLANP CopyLinearPlan
(
	LINEARPLANP plp
)
{
	LINEARPLANP plpCopy;

	ENTER("CopyLinearPlan",FALSE);
	plpCopy=(LINEARPLANP)CopyAlloc(plp,sizeof(struct LinearPlan));
	plpCopy->apbtWorld=CopyWorld(plp->apbtWorld);
	plpCopy->pcActionName=CopyCell(plp->pcActionName);
	plpCopy->pcPromise=CopyCell(plp->pcPromise);
	plpCopy->pcTLForm=CopyCell(plp->pcTLForm);
	EXIT("CopyLinearPlan");
	return plpCopy;
}

/* PlanCount -------------------------------------------------------------------

Description:
	Return the number of plans in a list.
*/

int PlanCount
(
	LINEARPLANP plpPlan
)
{
	int n;
	LINEARPLANP plp;

	ENTER("PlanCount",TRUE);
	n=0;
	for(plp=plpPlan;plp;plp=plp->plpNext)
		n++;
	EXIT("PlanCount");
	return n;
}

/* -----------------------------------------------------------------------------
 To extend a plan we examine the final world
 1. Progress the temporal formula through the world.
 2. If the progressed formual is not NULL:
 3. Generate all successor worlds by applying all applicable actions.
 4. Build up the plan extensions.
*/

/* PlanExtend

Description:
	Extend a plan by appending the new worldAction struct to it.
Scheme:

(define (plan-extend new-world/action new-tlform plan)
	(make-plan new-world/action new-tlform
			   (+ (world/action-action-duration new-world/action)
		  (plan-duration plan))
			   (+ (world/action-action-cost new-world/action) (plan-cost plan))
			   (+ 1 (plan-length plan))
			   plan))
*/

LINEARPLANP PlanExtend
(
	LINEARPLANP plpNew,					/* new plan */
	CELLP pcTLNew,						/* new temporal control formula */
	CELLP pcCCNew,						/* new current control formula */
	LINEARPLANP plpPlan,				/* progenitor plan */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plp;

	ENTER("PlanExtend",TRUE);
	plp=MakeLinearPlan(plpNew,pcTLNew,pcCCNew,
		LinearPlanActionDuration(plpNew)+LinearPlanTime(plpPlan),
		LinearPlanActionCost(plpNew)+LinearPlanCost(plpPlan),
		LinearPlanLength(plpPlan)+1,plpPlan,pbBindings);
	EXIT("PlanExtend");
	return plp;
}

/* PlanExtendedEqQ -------------------------------------------------------------

Description:
	Plan equality test used in graph searcher routines.
	Two extended plans are equal if they lead to the same world, and
	the same control formula.
Scheme:

(defun plan-extended= (plan1 plan2)
  (and (world= (plan-world plan1) (plan-world plan2))
	   (equal (plan-tlform plan1) (plan-tlform plan2))))

*/

BOOL PlanExtendedEqQ
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
)
{
	BOOL b;

	ENTER("PlanExtendedEqQ",TRUE);
	b=WorldEqQ(plpPlan1,plpPlan2)&&
		FormulaEqQ(LinearPlanTLForm(plpPlan1),LinearPlanTLForm(plpPlan2));
	EXIT("PlanExtendedEqQ");
	return b;
}

/* PlanClassicEqQ

Description:
	Plan equality test used in graph searcher routines.
	Two classic plans are equal if they lead to the same world.
Scheme:

(define (plan-classic= plan1 plan2)
  (world= (plan-world plan1) (plan-world plan2)))
*/

BOOL PlanClassicEqQ
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
)
{
	BOOL b;

	ENTER("PlanClassicEqQ",TRUE);
	b=WorldEqQ(plpPlan1,plpPlan2);
	EXIT("PlanClassicEqQ");
	return b;
}

/* The function pfPlanEqQ is set by the interface to deal with the
correct type of control formulas.
*/

/* PlanGoalQ -------------------------------------------------------------------

Description:
	Plan termination test used in graph searcher routines.
	Test if plan's final state achieves the goal.

	SetGoalType is essentially used to specify the routine pfFinalWorldGoal,
	used to detect the goal world.  SetGoal is used to specify the formula
	interpreted by pfFinalWorldGoal.

	If we have a classic goal we test for success by evaluating a conjunction
	of the final world goals.

	If we have an extended goal we test for success by idling the "progressed"
	goal temporal formula.  In this case, we start off with the goal and
	control formulas being the same.
Scheme:

(define (plan-goal? plan)
  (final-world-goal? (plan-tlform plan) (plan-world/action plan)))
*/

static BOOL PlanGoalQ
(
	LINEARPLANP plpPlan
)
{
	BOOL b;

	ENTER("PlanGoalQ",TRUE);
	b=(*pfFinalWorldGoalQ)(LinearPlanTLForm(plpPlan),plpPlan);
	EXIT("PlanGoalQ");
	return b;
}

/* FollowLinearPlan

Description:
	Make a forward list of plans (for display).
Note:
	We modify the normally unused plpNext pointers.
Scheme:

(define (plan-world/actions plan)
  (reverse (map-plan plan-world/action plan)))
*/

LINEARPLANP FollowLinearPlan
(
	LINEARPLANP plpPlan					/* list of plans */
)
{
	LINEARPLANP plpFirst,plp;

	ENTER("FollowLinearPlan",TRUE);
	plpFirst=NULL;						/* initialize list head */
	for(plp=plpPlan;plp;plp=plp->plpParent)
	{
		plp->plpNext=plpFirst;
		plpFirst=plp;
	}
	EXIT("FollowLinearPlan");
	return plpFirst;
}

/* LogStatistics

Description:
	If a statistics file has been named, attempt to open it and log
	this run.
*/

static void LogStatistics
(
	char *psStatus,						/* status string */
	int nWorldsGenerated,				/* number of worlds generated */
	int nWorldsSearched,				/* number of worlds searched */
	int nWorldsPruned,					/* number of worlds pruned */
	int nWorldsDiscarded,				/* number of worlds discarded */
	int nWorldsUnexamined,				/* number of worlds unexamined */
	int nPlanLength,					/* total plan length in steps */
	double dfPlanCost,					/* total plan cost */
	double dfCPUTime					/* plan search time */
)
{
	FILE *ofs;

	if(psStatisticsFile)
	{
		ofs=fopen(psStatisticsFile,"a");
		if(!ofs)
		{
			ErrorMessage("Failed to open statistics file %s\n",psStatisticsFile);
			return;
		}
		CommandPrintf(ofs,"\"%s\"",psPlanName?psPlanName:"");
		CommandPrintf(ofs,",\"%s\"",psStatus?psStatus:"OK");
		CommandPrintf(ofs,",%d,%d,%d,%d,%d,%d,%f,%.3f\n",
			nWorldsGenerated,nWorldsSearched,nWorldsPruned,nWorldsDiscarded,
			nWorldsUnexamined,nPlanLength,dfPlanCost,dfCPUTime);
		fclose(ofs);
	}
}

#ifdef _DEBUG
/* PlanSizeOf

Description:
	Print the sizes of all of the worlds and control formulas in a plan.
*/

static void PlanSizeOf
(
	LINEARPLANP plpList					/* list of linear plans */ 
)
{
	LINEARPLANP plp;
	int nWorldSize,nControlSize;
	int nWorldSum,nControlSum;
	int nCount;

//	CommandPrintf(pfTraceStream,"Incremental Sizes of all Worlds and Control Formulas in Plan\n");
	nWorldSum=0;
	nControlSum=0;
	nCount=0;
	for(plp=plpList;plp;plp=plp->plpNext)
	{
		nWorldSize=0;
		WorldSizeOf(plp->apbtWorld,&nWorldSize);
		nControlSize=0;
		FormulaSizeOf(plp->pcTLForm,&nControlSize);
//		CommandPrintf(pfTraceStream,"%d. world %d control %d\n",
//			plp->nWorldNumber,nWorldSize,nControlSize); 
		nWorldSum+=nWorldSize;
		nControlSum+=nControlSize;
		nCount++;
	}
	fprintf(stderr,"Average World Size %.1f bytes\n",(double)nWorldSum/nCount);
	fprintf(stderr,"Average Control Size %.1f bytes\n",(double)nControlSum/nCount);
}
#endif // _DEBUG

/* ZoneCopyLinearPlan

Description:
	ZoneCopy a plan (for garbage collection)
*/

//void ZoneCopyLinearPlan
//(
//	LINEARPLANP plpLinearPlan
//)
//{
//	ENTER("ZoneCopyLinearPlan",TRUE);
//	if(plpLinearPlan)
//	{
//		ZoneCopy(plpLinearPlan);
//		ZoneCopyWorld(plpLinearPlan->apbtWorld);
//		ZoneCopyFormula(plpLinearPlan->pcActionName);
//		ZoneCopyFormula(plpLinearPlan->pcPromise);
//		ZoneCopyFormula(plpLinearPlan->pcTLForm);
//	}
//	EXIT("ZoneCopyLinearPlan");
//}

/* ZoneRelocLinearPlan

Description:
	ZoneReloc a plan
*/

//void ZoneRelocLinearPlan
//(
//	LINEARPLANP plpLinearPlan
//)
//{
//	ENTER("ZoneRelocLinearPlan",TRUE);
//	if(plpLinearPlan)
//	{
//		ZoneReloc(&plpLineaarPlan->apbtWorld);
//		ZoneRelocWorld(plpLinearPlan->apbtWorld);
//		ZoneReloc(&plpLinearPlan->pcActionName);
//		ZoneRelocFormula(plpLinearPlan->pcActionName);
//		ZoneReloc(&plpLinearPlan->pcPromise);
//		ZoneRelocFormula(plpLinearPlan->pcPromise);
//		ZoneReloc(&plpLinearPlan->pcTLForm);
//		ZoneRelocFormula(plpLinearPlan->pcTLForm);
//	}
//	EXIT("ZoneRelocLinearPlan");
//}

void PrintPddlPlan
(
	LINEARPLANP plpSuccessors			// plan list
)
{
	FILE *ofs;							// output file stream
	ELIDEDOPERATORP peo;
	LINEARPLANP plpSuccessor;			// current world
	char acBuf[_MAX_PATH];				// format buffer
	double dfTime;						// default time increment
	char *ps;							// file name string pointer

	ps=strrchr(psPlanName,'/');			// strip path from pddl file name
	if(!ps)
		ps=psPlanName;					// no path, use full file name
	else
		ps++;							// skip over trailing slash
	sprintf(acBuf,"%s.soln",ps);
	ofs=fopen(acBuf,"w");
	if(!ofs)
	{
		ErrorMessage("Failed to open PDDL solution file: %s\n",ps);
		return;
	}
	
	dfTime=GetInternalRunTime()-dfPddlTime;	/* calculate total time */
	fprintf(ofs,";Time %d\n",(int)(dfTime*1000.0+0.5));
	if(bConcurrentPlanning)
	{
		if(plpSuccessors)
		{
			for(plpSuccessor=plpSuccessors->plpNext;plpSuccessor;
				plpSuccessor=plpSuccessor->plpNext)
			{
				/* discard events */

//				if(StringEqQ(plpSuccessor->pcActionName->pfForm->psName,apsStringTab[STRING_EVENT]))
				if(!strcmp(plpSuccessor->pcActionName->pfForm->psName,"event"))
					continue;

				/* discard elided actions */

				for(peo=peoElidedOperators;peo;peo=peo->peoNext)
					if(StringEqQ(plpSuccessor->pcActionName->pfForm->psName,peo->psName))
						goto endloop1;

				fprintf(ofs,"%f: ",
					plpSuccessor->dfTime+1.0);
				PrintFlatFormula(ofs,plpSuccessor->pcActionName);
				if(plpSuccessor->dfDuration)
					fprintf(ofs," [%f]\n",plpSuccessor->dfDuration);
				else
					fprintf(ofs,"\n");
endloop1:;
			}
		}
	}
	else
	{
		dfTime=1.0;
		if(plpSuccessors)
		{
			for(plpSuccessor=plpSuccessors->plpNext;plpSuccessor;
				plpSuccessor=plpSuccessor->plpNext)
			{
				/* discard elided actions */

				for(peo=peoElidedOperators;peo;peo=peo->peoNext)
					if(StringEqQ(plpSuccessor->pcActionName->pfForm->psName,peo->psName))
						goto endloop2;

				fprintf(ofs,"%f: ",dfTime);
				PrintFormula(ofs,plpSuccessor->pcActionName,0);
				dfTime+=LinearPlanActionDuration(plpSuccessor);
endloop2:;
			}
		}
	}
	fclose(ofs);
}

// -----------------------------------------------------------------------------

/* FormulaCallsQ

Description:
	Check a formula to see if it calls a given action.
*/

static BOOL FormulaCallsQ
(
	CELLP pcFormulaList,				/* formula-list to check */
	ACTIONP paAction					/* action to find */
)
{
	CELLP pc;							/* formula pointer */
	CELLP pcFormula;

	for(pcFormula=pcFormulaList;pcFormula;pcFormula=pcFormula->pcNext)
	{
		if(pcFormula->pfForm->paAction==paAction)
			return TRUE;
		for(pc=pcFormula->pfForm->pcVars;pc;pc=pc->pcNext)
		{
			if(FormulaCallsQ(pc,paAction))
				return TRUE;
		}
		for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
		{
			if(FormulaCallsQ(pc,paAction))
				return TRUE;
		}
		for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
		{
			if(FormulaCallsQ(pc,paAction))
				return TRUE;
		}
	}
	return FALSE;
}

