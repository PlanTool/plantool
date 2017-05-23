/* eval1.c

Copyright C, 1996 - 2002  F. Bacchus

Interface

 Functions to handle the interface.
*/

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "btree.h"
#include "color.h"
#include "compute.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "idle.h"
#include "iface.h"
#include "internal.h"
#include "isaac.h"
#include "makeform.h"
#include "makegen.h"
#include "oper.h"
#include "lexpddl.h"
#include "plan.h"
#include "queue.h"
#include "search.h"
#include "tllex.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "user.h"
#include "util.h"
#include "world.h"
#include "zone.h"

/* local function prototypes */

static void ProcessClassicGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
static void ResetHeuristicFn(void);
static void ResetPriorityFn(void);
static void ListDomains(void);
static void ResetGoalAddendum(void);

/* global data ----------------------------------------------------------------- */

DECLSPEC FILE *pfTraceStream;			/* output stream for trace information. */

CELLP pcTLForm;							/* the initial temporal control formula */
LINEARPLANP plpInitialWorld;			/* the initial world */
LINEARPLANP plpFinalPlan;				/* the final world (and plan) */
CELLP pcInitialFacts;					/* initial facts */
CELLP pcInitializationSequence;			/* initialization sequence */
CELLP pcGoalSequence;					/* goal sequence */
CELLP pcHeuristicFn;					/* heuristic cost function */
CELLP pcPriorityFn;						/* priority function */
CELLP pcPrintWorld;						/* print world predicate */
CELLP pcGoalAddendum;					/* goal addendum */

SEARCHP pfSearchStrategy=(SEARCHP)DepthFirstPriority;
BOOL (*pfPlanEqQ)(LINEARPLANP, LINEARPLANP)=PlanClassicEqQ;

CELLP pcGoalFormula=NULL;				/* conjunctive goal formula for (*pfFinalWorldGoalQ)() */
void (*pfProcessGoal)(CELLP, LINEARPLANP, BINDINGP)=ProcessClassicGoal;
FINALWORLDGOALQP pfFinalWorldGoalQ=ClassicGoalQ;
jmp_buf *pjbCurrentJump;				/* pointer to current longjmp context */
CELLP pcSSForm;							/* search-strategy formula */
char *psStatisticsFile;					/* statistics output file name */
jmp_buf jbTLPlannerJump;				/* planner longjmp context */

BOOL bAtemporalControl=FALSE;			/* use atemporal control formula */
BOOL bConcurrentPlanning=TRUE;			/* plan in concurrent mode */
BOOL bCycleChecking=TRUE;				/* cycle checking is enabled */
BOOL bTimingStatistics=FALSE;			/* timing statistics are enabled */
BOOL bBackTracking=TRUE;				/* search sibling worlds only */
BOOL bPruningAllSuccessors=FALSE;		/* prune worlds as they are created */
int nPddlSupport=PDDL_UNSUPPORTED;		/* support pddl problem input and plan output */

CELLP pcPredefinedPlan;					/* predefined plan for follow-predefined-plan */

RNG arRNGTable[]=
{
	{apsStringTab+STRING_FIBONACCI,
		FibRand,FibSeed,2147483647.0},
	{apsStringTab+STRING_MS,
		MSRand,MSSeed,32767.0},
	{apsStringTab+STRING_ISAAC,
		(int (*)(void))IsaacRand,(void (*)(int))IsaacSeed,4294967295.0}
};
RNGP prCurrentRNG=arRNGTable;

/* local data ------------------------------------------------------------------ */

/* NewMakeSymbolInfoForm

Description:
	Make a formula for an object registered in the symbol info table.
New:
	This version doesn't rely on lists.
*/

static CELLP NewMakeSymbolInfoForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcName,						/* name */
	CELLP pcArgs						/* list of arguments */
)
{
	CELLP pc,pc1;
	FORMULAP pf;
	int nLength;						/* number of arguments */
	SYMBOLINFOP psiSymbolInfo;
	char ac[40];

	ENTER("NewMakeSymbolInfoForm",TRUE);

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=pcName->pfForm->nType;
	pf->uValue=pcName->pfForm->uValue;
	pf->psName=pcName->pfForm->psName;

	psiSymbolInfo=GetSymbolInfoPtr(GetName(pf,ac));
	if(!psiSymbolInfo)
		ErrorMessage("NewMakeSymbolInfoForm:  Unknown symbol %s\n",GetName(pf,ac));
	else
	{
		if(psiSymbolInfo->nArity>=0)
		{
			nLength=0;					/* check arity */
			for(pc1=pcArgs;pc1;pc1=pc1->pcNext)
				nLength++;
			if(psiSymbolInfo->nArity!=nLength)
			{
				ErrorMessage("%s called with %d argument%s, requires %d\n",
					pf->psName,nLength,nLength==1?"":"s",psiSymbolInfo->nArity);
				EXIT("NewMakeSymbolInfoForm");
				return NULL;
			}
		}	
		pf->paAction=psiSymbolInfo->paAction;
	}
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("NewMakeSymbolInfoForm");
	return pc;
}

/* Search Interface ------------------------------------------------------------ */

/* The number of nodes expanded by the planner. */

int nSearchLimit=100000;

/* EvalSetSearchLimit (Interface Command)

Description:
	Set the number of nodes expanded by the planner.
Scheme:

(define (setSearchLimit limit)
	(set! *searchLimit* limit))
*/

BOOL EvalSetSearchLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int n;
	BOOL bStatus;

	ENTER("EvalSetSearchLimit",FALSE);

	bStatus=FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&n);
	if(bStatus)
		nSearchLimit=n;	
	EXIT("EvalSetSearchLimit");
	return bStatus;
}

/* ComputeGetSearchLimit (Interface Command)

Description:
	Return the planner's current search limit.
Scheme:

(define (getSearchLimit )
	*searchLimit*)
*/

CELLP ComputeGetSearchLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetSearchLimit",FALSE);
	pc=MakeIntegerForm(nSearchLimit);
	EXIT("ComputeGetSearchLimit");
	return pc;
}

/* EvalResetSearchLimit (Interface Command)

Description:
	Reset the search limit to its default value.
Scheme:

(define (resetSearchLimit )
	(set! *searchLimit* 10000))
*/

BOOL EvalResetSearchLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetSearchLimit",FALSE);
	nSearchLimit=100000;
	EXIT("EvalResetSearchLimit");
	return TRUE;
}

/* Strategy Interface ---------------------------------------------------------- */

struct StrategyTab
{
	char **ppsName;						/* strategy keyword */
	SEARCHP pfStrategy;					/* strategy code */
};

static struct StrategyTab astStrategyTable[]=
{
	{apsStringTab+STRING_BREADTH_FIRST,BreadthFirst},
	{apsStringTab+STRING_BREADTH_FIRST_PRIORITY,BreadthFirstPriority},
	{apsStringTab+STRING_BEST_FIRST,BestFirst},
	{apsStringTab+STRING_DEPTH_FIRST,(SEARCHP)DepthFirst},
	{apsStringTab+STRING_DEPTH_FIRST_PRIORITY,(SEARCHP)DepthFirstPriority},
	{apsStringTab+STRING_DEPTH_BEST_FIRST,(SEARCHP)DepthBestFirst},
	{apsStringTab+STRING_DEPTH_FIRST_NO_BACKTRACKING,(SEARCHP)DepthFirstNoBacktracking}
};

/* EvalSetSearchStrategy (Interface Command)

Description:
	Set the search strategy to be used by the planner.
Scheme:

(define search-strategy depth-first-priority)
(define (set-search-strategy strategy)
  "Set the search strategy to be used by the planner."
  (cond ((eqv? strategy 'depth-first-priority)
	 (set! search-strategy depth-first-priority))
	((eqv? strategy 'depth-first)
	 (set! search-strategy depth-first))
	((eqv? strategy 'breadth-first-priority)
	 (set! search-strategy breadth-first-priority))
	((eqv? strategy 'breadth-first)
	 (set! search-strategy breadth-first))
	((eqv? strategy 'best-first)
	 (set! search-strategy best-first))
	((eqv? strategy 'id-search)
	 (set! search-strategy id-search))
	(else
	 (format #t "~%Search strategy must be one of the symbols:~
					~%depth-first-priority~
					~%breadth-first-priority~
					~%depth-first~
					~%breadth-first~
					~%best-first~
					~%id-search"))))
*/

BOOL EvalSetSearchStrategy
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char *psStrategy;
	int i;

	/* check user input */

	ENTER("EvalSetSearchStrategy",FALSE);
	pc=pcFormula->pfForm->pcArgs;
	psStrategy=IdentName(pcFormula->pfForm->pcArgs->pfForm);
	for(i=0;i<sizeof(astStrategyTable)/sizeof(struct StrategyTab);i++)
	{
		if(StringEqQ(psStrategy,*astStrategyTable[i].ppsName))
		{
			pfSearchStrategy=astStrategyTable[i].pfStrategy;
			pcSSForm=CopyCell(pc->pcNext);
			EXIT("EvalSetSearchStrategy");
			return TRUE;
		}
	}
	ErrorMessage("set-search-strategy:  Unknown strategy \"%s\".\n"
		"Search strategy must be one of the following: \n"
		"depth-first, depth-first-priority, depth-best-first, \n"
		"breadth-first, breadth-first-priority, or best-first.\n",psStrategy);
	EXIT("EvalSetSearchStrategy");
	return FALSE;
}

/* ComputeGetSearchStrategy (Interface Command)

Description:
	Return the current search strategy.
Scheme:

(define (get-search-strategy )
  search-strategy)
*/

CELLP ComputeGetSearchStrategy
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int i;

	ENTER("ComputeGetSearchStrategy",FALSE);
	for(i=0;i<sizeof(astStrategyTable)/sizeof(struct StrategyTab);i++)
	{
		if(pfSearchStrategy==astStrategyTable[i].pfStrategy)
		{
			pc=MakeStringForm(*astStrategyTable[i].ppsName);
			EXIT("ComputeGetSearchStrategy");
			return pc;
		}
	}
	ErrorMessage("get-search-strategy:  Search strategy is not known.\n");
	EXIT("ComputeGetSearchStrategy");
	return NULL;
}

/* EvalResetSearchStrategy (Interface Command)

Description:
	Reset the search strategy to the default.
Scheme:

(define (reset-search-strategy )
  (reset-priority-fn)
  (set! search-strategy depth-first-priority))
*/

BOOL EvalResetSearchStrategy
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetSearchStrategy",FALSE);
	ResetPriorityFn();
	pfSearchStrategy=(SEARCHP)DepthFirstPriority;
	EXIT("EvalResetSearchStrategy");
	return TRUE;
}

/* Random number generator selection ---------------------------------------- */

/* EvalSetRNG (Interface Command)

Description:
	Set the underlying random number generator.
*/

BOOL EvalSetRNG
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i;
	CELLP pc;
	char *psRNG;

	ENTER("EvalSetRNG",FALSE);

	pc=pcFormula->pfForm->pcArgs;
	psRNG=IdentName(pc->pfForm);
	for(i=0;i<sizeof(arRNGTable)/sizeof(RNG);i++)
	{
		if(StringEqQ(psRNG,*arRNGTable[i].ppsName))
		{
			prCurrentRNG=arRNGTable+i;
			EXIT("EvalSetRNG");
			return TRUE;
		}
	}
	ErrorMessage("Unknown random number generator: %s\n",psRNG);
	EXIT("EvalSetRNG");
	return FALSE;
}

/* ComputeGetRNG (Interface Command)

Description:
	Return the planner's current random number generator.
*/

CELLP ComputeGetRNG
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetRNG",FALSE);
	pc=MakeStringForm(*prCurrentRNG->ppsName);
	EXIT("ComputeGetRNG");
	return pc;
}

/* EvalResetRNG (Interface Command)

Description:
	Reset the random number generator to its default value.
*/

BOOL EvalResetRNG
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetRNG",FALSE);
	prCurrentRNG=arRNGTable;
	EXIT("EvalResetRNG");
	return TRUE;
}

/* EvalSetControl (Interface Command) ------------------------------------------

Description:
	Set the control strategy to be used by the planner.
*/

BOOL EvalSetControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char *psControl;

	/* check user input */

	ENTER("EvalSetControl",FALSE);
	pc=NULL;
	
	if(!FormulaToString(pcFormula->pfForm->pcArgs,plpLinearPlan,
		pbBindings,&psControl))
		return FALSE;
	if(StringEqQ(psControl,apsStringTab[STRING_ATEMPORAL]))
		bAtemporalControl=TRUE;
	else
	{
		if(!StringEqQ(psControl,apsStringTab[STRING_TEMPORAL]))
			ErrorMessage("set-control:  Unknown control type.  Only temporal "
				"and atemporal control are supported.\n"
				"Defaulting to temporal control.\n");
		bAtemporalControl=TRUE;
	}
	EXIT("EvalSetControl");
	return TRUE;
}

/* EvalEnable (Interface Command) ----------------------------------------------

Description:
	Enable a feature
*/

BOOL EvalEnable
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	/* check user input */

	ENTER("EvalEnable",FALSE);

	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		ErrorMessage("enable:  Missing argument.\n"
			"Command ignored.\n");
		return FALSE;
	}
	if(StringEqQ(pc->pfForm->psName,apsStringTab[STRING_CONCURRENT_PLANNING]))
		bConcurrentPlanning=TRUE;
	else if(StringEqQ(pc->pfForm->psName,apsStringTab[STRING_CYCLE_CHECKING]))
		bCycleChecking=TRUE;
	else if(StringEqQ(pc->pfForm->psName,apsStringTab[STRING_TIMING_STATISTICS]))
		bTimingStatistics=TRUE;
	else if(StringEqQ(pc->pfForm->psName,apsStringTab[STRING_BACKTRACKING]))
		bBackTracking=TRUE;
	else if(StringEqQ(pc->pfForm->psName,apsStringTab[STRING_PRUNING_ALL_SUCCESSORS]))
		bPruningAllSuccessors=TRUE;
	else if(StringEqQ(pc->pfForm->psName,apsStringTab[STRING_PDDL_SUPPORT]))
	{
		pc=pc->pcNext;
		if(pc&&StringEqQ(pc->pfForm->psName,apsStringTab[STRING_PRIORITY]))
			nPddlSupport=PDDL_PRIORITY_METRIC;
		else if(pc&&StringEqQ(pc->pfForm->psName,apsStringTab[STRING_COST]))
			nPddlSupport=PDDL_COST_METRIC;
		else
			nPddlSupport=PDDL_IGNORE_METRIC;
	}
	else
	{
		ErrorMessage("enable:  Unknown enable setting.\n"
			"Command ignored.\n");
		return FALSE;
	}
	EXIT("EvalEnable");
	return TRUE;
}

/* EvalDisable (Interface Command) ---------------------------------------------

Description:
	Disable a feature
*/

BOOL EvalDisable
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	/* check user input */

	ENTER("EvalDisable",FALSE);
	pc=NULL;
	
	if(!pcFormula->pfForm->pcArgs)
	{
		ErrorMessage("enable:  Missing argument.\n"
			"Command ignored.\n");
		return FALSE;
	}
	if(StringEqQ(pcFormula->pfForm->pcArgs->pfForm->psName,
		apsStringTab[STRING_CONCURRENT_PLANNING]))
		bConcurrentPlanning=FALSE;
	else if(StringEqQ(pcFormula->pfForm->pcArgs->pfForm->psName,
		apsStringTab[STRING_CYCLE_CHECKING]))
		bCycleChecking=FALSE;
	else if(StringEqQ(pcFormula->pfForm->pcArgs->pfForm->psName,
		apsStringTab[STRING_TIMING_STATISTICS]))
		bTimingStatistics=FALSE;
	else if(StringEqQ(pcFormula->pfForm->pcArgs->pfForm->psName,
		apsStringTab[STRING_BACKTRACKING]))
		bBackTracking=FALSE;
	else if(StringEqQ(pcFormula->pfForm->pcArgs->pfForm->psName,
		apsStringTab[STRING_PRUNING_ALL_SUCCESSORS]))
		bPruningAllSuccessors=FALSE;
	else if(StringEqQ(pcFormula->pfForm->pcArgs->pfForm->psName,
		apsStringTab[STRING_PDDL_SUPPORT]))
		nPddlSupport=PDDL_UNSUPPORTED;
	else
	{
		ErrorMessage("disable:  Unknown disable setting.\n"
			"Command ignored.\n");
		return FALSE;
	}
	EXIT("EvalDisable");
	return TRUE;
}

/* Trace Interface ------------------------------------------------------------- */

int nTrace=0;							/* Trace level */
BOOL bVerbose=TRUE;						/* The planner should be verbose? */

/* EvalVerboseOn (Interface Command) */

BOOL EvalVerboseOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalVerboseOn",FALSE);
	bVerbose=TRUE;
	EXIT("Eval;VerboseOn");
	return TRUE;
}

/* EvalVerboseOff (Interface Command) */

BOOL EvalVerboseOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalVerboseOff",FALSE);
	bVerbose=FALSE;
	EXIT("EvalVerboseOff");
	return TRUE;
}

/* EvalSetTraceLevel (Interface Command)

Description:
	Set the trace level.
Scheme:

(define (set-trace-level i)
  (set! *trace* i))
*/

BOOL EvalSetTraceLevel
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;
	int n;

	/* check user input */

	ENTER("EvalSetTraceLevel",FALSE);

	bStatus=FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&n);
	if(bStatus)
		nTrace=n;
	EXIT("EvalSetTraceLevel");
	return bStatus;
}

/* ComputeGetTraceLevel (Interface Command)

Description:
	Return the current trace level.
*/

CELLP ComputeGetTraceLevel
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetTraceLevel",FALSE);
	pc=MakeIntegerForm(nTrace);
	EXIT("ComputeGetTraceLevel");
	return pc;
}

/* FunctionTracer

Description:
Scheme:

(define (function-tracer key data)
	(cond
		((eq? key 'plan-successors)
			(format *trace-stream*
				"~%~%Expanding world at depth ~S:"
				(plan-length (first data)))
			(print-world (plan-world/action (first data)) *trace-stream*)
			(format *trace-stream*
				"~%This world was obtained by the action: ~S"
				(plan-action-name (first data)))
			(if (second data)
				(format *trace-stream*
					"~%Progression successful, generating world successors.")
				(format *trace-stream*
					"~%Progression FAILED, pruning world."))
			(if (and (>= *trace* 2) (second data))
				(format *trace-stream*
					"~%Progressed temporal formula is:~%~S"
					(second data)))
			(if (and (>= *trace* 3) (second data))
				(begin
					(format *trace-stream*
						"~%Number of successor worlds = ~S."
						(length (third data)))
					(for-each (lambda (plan)
							(format *trace-stream*
								"~%Action: ~S"
								(plan-action-name plan))
							(print-world (plan-world/action plan) *trace-stream*)
							(format *trace-stream*  "~%"))
						(third data)))))
		(else #t)))
*/

void FunctionTracerProlog
(
	LINEARPLANP plpParent,	 			/* parent world */
	BINDINGP pbBindings					/* bindings list */
)
{
	/* trace level 1
	display statistics and generating action
	display the progression status
	*/

	CommandPrintf(pfTraceStream,"-----------------------------------------"
		"---------------------------------------\n");

	// simplify looking at incomplete plans

	if(pfSearchStrategy==BestFirst||pfSearchStrategy==DepthBestFirst)
	{
		if(bConcurrentPlanning)
			CommandPrintf(pfTraceStream,"Expanding world %d at depth %d, heuristic %f delta %f time %f action: ",
				plpParent->nWorldNumber,LinearPlanLength(plpParent),plpParent->dfHeuristic,
				(plpParent->plpParent)?plpParent->dfHeuristic-plpParent->plpParent->dfHeuristic:plpParent->dfHeuristic,plpParent->dfTime);
		else
			CommandPrintf(pfTraceStream,"Expanding world %d at depth %d, heuristic %f delta %f (action-cost %f total-cost %f) action: ",
				plpParent->nWorldNumber,LinearPlanLength(plpParent),plpParent->dfHeuristic,
				(plpParent->plpParent)?plpParent->dfHeuristic-plpParent->plpParent->dfHeuristic:plpParent->dfHeuristic,
				plpParent->dfActionCost,plpParent->dfCost);
	}
	else if(pfSearchStrategy==BreadthFirstPriority||pfSearchStrategy==DepthFirstPriority)
	{
		if(bConcurrentPlanning)
			CommandPrintf(pfTraceStream,"Expanding world %d at depth %d, priority %f time %f action: ",
				plpParent->nWorldNumber,LinearPlanLength(plpParent),plpParent->dfPriority,plpParent->dfTime);
		else
			CommandPrintf(pfTraceStream,"Expanding world %d at depth %d, priority %f action: ",
				plpParent->nWorldNumber,LinearPlanLength(plpParent),plpParent->dfPriority);
	}
	else
	{
		if(bConcurrentPlanning)
			CommandPrintf(pfTraceStream,"Expanding world %d at depth %d time %f action: ",
				plpParent->nWorldNumber,LinearPlanLength(plpParent),plpParent->dfTime);
		else
			CommandPrintf(pfTraceStream,"Expanding world %d at depth %d action: ",
				plpParent->nWorldNumber,LinearPlanLength(plpParent));
	}
	PrintFlatFormula(pfTraceStream,plpParent->pcActionName);
	CommandPrintf(pfTraceStream,"\n\n");
	PrintWorld(STDLOG,plpParent,pbBindings);
}

void FunctionTracer
(
	LINEARPLANP plpParent,	 			/* parent world */
	CELLP pcProgressedTLForm,			/* progressed temporal control formula */
	CELLP pcTLForm,						/* unprogressed (parent) temporal control formula */
	CELLP pcCCForm,						/* current control formula */
	LINEARPLANP plpSuccessors,			/* successor worlds */
	BINDINGP pbBindings					/* bindings list */
)
{
	LINEARPLANP plpPlan;
	int nCount;

	ENTER("FunctionTracer",TRUE);

	/* trace level 1
	display statistics and generating action
	display the progression status
	*/

	if((!bAtemporalControl&&FalseFormQ(pcProgressedTLForm))||(bAtemporalControl&&!pcProgressedTLForm))
	{
		CommandPrintf(pfTraceStream,"Progression FAILED, world pruned.\n\n");
		if(nTrace>=2&&pcProgressedTLForm)
			PColorFormula(pfTraceStream,pcTLForm,plpParent,0,0);

		if(nTrace>=3)
		{
			if(bConcurrentPlanning)
			{
				DumpQueueEvents(plpParent);
//				DumpQueue();
				CommandPrintf(pfTraceStream,"\n");
			}
		}
	}
	else
	{
		/* trace level 2
		display the progressed formula
		display the current formula
		*/

		if(nTrace>=2)
		{
			if(bAtemporalControl)
				CommandPrintf(pfTraceStream,"Progression successful\n");
			else
			{
				CommandPrintf(pfTraceStream,"Progression successful, "
					"progressed temporal formula:\n");
				PrintFormula(pfTraceStream,pcProgressedTLForm,0);
//				RefPrintFormula(pfTraceStream,pcProgressedTLForm,0);
				CommandPrintf(pfTraceStream,"CC formula:\n");
				PrintFormula(pfTraceStream,pcCCForm,0);
			}
		}
		else
			CommandPrintf(pfTraceStream,"Progression successful "
				"world successors generated.\n");

		/* trace level 3 
		display the delayed action queue
		display the successor list
		*/

		if(nTrace>=3)
		{
			if(bConcurrentPlanning)
			{
				DumpQueueEvents(plpParent);
//				DumpQueue();
			}

			nCount=0;
			for(plpPlan=plpSuccessors;plpPlan;plpPlan=plpPlan->plpNext)
				nCount++;
			CommandPrintf(pfTraceStream,"\nNumber of successor worlds = %d.\n",
				nCount);
			for(plpPlan=plpSuccessors;plpPlan;plpPlan=plpPlan->plpNext)
			{
				if(pfSearchStrategy==BestFirst||pfSearchStrategy==DepthBestFirst)
					CommandPrintf(pfTraceStream,"World %d (Heuristic %f) Action:  ",
						plpPlan->nWorldNumber,plpPlan->dfHeuristic);
				else
				if(pfSearchStrategy==BreadthFirstPriority||pfSearchStrategy==DepthFirstPriority)
					CommandPrintf(pfTraceStream,"World %d (Priority %f) Action:  ",
						plpPlan->nWorldNumber,plpPlan->dfPriority);
				else
					CommandPrintf(pfTraceStream,"World %d Action: ",
						plpPlan->nWorldNumber);
				PrintFormula(pfTraceStream,plpPlan->pcActionName,0);
			}
		}
	}
	EXIT("FunctionTracer");
}

//------------

/* EvalSetStatisticsFile (Interface Command)

Description:
	Specify a name for the plan output file.
*/

BOOL EvalSetStatisticsFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;
	char *ps;

	ENTER("EvalSetStatisticsFile",FALSE);
	bStatus=FormulaToString(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&ps);
	if(bStatus)
		psStatisticsFile=ps;
	EXIT("EvalSetStatisticsFile");
	return bStatus;
}

/* EvalSetGoalAddendum (Interface Command) -------------------------------------

Description:
	Set an extra goal condition.
*/

BOOL EvalSetGoalAddendum
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	/* check user input */

	ENTER("EvalSetGoalAddendum",FALSE);

	pc=pcFormula->pfForm->pcArgs;
	if(pc->pfForm->paAction->pfEval!=EvalDefPredicate)
	{
		ErrorMessage("set-goal-addendum:  Goal addendum must be a "
			"defined predicate.\n");
		EXIT("EvalSetGoalAddendum");
		return FALSE;
	}
	pcGoalAddendum=pc;	
	EXIT("EvalSetGoalAddendum");
	return TRUE;
}

/* ComputeGetGoalAddendum (Interface Command)

Description:
	Return the current goal addendum.
*/

CELLP ComputeGetGoalAddendum
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetGoalAddendum",FALSE);
	if(pcGoalAddendum)
		pc=CopyCell(pcGoalAddendum);
	else
		pc=MakeTrueForm();
	EXIT("ComputeGetGoalAddendum");
	return pc;
}

/* EvalResetGoalAddendum (Interface Command)

Description:
	Reset the goal addendum to its default.
*/

BOOL EvalResetGoalAddendum
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetGoalAddendum",FALSE);
	ResetGoalAddendum();
	EXIT("EvalResetGoalAddendum");
	return TRUE;
}

/* ResetGoalAddendum

Description:
	Reset the Heuristic function to its default.
*/

static void ResetGoalAddendum(void)
{
	ENTER("ResetGoalAddendum",FALSE);
	pcGoalAddendum=NULL;
	EXIT("ResetGoalAddendum");
}

/* Heuristic Interface ------------------------------------------------------- */

/* EvalSetHeuristicFn (Interface Command)

Description:
	Set the heuristic cost function used by best first search.
Scheme:

(define (set-cost-fn fn)
  (cond ((procedure? fn)
	 (set! cost-fn fn))
	((eqv? fn 'optimal-cost)
	 (set! cost-fn optimal-cost))
	(else (format #t "%Cost  function must be a function
						~%or the symbol 'optimal-cost"))))
*/

BOOL EvalSetHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalSetHeuristicFn",FALSE);

	pc=pcFormula->pfForm->pcArgs;
	if(pc->pfForm->paAction->pfCompute==NoComputor)
	{
		ErrorMessage("set-heuristic-fn:  Heuristic function must be a "
			"function or (optimal-cost).\n");
		EXIT("EvalSetHeuristicFn");
		return FALSE;
	}
	pcHeuristicFn=pc;
	EXIT("EvalSetHeuristicFn");
	return TRUE;
}

/* ComputeGetHeuristicFn (Interface Command)

Description:
	Return the current Heuristic function used by the planner.
Scheme:

(define (get-Heuristic-fn)
  Heuristic-fn)
*/

CELLP ComputeGetHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetHeuristicFn",FALSE);
	if(pcHeuristicFn)
		pc=CopyCell(pcHeuristicFn);
	else
		pc=MakeOptimalCostForm();
	EXIT("ComputeGetHeuristicFn");
	return pc;
}

/* ComputeHeuristicFn (Interface Command)

Description:
	Explicitly calculate the value of the heuristic function.
*/

CELLP ComputeHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeHeuristicFn",FALSE);
	if(pcHeuristicFn)
		pc=(*pcHeuristicFn->pfForm->paAction->pfCompute)(pcHeuristicFn,plpLinearPlan,pbBindings);
	else
		pc=MakeFloatForm(0.0);
	EXIT("ComputeHeuristicFn");
	return pc;
}

/* EvalResetHeuristicFn (Interface Command)

Description:
	Reset the Heuristic function to its default.
Scheme:

(define (reset-Heuristic-fn )
  (set! Heuristic-fn optimal-Heuristic))
*/

BOOL EvalResetHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetHeuristicFn",FALSE);
	pcHeuristicFn=NULL;
	EXIT("EvalResetHeuristicFn");
	return TRUE;
}

/* ResetHeuristicFn

Description:
	Reset the Heuristic function to its default.
Scheme:

(define (reset-Heuristic-fn )
  (set! Heuristic-fn optimal-Heuristic))
*/

static void ResetHeuristicFn(void)
{
	ENTER("ResetHeuristicFn",FALSE);
	pcHeuristicFn=NULL;
	EXIT("ResetHeuristicFn");
}

/* Priority Interface ---------------------------------------------------------- */

/* EvalSetPriorityFn (Interface Command)

Description:
	Set the priority function. Either set to a previously defined fn.
	Or to a new function that must take a plan and return a
	numeric ranking indicating the merit of exploring this plan first.
Scheme:

(define (set-priority-fn fn)
	(cond ((procedure? fn)
			(set! priority-fn fn))
		((eqv? fn 'best-action)
			(set! priority-fn best-action))
		(else
			(format #t "%Priority function must be a function
				or the symbol 'best-action"))))
*/

BOOL EvalSetPriorityFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalSetPriorityFn",FALSE);

	pc=pcFormula->pfForm->pcArgs;
	if(pc->pfForm->paAction->pfCompute!=ComputeDefFunction&&
		pc->pfForm->paAction->pfCompute!=ComputeBestAction)
	{
		ErrorMessage("set-priority-fn:  Priority function must be a "
			"defined function or (best-action)\n");
		EXIT("EvalSetPriorityFn");
		return FALSE;
	}
	pcPriorityFn=pc;
	EXIT("EvalSetPriorityFn");
	return TRUE;
}

/* ComputeGetPriorityFn (Interface Command)

Description:
	Return the current priority function.
Scheme:

(define (get-priority-fn )
  priority-fn)
*/

CELLP ComputeGetPriorityFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetPriorityFn",FALSE);
	if(pcPriorityFn)
		pc=CopyCell(pcPriorityFn);
	else
		pc=MakeBestActionForm();
	EXIT("ComputeGetPriorityFn");
	return pc;
}

/* EvalResetPriorityFn (Interface Command)

Description:
	Reset the priority function to its default.
Scheme:

(define (reset-priority-fn )
  (set! priority-fn best-action))
*/

BOOL EvalResetPriorityFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetPriorityFn",FALSE);
	pcPriorityFn=NULL;
	EXIT("EvalResetPriorityFn");
	return TRUE;
}

/* ResetPriorityFn

Description:
	Reset the priority function to its default.
Scheme:

(define (reset-priority-fn )
  (set! priority-fn best-action))
*/

void ResetPriorityFn(void)
{
	ENTER("ResetPriorityFn",FALSE);
	pcPriorityFn=NULL;
	EXIT("ResetPriorityFn");
}

/* Domain Interface ------------------------------------------------------------ */

CELLP pcDomainStart=NULL;			/* Domain listhead */
CELLP pcDomainEnd=(CELLP)&pcDomainStart;

/* EvalResetDomains (Interface Command)

Description:
	Reset the known domains to NULL.
	We also take this opportunity to clear memory.
Scheme:

(define (reset-domains )
  (set! *domains* '()))
*/

BOOL EvalResetDomains
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetDomains",FALSE);
	pcDomainStart=NULL;
	pcDomainEnd=(CELLP)&pcDomainStart;
	EXIT("EvalResetDomains");
	return TRUE;
}

/* EvalDefDomain (Interface Command)

Description:
	Add a domain to the list of domains.
Scheme:

(define (def-domain domain-name file-list description)
  (set! *domains* (cons (list domain-name file-list description) *domains*)))
*/

BOOL EvalDefDomain
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalDefDomain",FALSE);
	pcDomainEnd=pcDomainEnd->pcNext=pcFormula;
	EXIT("EvalDefDomain");
	return TRUE;
}

/* EvalListDomains (Interface Command)

Description:
	Print out a list of the known domains.
Scheme:

(define (list-domains)
  (format #t "~%The known domains are")
  (for-each (lambda (domain)
		  (format #t "~%~@20S , ~A" (first domain) (third domain)))
		*domains*))
*/

BOOL EvalListDomains
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalListDomains",FALSE);
	ListDomains();
	EXIT("EvalListDomains");
	return TRUE;
}

/* ListDomains

Description:
	Print out a list of the known domains.
Scheme:

(define (list-domains)
  (format #t "~%The known domains are")
  (for-each (lambda (domain)
		  (format #t "~%~@20S , ~A" (first domain) (third domain)))
		*domains*))
*/

static void ListDomains(void)
{
	CELLP pc;
	char ac[40];

	ENTER("ListDomains",TRUE);
	if(!pcDomainStart)
	{
		CommandPrintf(stdout,"list-domains: There are no domains defined.\n");
		EXIT("ListDomains");
		return;
	}
	CommandPrintf(stdout,"The known domains are:\n");
	for(pc=pcDomainStart;pc;pc=pc->pcNext)
	{
		CommandPrintf(stdout,"%20s:  %s\n",
			GetName(pc->pfForm->pcArgs->pfForm,ac),
			GetName(pc->pfForm->pcArgs->pcNext->pfForm,ac));
	}
	EXIT("ListDomains");
}

/* EvalLoadDomain (Interface Command)

Description:
	Load the appropriate files for the domain
Scheme:

(define (load-domain . optional-args)
  (let ((domain-name
	 (if (null? optional-args)
		 '()
		 (first optional-args))))
	(let ((domain
	   (find domain-name *domains*
		 :test (lambda (name domain)
			 (eqv? name (first domain))))))
	  (if domain
	  (for-each (lambda(file) (load file)) (second domain))
	  (begin
		(format #t "~%Unknown domain ~S." domain-name)
		(list-domains))))))
*/

BOOL EvalLoadDomain
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char *psName;
	char ac[256];

	ENTER("EvalLoadDomain",FALSE);

	/* find domain and load its files */

	psName=IdentName(pcFormula->pfForm->pcArgs->pfForm);
	for(pc=pcDomainStart;pc;pc=pc->pcNext)	// find the domain in the domain list
	{
		if(StringEqQ(psName,IdentName(pc->pfForm->pcArgs->pfForm)))
			break;
	}
	if(pc)								// load the associated files
	{
		for(pc=pc->pfForm->pcArgs->pcNext->pcNext;pc;pc=pc->pcNext)
			LoadFile(GetName(pc->pfForm,ac));
	}
	else								// domain not found, show domain list
	{
		CommandPrintf(stdout,"Unknown domain '%s'.\n",psName);
		ListDomains();
		return FALSE;
	}
	EXIT("EvalLoadDomain");
	return TRUE;
}

/* MarkDomains

Description:
	Mark the list of domains for garbage collection.
*/

void MarkDomains(void)
{
	ENTER("MarkDomains",TRUE);
	MarkFormulaList(pcDomainStart);
	EXIT("MarkDomains");
}

/* -------------------------------------------------------------------------- */

/* EvalLoadFile (Interface command)

Description:
	Read in a list of tlplanner commands.
*/

BOOL EvalLoadFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];
	
	ENTER("EvalLoadFile",FALSE);
	LoadFile(GetName(pcFormula->pfForm->pcArgs->pfForm,ac));
	EXIT("EvalLoadFile");
	return TRUE;
}

/* -------------------------------------------------------------------------- */

/* EvalLoadPddlFile (Interface command)

Description:
	Read in a PDDL file.
	Only PDDL problem syntax is accepted.
*/

BOOL EvalLoadPddlProblem
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];
	
	ENTER("EvalLoadPddlProblem",FALSE);
	LoadPddlProblem(GetName(pcFormula->pfForm->pcArgs->pfForm,ac));
	EXIT("EvalLoadPddlProblem");
	return TRUE;
}

/* Print World Interface ------------------------------------------------------- */

/* EvalSetPrintWorldFn (Interface Routine)

Description:
	Set the world print function used during tracing etc.
Scheme:

(define (set-print-world-fn fn)
  (set! print-world fn))
*/

BOOL EvalSetPrintWorldFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	SYMBOLINFOP psiPrint;
	CELLP pcStream;
	char ac[40];

	ENTER("EvalSetPrintWorldFn",FALSE);
	pc=pcFormula->pfForm->pcArgs;
	psiPrint=GetSymbolInfoPtr(GetName(pc->pfForm,ac));
	if(!psiPrint)
	{
		ErrorMessage("set-print-world-fn:  Unknown print routine %s\n",
			GetName(pc->pfForm,ac));
		EXIT("EvalSetPrintWorldFn");
		return FALSE;
	}

	/* generate a (<name> ?stream) formula */

	pcStream=(CELLP)MemAlloc(sizeof(CELL));
	pcStream->pfForm=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pcStream->pfForm->nType=ATOM_IDENT;
	pcStream->pfForm->psName=IdentAlloc("?stream");
	pcStream->pfForm->paAction=&aIdentAction;
	pcPrintWorld=NewMakeSymbolInfoForm(FALSE,pc,pcStream);
	EXIT("EvalSetPrintWorldFn");
	return TRUE;
}

/* EvalResetPrintWorldFn (Interface Routine)

Description:
	Reset the world print function to its default.
Scheme:

(define (reset-print-world-fn )
  (set! print-world default-print-world))
*/

BOOL EvalResetPrintWorldFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetPrintWorldFn",FALSE);
	pcPrintWorld=NULL;
	EXIT("EvalResetPrintWorldFn");
	return TRUE;
}

/* ResetPrintWorldFn

Description:
	Reset the world print function to its default.
Scheme:

(define (reset-print-world-fn )
  (set! print-world default-print-world))
*/

void ResetPrintWorldFn(void)
{
	ENTER("ResetPrintWorldFn",FALSE);
	pcPrintWorld=NULL;
	EXIT("ResetPrintWorldFn");
}

/* EvalPrintDeltaTime (Interface Routine)

Description:
	Calculate and print the elapsed time since we were last called.
	The first call to print-delta-time returns the time since the program was started.
	The description argument is optional.  If it is not present, then nothing is
	printed.  This behavior is useful for zeroing the internal counter.
Call:
	(print-delta-time <description>)
*/

BOOL EvalPrintDeltaTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	static double dfPreviousTime;		// previous time
	double dfCurrentTime;				// new time
	char *ps;							// string pointer

	ENTER("EvalPrintDeltaTime",FALSE);
	dfCurrentTime=GetInternalRunTime();
	
	if(pcFormula->pfForm->pcArgs)
	{
		ps=IdentName(pcFormula->pfForm->pcArgs->pfForm);
		CommandPrintf(stderr,"%s%.3f\n",ps,dfCurrentTime-dfPreviousTime);
	}
	dfPreviousTime=dfCurrentTime;		// pass the baton
	EXIT("EvalPrintDeltaTime");
	return TRUE;
}

/* -------------------------------------------------------------------------- */

/* EvalDeclareDescribedSymbols (Interface Command)

Description:
	Given a list of described symbols, set up the asiWorldSymbols
	information array. SymbolInfoList is a list of the form
	(name type[function|predicate] arity).
Scheme:

(define (declare-described-symbols symbol-info-list)
  (if (not (eqv? *number-of-world-symbols* 0))
	  (error "Described symbols must be defined first. Call
			~%(clear-world-symbols) and try again."))
  (do ((symbol-info-item '()))
	  ((null? symbol-info-list))
	(set! symbol-info-item (first symbol-info-list))
	(set! symbol-info-list (rest symbol-info-list))
	(if (or (eqv? (second symbol-info-item) 'predicate)
		(eqv? (second symbol-info-item) 'function))
	(insert-symbol-info
	 (first symbol-info-item) (second symbol-info-item)
	 (third symbol-info-item) 'DESCRIBED)
	(error "In declare-described-symbols second item of symbol-info-item~
			 ~%Should be one of the symbols ~S or ~S"
		   'predicate 'function)))
  (set! *number-of-described-symbols* (vector-length *world-symbols*)))
*/

BOOL EvalDeclareDescribedSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcType;
	CELLP pcName;
	CELLP pcArity;
	CELLP pcExtra;
	int nType;
	int nArity;
	BOOL bStatus;
	BOOL bNoCycleCheck;
	BOOL bRewritable;

	char ac[40];

	ENTER("EvalDeclareDescribedSymbols",FALSE);
	if(nNumberOfWorldSymbols)
	{
		ErrorMessage("Described symbols must be declared first.  "
			"Call clear-world-symbols and try again.\n");
		EXIT("EvalDeclareDescribedSymbols");
		return FALSE;
	}

	/* check user input */

	bStatus=TRUE;						/* assume success */
	for(pcType=pcFormula->pfForm->pcArgs;pcType;pcType=pcType->pcNext)
	{
		if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_PREDICATE])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_FUNCTION]))
		{
			nType=StringToSymbolType(GetName(pcType->pfForm,ac));

			pcName=pcType->pfForm->pcArgs;
			if(!pcName)
			{
				ErrorMessage("declare-described-symbols: %s missing name and arity\n",
					GetName(pcType->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(Reserved(GetName(pcName->pfForm,ac)))
			{
				ErrorMessage("declare-described-symbols: %s is a reserved symbol, "
					"please use a different name for this symbol\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			pcArity=pcName->pcNext;
			if(!pcArity)
			{
				ErrorMessage("declare-described-symbols: %s missing arity.\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(!FormulaToInteger(pcArity,plpLinearPlan,pbBindings,&nArity))
			{
				ErrorMessage("declare-described-symbols: Arity must be a valid integer.\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			bNoCycleCheck=FALSE;
			bRewritable=FALSE;
			for(pcExtra=pcArity->pcNext;pcExtra;pcExtra=pcExtra->pcNext)
			{
				if(StringEqQ(IdentName(pcExtra->pfForm),apsStringTab[STRING_NO_CYCLE_CHECK]))
					bNoCycleCheck=TRUE;
				else if(StringEqQ(IdentName(pcExtra->pfForm),apsStringTab[STRING_REWRITABLE]))
				{
					if(nType!=SYMBOL_PREDICATE)
					{
						ErrorMessage("declare-described-symbols: Only described predicate can be rewritable: %s\n",
							GetName(pcName->pfForm,ac));
						bStatus=FALSE;
						continue;
					}
					bRewritable=TRUE;
				}
				else
				{
					ErrorMessage("declare-described-symbols: Unknown trailing keyword \"%s\" in declaration of %s\n",
						GetName(pcExtra->pfForm,ac),GetName(pcName->pfForm,ac));
					bStatus=FALSE;
				}
			}
			if(bNoCycleCheck)
				continue;				/* skip extra symbols for now */
			if(nType==SYMBOL_PREDICATE)
				InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DESCRIBED,
					(void (*)(void))EvalDescPredicate,NULL,bRewritable);
			else
				InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DESCRIBED,
					(void (*)(void))ComputeDescFunction,NULL,FALSE);
		}
		else
		{
			ErrorMessage("declare-described-symbols:  Each declaration must begin "
				"with either PREDICATE or FUNCTION\n");
			bStatus=FALSE;
		}
	}

	/* handle extra symbols */

	nNumberOfCheckedSymbols=nNumberOfWorldSymbols;
	if(bStatus)
	{
		for(pcType=pcFormula->pfForm->pcArgs;pcType;pcType=pcType->pcNext)
		{
			pcName=pcType->pfForm->pcArgs;
			pcArity=pcName->pcNext;

			bNoCycleCheck=FALSE;
			bRewritable=FALSE;
			for(pcExtra=pcArity->pcNext;pcExtra;pcExtra=pcExtra->pcNext)
			{
				if(StringEqQ(IdentName(pcExtra->pfForm),apsStringTab[STRING_NO_CYCLE_CHECK]))
					bNoCycleCheck=TRUE;
				else if(StringEqQ(IdentName(pcExtra->pfForm),apsStringTab[STRING_REWRITABLE]))
					bRewritable=TRUE;
			}

			if(bNoCycleCheck)
			{
				nType=StringToSymbolType(GetName(pcType->pfForm,ac));
				FormulaToInteger(pcArity,plpLinearPlan,pbBindings,&nArity);
				if(nType==SYMBOL_PREDICATE)
					InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DESCRIBED,
						(void (*)(void))EvalDescPredicate,NULL,bRewritable);
				else
					InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DESCRIBED,
						(void (*)(void))ComputeDescFunction,NULL,FALSE);
			}
		}	
	}

	nNumberOfDescribedSymbols=nNumberOfWorldSymbols;
	if(nNumberOfDescribedSymbols)		/* make sure default world follows */
		plpCurrentPlan->apbtWorld=MakeWorld();
	EXIT("EvalDeclareDescribedSymbols");
	return bStatus;
}

/* EvalDeclareDefinedSymbols (Interface Command)

Description:
	Given a list of defined symbols, set up the asiWorldSymbols
	information array.  plCommand is a list of lists of the form
	(type name arity), where type is either the literal
	"function" or "predicate".
*/

BOOL EvalDeclareDefinedSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcType;
	CELLP pcName;
	CELLP pcArity;
	int nType;
	int nArity;
	BOOL bStatus;
	char ac[40];

	/* check user input */

	ENTER("EvalDeclareDefinedSymbols",FALSE);

	bStatus=TRUE;						/* assume success */
	for(pcType=pcFormula->pfForm->pcArgs;pcType;pcType=pcType->pcNext)
	{
		if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_PREDICATE])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_FUNCTION])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_GENERATOR]))
		{
			pcName=pcType->pfForm->pcArgs;
			if(!pcName)
			{
				ErrorMessage("declare-defined-symbols: %s missing name and arity\n",
					GetName(pcType->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(Reserved(GetName(pcName->pfForm,ac)))
			{
				ErrorMessage("declare-defined-symbols: %s is a reserved symbol, "
					"please use a different name for this symbol\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			pcArity=pcName->pcNext;
			if(!pcArity)
			{
				ErrorMessage("declare-defined-symbols: %s missing arity.\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(!FormulaToInteger(pcArity,plpLinearPlan,pbBindings,&nArity))
			{
				ErrorMessage("declare-defined-symbols: Arity must be a valid integer.\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			nType=StringToSymbolType(GetName(pcType->pfForm,ac));
			if(nType==SYMBOL_PREDICATE)
				InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DEFINED,
					(void (*)(void))EvalDefPredicate,NULL,FALSE);
			else if(nType==SYMBOL_FUNCTION)
				InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DEFINED,
					(void (*)(void))ComputeDefFunction,NULL,FALSE);
			else
				InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,EVAL_DEFINED,
					(void (*)(void))GenerateDefGenerator,NULL,FALSE);
		}
		else
		{
			ErrorMessage("declare-defined-symbols:  Each declaration must begin "
				"with either PREDICATE or FUNCTION\n");
			bStatus=FALSE;
		}
	}
	EXIT("EvalDeclareDefinedSymbols");
	return bStatus;
}

/* EvalDeclareGlobalVariables (Interface Command)

Description:
	Given a list of global variables, set up the pbGlobalVariables
	and pbGlobalInitialization lists.  plCommand is a list of lists of the form
	(type name value), where type is one of the literals:
	"domain-global" -- variables are initialized once only (now), 
	"search-global" -- variables are initialized by the planner.
	"world-global" -- variables belong to each world and are initialized each time
		a world is created.
*/

BOOL EvalDeclareGlobalVariables
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcType;
	CELLP pcVar,pcVal;					/* bindings */
	CELLP pc,pc1;
	CELLP pcEnd;
	BOOL bStatus;
	char ac[40];						/* string buffer */

	ENTER("EvalDeclareGlobalVariables",FALSE);

	/* point to end of pcSearchGlobalInit */

	for(pcEnd=(CELLP)&pcSearchGlobalInit;pcEnd->pcNext;pcEnd=pcEnd->pcNext);

	/* process the variable declarations */

	bStatus=TRUE;						/* assume success */
	for(pcType=pcFormula->pfForm->pcArgs;pcType;pcType=pcType->pcNext)
	{
		if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_DOMAIN_GLOBAL])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_SEARCH_GLOBAL])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_WORLD_GLOBAL]))
		{
			pc=pcType->pfForm->pcArgs;
			if(!pc)
			{
				ErrorMessage("declare-global-variables: %s missing name and value.\n",
					GetName(pcType->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(*GetName(pc->pfForm,ac)!='?')
			{
				ErrorMessage("declare-global-variables: %s does not have a leading '?'.\n",
					GetName(pc->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			pcVar=CopyCell(pc);
			pc=pc->pcNext;
			if(!pc)
			{
				ErrorMessage("declare-global-variables: %s missing value.\n",
					GetName(pcVar->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_DOMAIN_GLOBAL]))
			{
				pcVal=ComputeTerm(pc,0,pbGlobalVariables);
				if(!pcVal)
					TermError("declare-global-variable",pc,pbBindings);
				if(pcVar->pfForm->pcArgs)	/* if array */
				{
					pcVar=MakeArray(pcVar,pcVal,0,pbGlobalVariables);
					pbGlobalVariables=ExtendBindings(pcVar,pcVar,pbGlobalVariables);
				}
				else
					pbGlobalVariables=ExtendBindings(pcVar,pcVal,pbGlobalVariables);
			}
			else if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_SEARCH_GLOBAL]))
			{
				pcVal=ComputeTerm(pc,0,pbGlobalVariables);
				pc1=MakeIntegerForm(0);
				if(pcVar->pfForm->pcArgs)	/* if array */
					pcVar=MakeArray(pcVar,pc1,0,pbGlobalVariables);
				pbGlobalVariables=ExtendBindings(pcVar,pcVar,pbGlobalVariables);
				pcEnd=pcEnd->pcNext=MakeSearchGlobalInitializationForm(FALSE,pcVar,pcVal);
			}
			else if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_WORLD_GLOBAL]))
			{
				ErrorMessage("declare-global-variables:  World-global variables not supported (yet).\n");
				bStatus=FALSE;
			}
			else
			{
				ErrorMessage("declare-global-variables:  Unexpected variable type %s.\n",
					GetName(pcType->pfForm,ac));
				bStatus=FALSE;
			}
		}
		else
		{
			ErrorMessage("declare-global-variables:  Each declaration must begin "
				"with a variable type.\n");
			bStatus=FALSE;
		}
	}
	EXIT("EvalDeclareGlobalVariables");
	return bStatus;
}

/* EvalDeclareExternalSymbols (Interface Routine)

Description:
	Load a (domain specific) user library.
Syntax:
	(declare-external-symbols "file"
		(type name libref arity)
		...)
Where:
	type is the literal "generator", "function" or "predicate".
	name is the external (lisp-like) name of the routine
	libref is the internal (c-like) name of the routine
	arity is the number of arguments
*/

BOOL EvalDeclareExternalSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcPath;
	CELLP pcType;
	CELLP pcName;
	CELLP pcLibRef;
	CELLP pcArity;
	int nType;
	char *psLibRef;
	int nArity;
	BOOL bStatus;
	int nHandle;						/* library handle */
	void (*pfFunction)(void);
	char ac[40];						/* string buffer */

	/* check user input */

	ENTER("EvalDeclareExternalSymbols",FALSE);
	pcPath=pcFormula->pfForm->pcArgs;
	if(!NewLibrary(GetName(pcPath->pfForm,ac),&nHandle))
	{
		EXIT("EvalDeclareExternalSymbols");
		return FALSE;
	}

	bStatus=TRUE;						/* assume success */
	for(pcType=pcPath->pcNext;pcType;pcType=pcType->pcNext)
	{
		if(StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_PREDICATE])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_FUNCTION])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_GENERATOR])||
			StringEqQ(IdentName(pcType->pfForm),apsStringTab[STRING_MACRO]))
		{
			pcName=pcType->pfForm->pcArgs;
			if(!pcName)
			{
				ErrorMessage("declare-external-symbols: %s missing name, libref and arity\n",
					GetName(pcType->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(Reserved(GetName(pcName->pfForm,ac)))
			{
				ErrorMessage("declare-external-symbols: %s is a reserved symbol, "
					"please use a different name for this symbol\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			pcLibRef=pcName->pcNext;
			if(!pcLibRef)
			{
				ErrorMessage("declare-external-symbols: %s missing libref and arity\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(!FormulaToString(pcLibRef,plpLinearPlan,pbBindings,&psLibRef))
			{
				ErrorMessage("declare-external-symbols: %s must be specified as a quoted string\n",
					GetName(pcLibRef->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			pcArity=pcLibRef->pcNext;
			if(!pcArity)
			{
				ErrorMessage("declare-external-symbols: %s missing arity.\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			if(!FormulaToInteger(pcArity,plpLinearPlan,pbBindings,&nArity))
			{
				ErrorMessage("declare-external-symbols: %s arity must be a valid integer.\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
				continue;
			}
			nType=StringToSymbolType(GetName(pcType->pfForm,ac));
			switch(nType)
			{
				case SYMBOL_PREDICATE:
				case SYMBOL_FUNCTION:
				case SYMBOL_GENERATOR:
				case SYMBOL_MACRO:
					if(GetLibSymbol(nHandle,GetName(pcLibRef->pfForm,ac),(void **)&pfFunction))
						InsertSymbolInfo(GetName(pcName->pfForm,ac),nType,nArity,
							EVAL_EXTERNAL,pfFunction,NULL,FALSE);
					else
						bStatus=FALSE;
					break;
				default:
					ErrorMessage("Unsupported external symbol type: %s\n",
						GetName(pcType->pfForm,ac));
					bStatus=FALSE;
			}
		}
		else
		{
			ErrorMessage("declare-external-symbols:  Each declaration must begin "
				"with PREDICATE, FUNCTION, GENERATOR or MACRO\n");
			bStatus=FALSE;
		}			
	}
	EXIT("EvalDeclareExternalSymbols");
	return bStatus;
}

/* EvalDeclareMacroOperators (Interface Command)

Description:
	Given a list of macro operators, set up the amiMacroOperators
	information array.  plCommand is a list of lists of the form
	(name domain), where name is an operator name, and domain is
	a string literal path of its expansion domain.
*/

BOOL EvalDeclareMacroOperators
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcName;
	CELLP pcDomain;
	CELLP pcGoal;
	char ac[256];

	/* check user input */

	ENTER("EvalDeclareMacroOperators",FALSE);

	for(pcName=pcFormula->pfForm->pcArgs;pcName;pcName=pcName->pcNext)
	{
		if(Reserved(GetName(pcName->pfForm,ac)))
		{
			ErrorMessage("declare-macro-operators: %s is a reserved symbol, "
				"please use a different name for this symbol\n",
				GetName(pcName->pfForm,ac));
			EXIT("EvalDeclareMacroOperators");
			return FALSE;
		}
		pcDomain=pcName->pfForm->pcArgs;
		if(!pcDomain)
		{
			ErrorMessage("declare-macro-operators: %s missing domain file path\n",
				GetName(pcName->pfForm,ac));
			EXIT("EvalDeclareMacroOperators");
			return FALSE;
		}
		pcGoal=pcDomain->pcNext;
		if(!pcGoal)
		{
			ErrorMessage("declare-macro-operators: %s missing goal formula\n",
				GetName(pcName->pfForm,ac));
			EXIT("EvalDeclareMacroOperators");
			return FALSE;
		}
		AddMacroOperator(GetName(pcName->pfForm,ac),GetName(pcDomain->pfForm,ac),pcGoal);
	}
	EXIT("EvalDeclareMacroOperators");
	return TRUE;
}

/* EvalDeclareElidedOperators (Interface Command)

Description:
	Given a list of elided operators, set up the amiElidedOperators
	information array.  plCommand is a list of lists of the form
	(name), where name is an operator name.
*/

BOOL EvalDeclareElidedOperators
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcName;
	char ac[256];

	/* check user input */

	ENTER("EvalDeclareElidedOperators",FALSE);

	for(pcName=pcFormula->pfForm->pcArgs;pcName;pcName=pcName->pcNext)
	{
		if(Reserved(GetName(pcName->pfForm,ac)))
		{
			ErrorMessage("declare-elided-operators: %s is a reserved symbol, "
				"please use a different name for this symbol\n",
				GetName(pcName->pfForm,ac));
			EXIT("EvalDeclareElidedOperators");
			return FALSE;
		}
		AddElidedOperator(GetName(pcName->pfForm,ac));
	}
	EXIT("EvalDeclareElidedOperators");
	return TRUE;
}

/* Symbol Definitions ---------------------------------------------------------- */

/* EvalDefine (Interface Command)

Description:
	Define a macro symbol.
*/

BOOL EvalDefine
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcName;						/* name token */
	CELLP pcValue;						/* value list */
	SYMBOLP psSymbol;
	char ac[40];						/* string buffer */

	/* check user input */

	ENTER("EvalDefine",FALSE);
	pcName=pcFormula->pfForm->pcArgs;
	if(Reserved(GetName(pcName->pfForm,ac)))
	{
		ErrorMessage("define: %s is a reserved symbol, "
			"please use a different name for this symbol\n",
			GetName(pcName->pfForm,ac));
		return FALSE;
	}
	pcValue=pcName->pcNext;
	if(SymbolLookup(GetName(pcName->pfForm,ac),&psSymbol))
	{
		Message("define:  Redefining macro symbol %s\n",GetName(pcName->pfForm,ac));
		psSymbol->nType=ATOM_FORMULAP;
		psSymbol->uValue.pcFormula=pcValue;
		EXIT("EvalDefine");
		return TRUE;
	}
	InsertSymbolInfo(GetName(pcName->pfForm,ac),SYMBOL_MACRO,0,EVAL_DESCRIBED,
		(void (*)(void))ComputeDescMacro,pcValue,FALSE);
	EXIT("EvalDefine");
	return TRUE;
}

/* EvalDefDefinedPredicate (Interface Command)

Description:
	Parse and store a defined predicate.
Scheme:

(define-macro (def-defined-predicate name parameters formula)
  `(insert-symbol-info
	',name 'PREDICATE ,(length parameters) 'DEFINED
	(list ',parameters ',formula)))
*/

BOOL EvalDefDefinedPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	SYMBOLP psSymbol;					/* pointer to symbol table entry */
	SYMBOLINFOP psiSymbolInfo;			/* symbol information pointer */
	CELLP pcName;
	CELLP pcArguments;
	CELLP pcLocals;						/* local variables */
	CELLP pcForm;
	CELLP pc;
	FORMULAP pf;	
	int nLength;
	BOOL bStatus;
	char ac[40];						/* string buffer */

	ENTER("EvalDefDefinedPredicate",FALSE);

	/* set up useful pointers */

	bStatus=TRUE;
	pcName=pcFormula->pfForm->pcArgs;
	if(Reserved(GetName(pcName->pfForm,ac)))
	{
		ErrorMessage("def-defined-predicate: %s is a reserved symbol, "
			"please use a different name for this symbol\n",
			GetName(pcName->pfForm,ac));
		return FALSE;
	}
	pcArguments=pcName->pfForm->pcArgs;
	pcLocals=pcName->pcNext;
	if(StringEqQ(IdentName(pcLocals->pfForm),apsStringTab[STRING_LOCAL_VARS]))
	{
		pcForm=pcLocals->pcNext;
		pcLocals=pcLocals->pfForm->pcArgs;
	}
	else
	{
		pcForm=pcLocals;
		pcLocals=NULL;
	}


	nLength=0;
	for(pc=pcArguments;pc;pc=pc->pcNext)
		nLength++;

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->psName=IdentName(pcName->pfForm);
	pf->paAction=&aDefPredicateAction;
	pf->pcVars=pcArguments;
	pf->pcGenLit=pcLocals;
	pf->pcArgs=pcForm;

	/* install predicate in symbol-info table (if it isn't there already) */

	if(SymbolLookup(GetName(pcName->pfForm,ac),&psSymbol))	/* if symbol already declared */
	{
		if(psSymbol->nType==ATOM_SYMBOLINFOP)
		{
			psiSymbolInfo=psSymbol->uValue.psiSymbolInfo;
			if(psiSymbolInfo->pcFormula)
			{
				ErrorMessage("def-defined-predicate:  Predicate %s already defined\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
			}
			else
			{
				if(psiSymbolInfo->nArity!=nLength)
				{
					ErrorMessage(
						"def-defined-predicate:  %s arity declared as %d, defined as %d\n",
						psiSymbolInfo->psName,psiSymbolInfo->nArity,nLength);
					bStatus=FALSE;
				}
				if(psiSymbolInfo->nSymbolType!=SYMBOL_PREDICATE)
				{	
					ErrorMessage(
						"def-defined-predicate:  %s defined as predicate, declared as non-predicate\n",
						psiSymbolInfo->psName);
					bStatus=FALSE;
				}
				psiSymbolInfo->pcFormula=pc;
			}
		}
		else
		{
			ErrorMessage("def-defined-predicate:  Symbol %s already defined\n",
				GetName(pcName->pfForm,ac));
			bStatus=FALSE;
		}
	}
	else
	{
		InsertSymbolInfo(GetName(pcName->pfForm,ac),SYMBOL_PREDICATE,nLength,EVAL_DEFINED,
			(void (*)(void))EvalDefPredicate,pc,FALSE);
	}
	EXIT("EvalDefDefinedPredicate");
	return bStatus;
}

/* EvalDefDefinedGenerator (Interface Command)

Description:
	Parse and store a defined generator.
*/

BOOL EvalDefDefinedGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	SYMBOLP psSymbol;					/* pointer to symbol table entry */
	SYMBOLINFOP psiSymbolInfo;			/* symbol information pointer */
	CELLP pcName;
	CELLP pcArguments;
	CELLP pcLocals;						/* local variables */
	CELLP pcForm;
	CELLP pc;
	FORMULAP pf;	
	int nLength;
	BOOL bStatus;
	char ac[40];						// string symbol

	ENTER("EvalDefDefinedGenerator",FALSE);

	/* set up useful pointers */

	bStatus=TRUE;
	pcName=pcFormula->pfForm->pcArgs;
	if(Reserved(GetName(pcName->pfForm,ac)))
	{
		ErrorMessage("def-defined-generator: %s is a reserved symbol, "
			"please use a different name for this symbol\n",
			GetName(pcName->pfForm,ac));
		return FALSE;
	}
	pcArguments=pcName->pfForm->pcArgs;
	pcLocals=pcName->pcNext;
	if(StringEqQ(IdentName(pcLocals->pfForm),apsStringTab[STRING_LOCAL_VARS]))
	{
		pcForm=pcLocals->pcNext;
		pcLocals=pcLocals->pfForm->pcArgs;
	}
	else
	{
		pcForm=pcLocals;
		pcLocals=NULL;
	}


	nLength=0;
	for(pc=pcArguments;pc;pc=pc->pcNext)
		nLength++;

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->psName=IdentName(pcName->pfForm);
	pf->paAction=&aDefGeneratorAction;
	pf->pcVars=pcArguments;
	pf->pcGenLit=pcLocals;
	pf->pcArgs=pcForm;

	/* install generator in symbol-info table (if it isn't there already) */

	if(SymbolLookup(GetName(pcName->pfForm,ac),&psSymbol))	/* if symbol already declared */
	{
		if(psSymbol->nType==ATOM_SYMBOLINFOP)
		{
			psiSymbolInfo=psSymbol->uValue.psiSymbolInfo;
			if(psiSymbolInfo->pcFormula)
			{
				ErrorMessage("def-defined-generator:  Generator %s already defined\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
			}
			else
			{
				if(psiSymbolInfo->nArity!=nLength)
				{
					ErrorMessage(
						"def-defined-generator:  %s arity declared as %d, defined as %d\n",
						psiSymbolInfo->psName,psiSymbolInfo->nArity,nLength);
					bStatus=FALSE;
				}
				if(psiSymbolInfo->nSymbolType!=SYMBOL_GENERATOR)
				{	
					ErrorMessage(
						"def-defined-generator:  %s defined as generator, declared as non-generator\n",
						psiSymbolInfo->psName);
					bStatus=FALSE;
				}
				psiSymbolInfo->pcFormula=pc;
			}
		}
		else
		{
			ErrorMessage("def-defined-generator:  Symbol %s already defined\n",
				GetName(pcName->pfForm,ac));
			bStatus=FALSE;
		}
	}
	else
	{
		InsertSymbolInfo(GetName(pcName->pfForm,ac),SYMBOL_GENERATOR,nLength,EVAL_DEFINED,
			(void (*)(void))GenerateDefGenerator,pc,FALSE);
	}
	EXIT("EvalDefDefinedGenerator");
	return bStatus;
}

/* EvalDefDefinedFunction (Interface Command)

Description:
	Parse and store a defined function.
*/

BOOL EvalDefDefinedFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	SYMBOLP psSymbol;					/* pointer to symbol table entry */
	SYMBOLINFOP psiSymbolInfo;			/* symbol information pointer */
	CELLP pcName;
	CELLP pcArguments;
	CELLP pcLocals;						/* local variables */
	CELLP pcForm;
	CELLP pc;
	FORMULAP pf;	
	int nLength;
	BOOL bStatus;
	char ac[40];

	ENTER("EvalDefDefinedFunction",FALSE);

	/* set up useful pointers */

	bStatus=TRUE;
	pcName=pcFormula->pfForm->pcArgs;
	if(Reserved(GetName(pcName->pfForm,ac)))
	{
		ErrorMessage("def-defined-function: %s is a reserved symbol, "
			"please use a different name for this symbol\n",
			GetName(pcName->pfForm,ac));
		return FALSE;
	}
	pcArguments=pcName->pfForm->pcArgs;
	pcLocals=pcName->pcNext;
	if(StringEqQ(IdentName(pcLocals->pfForm),apsStringTab[STRING_LOCAL_VARS]))
	{
		pcForm=pcLocals->pcNext;
		pcLocals=pcLocals->pfForm->pcArgs;
	}
	else
	{
		pcForm=pcLocals;
		pcLocals=NULL;
	}

	nLength=0;
	for(pc=pcArguments;pc;pc=pc->pcNext)
		nLength++;

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->psName=IdentName(pcName->pfForm);
	pf->paAction=&aDefFunctionAction;
	pf->pcVars=pcArguments;
	pf->pcGenLit=pcLocals;
	pf->pcArgs=pcForm;

	/* install function in symbol-info table (if it isn't there already) */

	if(SymbolLookup(GetName(pcName->pfForm,ac),&psSymbol))
	{
		if(psSymbol->nType==ATOM_SYMBOLINFOP)
		{
			psiSymbolInfo=psSymbol->uValue.psiSymbolInfo;
			if(psiSymbolInfo->pcFormula)
			{
				ErrorMessage("def-defined-function:  Function %s already defined\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
			}
			else
			{
				if(psiSymbolInfo->nArity!=nLength)
				{
					ErrorMessage(
						"def-defined-function:  %s arity declared as %d, defined as %d\n",
						psiSymbolInfo->psName,psiSymbolInfo->nArity,nLength);
					bStatus=FALSE;
				}
				if(psiSymbolInfo->nSymbolType!=SYMBOL_FUNCTION)
				{
					ErrorMessage(
						"def-defined-function:  %s defined as function, declared as non-function\n",
						psiSymbolInfo->psName);
					bStatus=FALSE;
				}
				psiSymbolInfo->pcFormula=pc;
			}
		}
		else
		{
			ErrorMessage("def-defined-function:  Symbol %s already defined\n",
				GetName(pcName->pfForm,ac));
			bStatus=FALSE;
		}
	}
	else
		InsertSymbolInfo(GetName(pcName->pfForm,ac),SYMBOL_FUNCTION,nLength,EVAL_DEFINED,
			(void (*)(void))ComputeDefFunction,pc,FALSE);

	EXIT("EvalDefDefinedFunction");
	return bStatus;
}

/* EvalDefDefinedMacro (Interface Command)

Description:
	Parse and store a defined macro.
*/

BOOL EvalDefDefinedMacro
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	SYMBOLP psSymbol;					/* pointer to symbol table entry */
	SYMBOLINFOP psiSymbolInfo;			/* symbol information pointer */
	CELLP pcName;
	CELLP pcArguments;
	CELLP pcLocals;						/* local variables */
	CELLP pcForm;
	CELLP pc;
	FORMULAP pf;	
	int nLength;
	BOOL bStatus;
	char ac[40];

	ENTER("EvalDefDefinedMacro",FALSE);

	/* set up useful pointers */

	bStatus=TRUE;
	pcName=pcFormula->pfForm->pcArgs;
	if(Reserved(GetName(pcName->pfForm,ac)))
	{
		ErrorMessage("def-defined-macro: %s is a reserved symbol, "
			"please use a different name for this symbol\n",
			GetName(pcName->pfForm,ac));
		return FALSE;
	}
	pcArguments=pcName->pfForm->pcArgs;
	pcLocals=pcName->pcNext;
	if(StringEqQ(IdentName(pcLocals->pfForm),apsStringTab[STRING_LOCAL_VARS]))
	{
		pcForm=pcLocals->pcNext;
		pcLocals=pcLocals->pfForm->pcArgs;
	}
	else
	{
		pcForm=pcLocals;
		pcLocals=NULL;
	}

	nLength=0;
	for(pc=pcArguments;pc;pc=pc->pcNext)
		nLength++;

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->psName=IdentName(pcName->pfForm);
	pf->paAction=&aDefMacroAction;
	pf->pcVars=pcArguments;
	pf->pcGenLit=pcLocals;
	pf->pcArgs=pcForm;

	/* install macro in symbol-info table (if it isn't there already) */

	if(SymbolLookup(GetName(pcName->pfForm,ac),&psSymbol))
	{
		if(psSymbol->nType==ATOM_SYMBOLINFOP)
		{
			psiSymbolInfo=psSymbol->uValue.psiSymbolInfo;
			if(psiSymbolInfo->pcFormula)
			{
				ErrorMessage("def-defined-macro:  Macro %s already defined\n",
					GetName(pcName->pfForm,ac));
				bStatus=FALSE;
			}
			else
			{
				if(psiSymbolInfo->nArity!=nLength)
				{
					ErrorMessage(
						"def-defined-macro:  %s arity declared as %d, defined as %d\n",
						psiSymbolInfo->psName,psiSymbolInfo->nArity,nLength);
					bStatus=FALSE;
				}
				if(psiSymbolInfo->nSymbolType!=SYMBOL_MACRO)
				{
					ErrorMessage(
						"def-defined-macro:  %s defined as macro, declared as non-macro\n",
						psiSymbolInfo->psName);
					bStatus=FALSE;
				}
				psiSymbolInfo->pcFormula=pc;
			}
		}
		else
		{
			ErrorMessage("def-defined-macro:  Symbol %s already defined\n",
				GetName(pcName->pfForm,ac));
			bStatus=FALSE;
		}
	}
	else
		InsertSymbolInfo(GetName(pcName->pfForm,ac),SYMBOL_MACRO,nLength,EVAL_DEFINED,
			(void (*)(void))ComputeDefMacro,pc,FALSE);

	EXIT("EvalDefDefinedMacro");
	return bStatus;
}

/* -------------------------------------------------------------------------- */

/* EvalClearWorldSymbols (Interface Command)

Description:
	Clean up the current world symbols.
Scheme:

(define (clear-world-symbols)
  "Clean up the current world symbols"
  (do ((i 0 (+ 1 i)))
      ((>= i *number-of-described-symbols*))
	(put-symbol-index (get-symbol-name i) '()))
  (set! *world-symbols* (make-vector 0))
  (set! *number-of-world-symbols* 0)
  (set! *number-of-described-symbols* 0)
  (reset-tl-control)
  (reset-print-world-fn))
*/

BOOL EvalClearWorldSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i;

	ENTER("EvalClearWorldSymbols",FALSE);
	for(i=0;i<MAXSYMBOLS;i++)
	{
		asiWorldSymbols[i].psName=NULL;
		asiWorldSymbols[i].nSymbolType=SYMBOL_UNKNOWN;
		asiWorldSymbols[i].nEvalType=EVAL_UNKNOWN;
		asiWorldSymbols[i].pfFunction=NULL;	/* this is a function pointer, not memory */
		asiWorldSymbols[i].pcFormula=NULL;
	}
	nNumberOfWorldSymbols=0;
	nNumberOfDescribedSymbols=0;
	nNumberOfCheckedSymbols=0;
	bAtemporalControl=FALSE;
	bConcurrentPlanning=FALSE;
	bCycleChecking=TRUE;
	ResetTLControl();
	ResetPrintWorldFn();
	ResetPriorityFn();
	pfSearchStrategy=(SEARCHP)DepthFirstPriority;
	ResetHeuristicFn();
	pcInitializationSequence=NULL;
	pcGoalSequence=NULL;
	pcInitialFacts=NULL;
	pbGlobalVariables=NULL;
	pcSearchGlobalInit=NULL;
	plpInitialWorld=NULL;
	plpGoalWorld=NULL;
	CloseLibraries();
	nErrorCount=0;
	ClearOps();							/* clear operators */
	FreeMacroOperators();
	FreeElidedOperators();
	InitSymbolTable();					/* reinitialize the hash table */

	plpCurrentPlan->apbtWorld=NULL;

	EXIT("EvalClearWorldSymbols");
	return TRUE;
}

/* EvalClearOperators (Interface Command)

Description:
	Delete all operators.  We also clobber the initialization sequence!
*/

BOOL EvalClearOperators
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalClearOperators",FALSE);
	ClearOps();
	pcInitializationSequence=NULL;
	EXIT("EvalClearOperators");
	return TRUE;
}

/* EvalClearEventQueue (Interface Command)

Description:
	Empty the concurrent event queue.
*/

BOOL EvalClearEventQueue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalClearEventQueue",FALSE);
	ClearQueue();
	EXIT("EvalClearEventQueue");
	return TRUE;
}

/* EvalPrintPlanList (Interface Command)

Description:
	Print the current plan.
*/

BOOL EvalPrintPlanList
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nFile;
	FILE *pfFile;
	BOOL status;

	ENTER("EvalPrintPlanList",TRUE);

	if(!FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile))
	{
		EXIT("EvalPrintPlanList");
		return FALSE;
	}
	pfFile=HandleToFileStream(nFile);
	if(!pfFile)
	{
		ErrorMessage("print-plan-list:  Invalid file handle, %d\n",nFile);
		EXIT("PrintPlan");
		return FALSE;
	}

	status=PrintPlanList(plpLinearPlan,pfFile);
	EXIT("EvalPrintPlanList");
	return status;
}

/* EvalPrintPddlPlan

Description:
	Print the current plan to the PDDL output file.
*/

BOOL EvalPrintPddlPlan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plpSuccessors;

	ENTER("EvalPrintPddlPlan",TRUE);

	plpSuccessors=FollowLinearPlan(srSearchResult.plpState);
	PrintPddlPlan(plpSuccessors);
	EXIT("EvalPrintPddlPlan");
	return TRUE;
}

/* PrintPlanList

Description:
	Print the current plan.
*/

BOOL PrintPlanList
(
	LINEARPLANP plpLinearPlan,
	FILE *pfFile
)
{
	LINEARPLANP plp;
	CELLP pc,pcPrev;

	ENTER("PrintPlanList",TRUE);

	if(plpLinearPlan)
	{
		/* link up the plan in forward order */
	
		pc=pcPrev=NULL;
		for(plp=plpLinearPlan;plp->pcActionName;plp=plp->plpParent)
		{
			pc=CopyCell(plp->pcActionName);
			pc->pcNext=pcPrev;
			pcPrev=pc;
		}
		
		/* print the plan list */

		for(;pc;pc=pc->pcNext)
			PrintFormula(pfFile,pc,0);

		EXIT("PrintPlanList");
		return TRUE;
	}

	EXIT("PrintPlanList");
	return FALSE;
}

/* Initial World Interface -------------------------------------------------- */

/* EvalSetInitialWorld (Interface Command)

Description:
	Set the current initial world for the planner.
Scheme:

(define (set-initial-world  world-list)
  (set! initial-world/action
	(make-world/action (create-world world-list) '() 0 0 0))
  world-list)
*/

BOOL EvalSetInitialWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcStart,pcEnd;				/* list head */
	int i;
//	double dfInitializationTime;

	ENTER("EvalSetInitialWorld",FALSE);
	if(!nNumberOfDescribedSymbols)
	{
		ErrorMessage("set-initial-world: No described symbols declared.  "
			"Call declare-described-symbols first.\n");
		EXIT("EvalSetInitialWorld");
		return FALSE;
	}

	/* set up time queue for concurrent planning */

	if(bConcurrentPlanning)
		ClearQueue();

	/* append initial facts to list of formulas */

	pcStart=pcFormula->pfForm->pcArgs;
	
	/* we may have a random world generator here.  We look for a
	non-described function instead of a list of described
	functions and predicates.  If we find one, we compute the function 
	to get the desired list. */
	
//	dfInitializationTime=GetInternalRunTime();

	if(pcStart&&pcStart->pfForm->nType==ATOM_SYMBOLINFOP&&
		!DescribedQ(pcStart->pfForm->uValue.psiSymbolInfo)&&
		FunctionQ(pcStart->pfForm->uValue.psiSymbolInfo))
		pcStart=(*pcStart->pfForm->paAction->pfCompute)(pcStart,plpLinearPlan,pbBindings);
	
	for(pcEnd=(CELLP)&pcStart;pcEnd->pcNext;pcEnd=pcEnd->pcNext);

	pcEnd->pcNext=CopyCellList(pcInitialFacts);
	
//	dfInitializationTime=GetInternalRunTime()-dfInitializationTime;
//	CommandPrintf(stderr,"\nWorld Generator:  Elapsed CPU time %.3f sec.\n",dfInitializationTime);
	
	/* create the initial world */
	
//	dfInitializationTime=GetInternalRunTime();

	pcStart=MakeAddForm(FALSE,pcStart);
	plpInitialWorld=CreateInitialWorldAction(pcStart,NULL,0,0,0,pbBindings);
	WorldSignature(plpInitialWorld);

//	dfInitializationTime=GetInternalRunTime()-dfInitializationTime;
//	CommandPrintf(stderr,"\nInitial World:  Elapsed CPU time %.3f sec.\n",dfInitializationTime);

	/* balance BTrees in initial world */
	
	for(i=0;i<nNumberOfDescribedSymbols;i++)
		plpInitialWorld->apbtWorld[i]=BTreeBalance(plpInitialWorld->apbtWorld[i]);

	EXIT("EvalSetInitialWorld");
	return TRUE;
}

/* EvalSetInitializationSequence (Interface Command)

Description:
	Set the initialization sequence for the planner.
	This list of formulas will be executed in the context of iSetInitialWorld.
*/

BOOL EvalSetInitializationSequence
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalSetInitializationSequence",FALSE);
	if(!nNumberOfDescribedSymbols)
	{
		ErrorMessage("set-initialization-sequence: No described symbols declared.  "
			"Call declare-described-symbols first.\n");
		EXIT("EvalSetInitializationSequence");
		return FALSE;
	}

	// we append initialization sequences in the order we encounter them
	
//	for(pc=(CELLP)&pcInitializationSequence;pc->pcNext;pc=pc->pcNext);
//	pc->pcNext=pcFormula->pfForm->pcArgs;	/* set initialization list */

	// debug -- prefix the list instead

	for(pc=pcFormula->pfForm->pcArgs;pc->pcNext;pc=pc->pcNext);
	pc->pcNext=pcInitializationSequence;
	pcInitializationSequence=pcFormula->pfForm->pcArgs;
	
	EXIT("EvalSetInitializationSequence");
	return TRUE;
}

/* EvalSetInitialFacts (Interface Command)

Description:
	Setup the known initial described predicates and functions.
	The input consists of one or more lists of described predicates and functions.
*/

BOOL EvalSetInitialFacts
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalSetInitialFacts",FALSE);
	if(!nNumberOfDescribedSymbols)
	{
		ErrorMessage("set-initial-facts: No described symbols declared.  "
			"Call declare-described-symbols first.\n");
		EXIT("EvalSetInitialFacts");
		return FALSE;
	}

	pcInitialFacts=pcFormula->pfForm->pcArgs;	/* set initializer list */
	EXIT("EvalSetInitialFacts");
	return TRUE;
}

/* GetInitialPlan

Description:
	Return the initial world of the current planning problem.
Scheme:

(define (get-initial-world )
  initial-world/action)
*/

LINEARPLANP GetInitialPlan(void)
{
	ENTER("GetInitialPlan",TRUE);
	EXIT("GetInitialPlan");
	return plpInitialWorld;
}

/* Goal Interface -------------------------------------------------------------- */

/*
Classic Goals:
	By classic goals we mean a list of positive literals to be achieved.
	To check for such goals, we can construct a goal world (used to
	evaluate goal modalities), and set "FinalWorldGoal?" to
	simply evaluate the conjunction	of the goal literals in the world.
	Later on we have to do other things for different types of goals.
Notes:
	In C, a useful goal requires two parts.  We must define both a goal evaluator
	(a pointer to a function) and a formula for it to evaluate.
*/

/* ProcessGoal...

Description:
	Set the components for a planning problem.
Scheme:

(define process-goal process-classic-goal)
(define initial-world/action '())
(define goal-list '())
(define initial-time 0)
(define goal-type 'classic)
(define plan= plan-classic=)
(define tlform '(always (TRUE)))        ;This provides no search control.
*/

BOOL ClassicGoalQ
(
	CELLP pcTLForm,
	LINEARPLANP plpLinearPlan
)
{
	BOOL b;

	ENTER("ClassicGoalQ",FALSE);
	b=pcGoalFormula?
		(*pcGoalFormula->pfForm->paAction->pfEval)(pcGoalFormula,plpLinearPlan,pbGlobalVariables):TRUE;
	EXIT("ClassicGoalQ");
	return b;
}

/* ProcessClassicGoal

Description:
	This function sets up the planner to find a classic conjunctive goal.
Notes:
	Since we assume that all goal clauses are "described", we don't
	bother to optimize the goal formula by raising lower level ANDs.
Scheme:

(define (process-classic-goal goal-list)
	(create-goal-world/action goal-list)
	(set! final-world-goal?
		(lambda (tlform world/action)
			(eval-formula `(and ,@goal-list) world/action '()))))
*/

static void ProcessClassicGoal
(
	CELLP pcFormula,					/* list of goal formulas */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i;
	CELLP pcStart;

	ENTER("ProcessClassicGoal",FALSE);

	pcStart=MakeAddForm(FALSE,pcFormula);
	plpGoalWorld=CreateGoalWorldAction(pcStart,NULL,0,0,0,pbBindings);
	WorldSignature(plpGoalWorld);

	/* balance BTrees in goal world */
	
	for(i=0;i<nNumberOfDescribedSymbols;i++)
		plpGoalWorld->apbtWorld[i]=BTreeBalance(plpGoalWorld->apbtWorld[i]);
	
	/* set up goal formula */
	
	if(!pcFormula||!pcFormula->pcNext)
		pcGoalFormula=pcFormula;
	else
		pcGoalFormula=MakeAndForm(FALSE,pcFormula);
	pfFinalWorldGoalQ=ClassicGoalQ;
	EXIT("ProcessClassicGoal");
}

/* MTL extension. Goal formulas are dealt with by labeling the
initial world with them and progressing through all worlds
generated. To test completion we see if the formula will be
satisfied by a final world that idles.
The function that does this is part of the progress-formula code.
*/

/* ProcessExtendedGoal

Description:
	This function sets up the plan to find a temporally extended goal.
Scheme:

(define (process-extended-goal goal-formula)
  (set-tl-control goal-formula)
  (set! final-world-goal? (lambda (tlform world/action)
		(idle-formula tlform world/action '()))))
*/

void ProcessExtendedGoal
(
	CELLP pcFormula,					/* temporal goal formula */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("ProcessExtendedGoal",TRUE);
	pcTLForm=pcFormula;
	pfFinalWorldGoalQ=ExtendedGoalQ;
	EXIT("ProcessExtendedGoal");
}

BOOL ExtendedGoalQ
(
	CELLP pcTLForm,
	LINEARPLANP plpLinearPlan
)
{
	BOOL b;

	ENTER("ExtendedGoalQ",TRUE);
	b=IdleFormula(pcTLForm,plpLinearPlan,NULL);
	EXIT("ExtendedGoalQ");
	return b;
}

/* EvalSetGoalType

Description:
	Set the type of goal being planned for.
Scheme:

(define (set-goal-type type)
  (cond
   ((eq? type 'classic)
	(set! goal-type 'classic)
	(set! plan= plan-classic=)
	(set! process-goal process-classic-goal))
   ((eq? type 'extended)
	(set! goal-type 'extended)
	(set! plan= plan-extended=)
	(set! process-goal process-extended-goal))
   (else
	(format  #t "~%Only classic and extended goals are implemented so far,
				  ~%setting to classic by  default.")
	(set! process-goal process-classic-goal))))
*/

BOOL EvalSetGoalType
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;
	CELLP pc;
	
	ENTER("EvalSetGoalType",FALSE);

	bStatus=TRUE;
	pc=pcFormula->pfForm->pcArgs;
	if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_EXTENDED]))
	{
		pfPlanEqQ=PlanExtendedEqQ;
		pfProcessGoal=ProcessExtendedGoal;
	}
	else
	{
		if(!StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_CLASSIC]))
		{
			ErrorMessage("Only classic and extended goals are available.\n"
				"Setting goal type to classic by default.\n");
			bStatus=FALSE;
		}
		pfPlanEqQ=PlanClassicEqQ;
		pfProcessGoal=ProcessClassicGoal;
	}
	EXIT("EvalSetGoalType");
	return bStatus;
}

/* EvalSetGoal (Interface Command)

Description:
	Set the current goal for the planner.
Scheme:

(define (set-goal goal)
	(set! goal-list goal)
	(process-goal goal)
	goal-list)
*/

BOOL EvalSetGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalSetGoal",FALSE);
	if(!nNumberOfDescribedSymbols)
	{
		ErrorMessage("set-goal:  No described symbols declared.  "
			"Call declare-described-symbols first.\n");
		EXIT("EvalSetGoal");
		return FALSE;
	}

	pc=pcFormula->pfForm->pcArgs;

	/* we may have a random goal generator here.  We look for a
	non-described function instead of a list of described
	functions and predicates.  If we find one, we compute the function 
	to get the desired list. */
	
	if(pc&&pc->pfForm->nType==ATOM_SYMBOLINFOP&&
		!DescribedQ(pc->pfForm->uValue.psiSymbolInfo)&&
		FunctionQ(pc->pfForm->uValue.psiSymbolInfo))
		pc=(*pc->pfForm->paAction->pfCompute)(pc,plpLinearPlan,pbBindings);

	/* create the goal world and set up the goal formula */

	(*pfProcessGoal)(pc,plpLinearPlan,pbBindings);

	EXIT("EvalSetGoal");
	return TRUE;
}

/* EvalSetGoalFormula (Interface Command)

Description:
	Set the current goal for the planner, based on the current
	state of the goal world.  The goal world is enumerated to generate
	the goal formula.
*/

BOOL EvalSetGoalFormula
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BTREEP *apbtWorld;
	CELLP pcStart,pcEnd;
	int i;

	ENTER("EvalSetGoalFormula",FALSE);
	if(!nNumberOfDescribedSymbols)
	{
		ErrorMessage("set-goal-formula:  No described symbols declared.  "
			"Call declare-described-symbols first.\n");
		EXIT("EvalSetGoalFormula");
		return FALSE;
	}
	if(pfProcessGoal!=ProcessClassicGoal)
	{
		ErrorMessage("set-goal-formula:  Cannot generate a goal formula for non-classic goal.\n");
		EXIT("EvalSetGoalFormula");
		return FALSE;
	}

	/* create the goal world and set up the goal formula */

	(*pfProcessGoal)(NULL,plpLinearPlan,pbBindings);

	/* enumerate all goal world described symbols */

	apbtWorld=LinearPlanWorld(plpGoalWorld);
	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	for(i=0;i<nNumberOfDescribedSymbols;i++)
	{
		pcEnd->pcNext=ArgTreeToFormulaList(apbtWorld,i);
		for(;pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	}
	
	/* generate conjunctive formula */

	if(pcStart&&pcStart->pcNext)
		pcGoalFormula=MakeAndForm(FALSE,pcStart);
	else
		pcGoalFormula=pcStart;
	
	EXIT("EvalSetGoalFormula");
	return TRUE;
}

/* EvalSetGoalSequence (Interface Command)

Description:
	Set the goal sequence for the planner.
	This list of formulas will be executed in the context of SetGoal or SetGoalFormula.
*/

BOOL EvalSetGoalSequence
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalSetGoalSequence",FALSE);
	if(!nNumberOfDescribedSymbols)
	{
		ErrorMessage("set-goal-sequence: No described symbols declared.  "
			"Call declare-described-symbols first.\n");
		EXIT("EvalSetGoalSequence");
		return FALSE;
	}
	if(pfProcessGoal!=ProcessClassicGoal)
	{
		ErrorMessage("set-goal-sequence:  Cannot process a goal sequence for non-classic goal.\n");
		EXIT("EvalSetGoalSequence");
		return FALSE;
	}

	// we append initialization sequences in the order we encounter them
	
	for(pc=(CELLP)&pcGoalSequence;pc->pcNext;pc=pc->pcNext);
	pc->pcNext=pcFormula->pfForm->pcArgs;	/* set initialization list */
	EXIT("EvalSetGoalSequence");
	return TRUE;
}

/* TLControl Interface --------------------------------------------------------- */

/* EvalSetTLControl (Interface Command)

Description:
	Set the temporal control formula to be used by the planner.
Scheme:

(define (set-tl-control formula)
  (set! tlform formula))
*/

BOOL EvalSetTLControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalSetTLControl",FALSE);
	pcTLForm=pcFormula->pfForm->pcArgs;
	EXIT("EvalSetTLControl");
	return TRUE;
}

/* ComputeGetTLControl (Interface Command)

Description:
	Return the temporal control formula being used by the planner.
Scheme:

(define (get-tl-control )
  tlform)
*/

CELLP ComputeGetTLControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("ComputeGetTLControl",FALSE);
	EXIT("ComputeGetTLControl");
	return pcTLForm;
}

/* EvalResetTLControl (Interface Command)

Description:
	Set the temporal control formula to be no search control.
Scheme:

(define (reset-tl-control)
  (set! tlform '(always (TRUE))))
*/

BOOL EvalResetTLControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetTLControl",FALSE);
	ResetTLControl();
	EXIT("EvalResetTLControl");
	return TRUE;
}

/* ResetTLControl

Description:
	Set the temporal control formula to be no search control.
Scheme:

(define (reset-tl-control)
  (set! tlform '(always (TRUE))))
*/

void ResetTLControl(void)
{
	ENTER("ResetTLControl",TRUE);
	pcTLForm=StringToFormula("(always (TRUE))");
	EXIT("ResetTLControl");
}

/* EvalLoadPlan -------------------------------------------------------------------

Description:
	Load a plan... the previous plan is deleted.
	An initial world must already be defined.
*/

BOOL EvalLoadPlan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i;								/* loop index */
	BOOL bError;
	jmp_buf *pjbSaved;

	ENTER("EvalLoadPlan",FALSE);

	srSearchResult.psPlanStatus=apsStringTab[STRING_NO_PLAN];	/* assume plan fails */


	/* make sure we can do this */
	
	bError=FALSE;
	for(i=0;i<nNumberOfWorldSymbols;i++)
	{
		if(asiWorldSymbols[i].nSymbolType==SYMBOL_UNKNOWN)
		{
			ErrorMessage("load-plan:  Symbol %s has unknown type\n",
				asiWorldSymbols[i].psName);
			bError=TRUE;
		}
		if(asiWorldSymbols[i].nEvalType==EVAL_UNKNOWN)
		{
			ErrorMessage("load-plan:  Symbol %s is undefined\n",
				asiWorldSymbols[i].psName);
			bError=TRUE;
		}
	}
	if(bError)
	{
		ErrorMessage("load-plan:  Ensure all predicates, functions and generators ",
			"are correctly defined\nbefore calling load-plan.\n");
		EXIT("EvalLoadPlan");
		return FALSE;
	}
	if(nErrorCount&&!bInteractiveMode)
	{
		ErrorMessage("load-plan:  %d Error%s encountered.\n"
			"Please correct any problems and call clear-world-symbols before proceeding.\n",
			nErrorCount,nErrorCount==1?" was":"s were");
		EXIT("EvalLoadPlan");
		return FALSE;
	}
	if(!GetInitialPlan())
	{
		ErrorMessage("load-plan:  No initial world.\n"
			"Please call set-initial-world before proceeding.\n");
		EXIT("EvalLoadPlan");
		return FALSE;
	}

	/* plan according to predefined plan */
	
	pcPredefinedPlan=pcFormula->pfForm->pcArgs;
	pfSearchStrategy=(SEARCHP)FollowPredefinedPlan;
	pfPlanEqQ=PlanClassicEqQ;
	
	/* call the planner */

	nLineNumber=0;						/* turn off line number messages */
	pjbSaved=pjbCurrentJump;
	pjbCurrentJump=&jbTLPlannerJump;
	if(!setjmp(jbTLPlannerJump))		/* set up for quick exit from planner */
		TLPlanner(GetInitialPlan(),pcTLForm,pfSearchStrategy,
			pcHeuristicFn,pcPriorityFn,pbBindings);
	pjbCurrentJump=pjbSaved;

	pcPredefinedPlan=NULL;				/* discard predefined plan */
	EXIT("EvalLoadPlan");
	return TRUE;
}

/* EvalSelectInitialWorld

Description:
	Make the initial world the current plan.
*/

BOOL EvalSelectInitialWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;

	ENTER("EvalSelectInitialWorld",FALSE);

	bStatus=TRUE;
	if(plpInitialWorld)
		plpCurrentPlan=plpInitialWorld;
	else
		bStatus=FALSE;

	EXIT("EvalSelectInitialWorld");
	return bStatus;
}

/* EvalSelectFinalWorld

Description:
	Make the final world the current plan.
*/

BOOL EvalSelectFinalWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;

	ENTER("EvalSelectFinalWorld",FALSE);

	bStatus=TRUE;
	if(plpFinalPlan)
		plpCurrentPlan=plpFinalPlan;
	else
		bStatus=FALSE;

	EXIT("EvalSelectFinalWorld");
	return bStatus;
}

/* EvalSelectNextWorld

Description:
	Make the next world the current plan.
*/

BOOL EvalSelectNextWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;

	ENTER("EvalSelectNextWorld",FALSE);

	bStatus=TRUE;
	if(plpCurrentPlan->plpNext)
		plpCurrentPlan=plpCurrentPlan->plpNext;
	else
		bStatus=FALSE;

	EXIT("EvalSelectNextWorld");
	return bStatus;
}

/* EvalSelectPreviousWorld

Description:
	Make the previous world the current plan.
*/

BOOL EvalSelectPreviousWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bStatus;

	ENTER("EvalSelectPreviousWorld",FALSE);

	bStatus=TRUE;
	if(plpCurrentPlan->plpParent)
		plpCurrentPlan=plpCurrentPlan->plpParent;
	else
		bStatus=FALSE;

	EXIT("EvalSelectPreviousWorld");
	return bStatus;
}

