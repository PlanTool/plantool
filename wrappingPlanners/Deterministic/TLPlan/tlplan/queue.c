/* queue.c

Description:
	Concurrent time queue support.
*/

#include <limits.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "compute.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "makeform.h"
#include "queue.h"
#include "save.h"
#include "tlparse.h"
#include "util.h"
#include "var.h"
#include "zone.h"

// time queue

typedef struct tq
{
	struct tq *ptqNext;					// pointer to next
	struct tq *ptqPrev;					// pointer to previous
	double dfTime;						// time of dequeue
	LINEARPLANP plpProgenitor;			// pointer to progenitor world
	CELLP pcTag;						// tag formula
	CELLP pcFormula;					// formula to execute
	BINDINGP pbBindings;				// action context
}TQ, *TQP;

typedef struct tqlink
{
	struct tqlink *ptqlNext;			// pointer to next
	CELLP pcTag;						// instantiated tag
	struct tq *ptqDelayedAction;		// pointer to delayed action in queue
}TQLINK, *TQLINKP;

/* global data */

/* local data */

static TQ tqHead;						/* queue head */
//static double dfQueueTime;			/* the time of the most recently dequeued delayed-action */

/* local function prototypes */

CELLP Instantiate
(
	CELLP pcFormula,					// formula to instantiate
	BINDINGP pbBindings					// bindings list
);
static CELLP Instant
(
	CELLP pcFormula,					// formula to instantiate
	BINDINGP pbBindings					// bindings list
);

/* ClearQueue ------------------------------------------------------------------

Description:
	Clear the time queue.
*/

void ClearQueue(void)
{
//	dfQueueTime=0.0;

	tqHead.ptqNext=&tqHead;
	tqHead.ptqPrev=&tqHead;
}
										
/* EvalGlobalDelayedAction (Pseudo predicate)

Description:
	Queue an action formula for later execution.  This formula is not associated
	with any world, so that it is accessable to all worlds.
Notes:
	We save the bindings afforded by the operator, so we can instantiate the 
	free variables in tag and formula, since the action will be dequeued at a later time.
Call:
	(global-delayed-action delta tag formula)
Where
	delta is an incremental time.
	tag is a dummy formula, used to name the action when it is finally dequeued
	formula is to be executed by a dequeue.
*/

BOOL EvalGlobalDelayedAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcForm;
	CELLP pcTag;
	CELLP pcArg;
	double dfDelta;
	TQP ptq,ptq1;

	if(!tqHead.ptqNext||!tqHead.ptqPrev)
	{
		ErrorMessage("global-delayed-action:  Call (enable concurrent-planning) first.\n");
		return FALSE;
	}

	// fetch the incremental time

	pcArg=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pcArg,plpLinearPlan,pbBindings,&dfDelta))
		TermError("global-delayed-action",pcFormula,pbBindings);
	if(dfDelta<=0.0)
	{
		Message("global-delayed-action:  Warning, delta time is non-positive.\n");
		PrintFormula(pfTraceStream,pcFormula,0);
	}
	// fetch the tag
	
	pcArg=pcArg->pcNext;
	if(!pcArg)
		TermError("global-delayed-action",pcFormula,pbBindings);
	pcTag=CopyCell(pcArg);
	
	// fetch the formula
	
	pcArg=pcArg->pcNext;
	if(!pcArg)
		TermError("global-delayed-action",pcFormula,pbBindings);
	pcForm=CopyCell(pcArg);
	
	// fill in queue entry

	ptq=MemAlloc(sizeof(TQ));
	ptq->dfTime=plpLinearPlan->dfTime+dfDelta;
	ptq->pcTag=pcTag;
	ptq->pcFormula=pcForm;
	ptq->pbBindings=CopyBindings(pbBindings);
	ptq->plpProgenitor=NULL;

//	CommandPrintf(pfTraceStream,"Delayed-action world %04d time %f\n",
//		ptq->plpProgenitor->nWorldNumber,ptq->dfTime);

	// insert entry into queue in time order

	for(ptq1=tqHead.ptqNext;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
	{
		if(ptq1->dfTime>ptq->dfTime)
			break;
	}
	ptq->ptqNext=ptq1;
	ptq->ptqPrev=ptq1->ptqPrev;
	ptq1->ptqPrev=ptq;
	ptq->ptqPrev->ptqNext=ptq;
	
	return TRUE;
}

/* EvalDelayedAction (Pseudo predicate)

Description:
	Queue an action formula for later execution.
Notes:
	We save the bindings afforded by the operator, so we can instantiate the 
	free variables in tag and formula, since the action will be dequeued at a later time.
Call:
	(delayed-action delta tag formula)
Where
	delta is an incremental time.
	tag is a dummy formula, used to name the action when it is finally dequeued
	formula is to be executed by a dequeue.
*/

//double dfDelayedActionTime;
//int nDelayedAction;

BOOL EvalDelayedAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcForm;
	CELLP pcTag;
	CELLP pcArg;
	double dfDelta;
	TQP ptq,ptq1;

	if(!tqHead.ptqNext||!tqHead.ptqPrev)
	{
		ErrorMessage("delayed-action:  Call (enable concurrent-planning) first.\n");
		return FALSE;
	}

//dfDelayedActionTime-=GetInternalRunTime();
//nDelayedAction+=1;

	// fetch the incremental time

	pcArg=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pcArg,plpLinearPlan,pbBindings,&dfDelta))
		TermError("delayed-action",pcFormula,pbBindings);
	if(nPddlSupport)
	{
		if(dfDelta<0.0)
		{
			Message("delayed-action:  Warning, delta time is negative.\n");
			PrintFormula(stderr,pcFormula,0);
			PrintFormula(pfTraceStream,pcFormula,0);
		}
	}
	else
	{
		if(dfDelta<=0.0)
		{
			Message("delayed-action:  Warning, delta time is non-positive.\n");
			PrintFormula(stderr,pcFormula,0);
			PrintFormula(pfTraceStream,pcFormula,0);
		}
	}

	// fetch the tag
	
	pcArg=pcArg->pcNext;
	if(!pcArg)
		TermError("delayed-action",pcFormula,pbBindings);
	pcTag=CopyCell(pcArg);
	
	// fetch the formula
	
	pcArg=pcArg->pcNext;
	if(!pcArg)
		TermError("delayed-action",pcFormula,pbBindings);
	pcForm=CopyCell(pcArg);
	
	// fill in queue entry

	ptq=MemAlloc(sizeof(TQ));
	if(nPddlSupport)
		ptq->dfTime=plpSuccessorWorld->dfTime+dfDelta+1.0e-3;
	else
		ptq->dfTime=plpSuccessorWorld->dfTime+dfDelta;
	ptq->pcTag=pcTag;
	ptq->pcFormula=pcForm;
	ptq->pbBindings=CopyBindings(pbBindings);
	ptq->plpProgenitor=plpSuccessorWorld;
	plpSuccessorWorld->dfDuration=dfDelta;

	// insert entry into queue in time order

	for(ptq1=tqHead.ptqNext;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
	{
		if(ptq1->dfTime>ptq->dfTime)
			break;
	}
	ptq->ptqNext=ptq1;
	ptq->ptqPrev=ptq1->ptqPrev;
	ptq1->ptqPrev=ptq;
	ptq->ptqPrev->ptqNext=ptq;
	
//dfDelayedActionTime+=GetInternalRunTime();

	return TRUE;
}

/* EvalInhibitDelayedAction (Pseudo predicate)

Description:
	Inhibit a previously queued action so that it doesn't execute in
	any of our offspring states.
Notes:
	The inhibited action is any action with a matching tag and absolute
	completion time.  An inhibit is added to the queue like any other
	delayed action... it is flagged by a null formula.
Call:
	(inhibit-delayed-action delta tag)
Where
	delta is an incremental time.
	tag is a dummy formula, used to name the action to be inhibited
*/

BOOL EvalInhibitDelayedAction
(
	CELLP pcFormula,					// inhibit-delayed-action formula
	LINEARPLANP plpLinearPlan,			// parent world
	BINDINGP pbBindings
)
{
	CELLP pcForm;
	CELLP pcTag;
	CELLP pcArg;
	double dfDelta;
	TQP ptq,ptq1;

	if(!tqHead.ptqNext||!tqHead.ptqPrev)
	{
		ErrorMessage("delayed-action:  Call (enable concurrent-planning) first.\n");
		return FALSE;
	}

	// fetch the incremental time

	pcArg=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pcArg,plpLinearPlan,pbBindings,&dfDelta))
		TermError("delayed-action",pcFormula,pbBindings);
	if(dfDelta<=0.0)
	{
		Message("inhibit-delayed-action:  Warning, delta time is non-positive.\n");
		PrintFormula(stderr,pcFormula,0);
		PrintFormula(pfTraceStream,pcFormula,0);
	}

	// fetch the tag
	
	pcArg=pcArg->pcNext;
	if(!pcArg)
		TermError("inhibit-delayed-action",pcFormula,pbBindings);
	pcTag=CopyCell(pcArg);
	
	// fetch the formula
	
	pcForm=NULL;						// mark this as an inhibit
	
	// fill in queue entry

	ptq=MemAlloc(sizeof(TQ));
//	ptq->dfTime=plpLinearPlan->dfDuration+dfDelta;
	ptq->dfTime=plpSuccessorWorld->dfTime+dfDelta;
	ptq->pcTag=pcTag;
	ptq->pcFormula=pcForm;
	ptq->pbBindings=CopyBindings(pbBindings);
//	ptq->plpProgenitor=plpLinearPlan;
	ptq->plpProgenitor=plpSuccessorWorld;
//	if(plpSuccessorWorld->nWorldNumber==43)
//		fprintf(stderr,"debug\n");

//	CommandPrintf(pfTraceStream,"inibit-delayed-action world %04d time %f\n",
//		ptq->plpProgenitor->nWorldNumber,ptq->dfTime);

	// insert entry into queue in time order

	for(ptq1=tqHead.ptqNext;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
	{
		if(ptq1->dfTime>ptq->dfTime)
			break;
	}
	ptq->ptqNext=ptq1;
	ptq->ptqPrev=ptq1->ptqPrev;
	ptq1->ptqPrev=ptq;
	ptq->ptqPrev->ptqNext=ptq;
	
	return TRUE;
}

/* EvalWaitForNextEvent (Pseudo predicate)

Description:
	Wait for the next available actions and insert them into the current
	operator.  Associate the incremental time with the operator.
Notes:
	We don't release (delete) any queue entries here, because we have no
	idea if they will be needed again.
Call:
	(wait-for-next-event)
*/

//BOOL EvalWaitForNextEventOld
//(
//	CELLP pcFormula,
//	LINEARPLANP plpLinearPlan,
//	BINDINGP pbBindings
//)
//{
//	CELLP pcTag;						// tag formula
//	LINEARPLANP plp;
//	TQP ptq1;
//	double dfQueueTime;
//
//	// handle empty queue
//
//	if(tqHead.ptqNext==&tqHead)
//		return TRUE;
//
//	// advance the current time
//	
//	for(ptq1=tqHead.ptqNext;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
//	{
//		if(ptq1->dfTime>plpLinearPlan->dfTime)
//		{
//			for(plp=plpLinearPlan;plp;plp=plp->plpParent)
//			{
//				if(plp==ptq1->plpProgenitor||!ptq1->plpProgenitor)
//					goto end;
//			}
//		}			
//	}
//end:
//	if(ptq1==&tqHead)
//		return TRUE;
//
//	// modify the current world's duration
//
//	dfQueueTime=ptq1->dfTime;
//	plpSuccessorWorld->dfTime=dfQueueTime;
//
//	// evaluate all parent queue entries with same completion time
//	// append each action tag to the successor world's action name
//	
//	pcTag=(CELLP)&plpSuccessorWorld->pcActionName->pfForm->pcArgs;
//	for(;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
//	{
//		if(ptq1->dfTime!=dfQueueTime)
//			break;
//		for(plp=plpLinearPlan;plp;plp=plp->plpParent)
//		{
//			if(plp==ptq1->plpProgenitor||!ptq1->plpProgenitor)
//			{
//				(*ptq1->pcFormula->pfForm->paAction->pfEval)
//					(ptq1->pcFormula,plpLinearPlan,ptq1->pbBindings);
//
//				// append this delayed-action to the action tag
//				
//				pcTag=pcTag->pcNext=CopyFormula(ptq1->pcTag);	// this is rather inefficient!
//				if(pcTag->pfForm->pcArgs)
//				{
//					pcTag->pfForm->pcArgs=ComputeTerms(pcTag->pfForm->pcArgs,plpLinearPlan,ptq1->pbBindings);
//					if(!pcTag->pfForm->pcArgs)
//						TermError("eval-wait-for-next-event",pcFormula,pbBindings);
//				}
//				break;
//			}
//		}
//	}
//
//	return TRUE;
//}

// this version supports inhibits

//double dfWaitEventTime;
//int nWaitEvent;

BOOL EvalWaitForNextEvent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcTag;						// tag formula
	LINEARPLANP plp;
	TQP ptq1;
	TQLINKP ptqlHead,ptqlEnd;			// time queue list
	TQLINKP ptql0,ptql1,ptql2;			// time queue list pointers
	double dfQueueTime;

	if(!bConcurrentPlanning)
	{
		ErrorMessage("wait-for-next-event:  Call (enable concurrent-planning) first.\n");
		return TRUE;
	}

	// handle empty queue

	if(tqHead.ptqNext==&tqHead)
		return TRUE;

//dfWaitEventTime-=GetInternalRunTime();
//nWaitEvent+=1;

	// keep searching until we find something or run out of entries
	
	for(ptq1=tqHead.ptqNext;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
	{
		// find the next group of ancestor entries
	
		for(;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
		{
			if(ptq1->dfTime>plpLinearPlan->dfTime)
			{
				for(plp=plpLinearPlan;plp;plp=plp->plpParent)
				{
					if(plp==ptq1->plpProgenitor||!ptq1->plpProgenitor)
						goto end;
				}
			}			
		}
end:
		if(ptq1==&tqHead)
		{
//dfWaitEventTime+=GetInternalRunTime();

			return TRUE;
		}
		dfQueueTime=ptq1->dfTime;
		
		// find all parent queue entries with the same completion time
		// link them into a list, instantiate the tags
		
		ptqlHead=NULL;					// initialize list
		ptqlEnd=(TQLINKP)&ptqlHead;

		for(;ptq1!=&tqHead;ptq1=ptq1->ptqNext)
		{
			if(ptq1->dfTime!=dfQueueTime)
				break;
			for(plp=plpLinearPlan;plp;plp=plp->plpParent)
			{
				if(plp==ptq1->plpProgenitor||!ptq1->plpProgenitor)
				{
					ptqlEnd=ptqlEnd->ptqlNext=(TQLINKP)MemAlloc(sizeof(TQLINK));
					ptqlEnd->pcTag=CopyFormula(ptq1->pcTag);	// this is rather inefficient!
					if(ptqlEnd->pcTag->pfForm->pcArgs)
					{
						ptqlEnd->pcTag->pfForm->pcArgs=
							ComputeTerms(ptqlEnd->pcTag->pfForm->pcArgs,plpLinearPlan,ptq1->pbBindings);
						if(!ptqlEnd->pcTag->pfForm->pcArgs)
							TermError("eval-wait-for-next-event",pcFormula,pbBindings);
					}
					ptqlEnd->ptqDelayedAction=ptq1;
					break;
				}
			}
		}
		ptqlEnd->ptqlNext=NULL;

		// match any inhibits against their corresponding delayed-actions
		// and delete them.
		// it's an eror if an inhibit has no match.
		
		for(ptql1=ptqlHead;ptql1;ptql1=ptql1->ptqlNext)
		{
			if(!ptql1->ptqDelayedAction->pcFormula)	// if this is an inhibit
			{
				ptql1->ptqDelayedAction=NULL;	// flag this entry
				for(ptql2=ptqlHead;ptql2;ptql2=ptql2->ptqlNext)
				{
					if(FormulaEqQ(ptql1->pcTag,ptql2->pcTag))	// if we have a match
					{
						ptql2->ptqDelayedAction=NULL;	// flag this entry
						break;
					}
				}
				if(!ptql2||ptql2==ptql1)
					Message("Inibit does not match any delayed action\n");
			}
		}

		// remove all inhibits and flagged actions

		ptql0=(TQLINKP)&ptqlHead;
		for(ptql1=ptqlHead;ptql1;ptql1=ptql2)
		{
			ptql2=ptql1->ptqlNext;
			if(!ptql1->ptqDelayedAction)	// if entry flagged
				ptql0->ptqlNext=ptql2;	// delete entry
			else
				ptql0=ptql1;
		}
		if(!ptqlHead)					// no entries remain
			continue;

		// modify the current world's duration

		plpSuccessorWorld->dfTime=dfQueueTime;

		// evaluate all parent queue entries with same completion time
		// append each action tag to the successor world's action name
	
		pcTag=(CELLP)&plpSuccessorWorld->pcActionName->pfForm->pcArgs;
		for(ptql1=ptqlHead;ptql1;ptql1=ptql1->ptqlNext)
		{
			(*ptql1->ptqDelayedAction->pcFormula->pfForm->paAction->pfEval)
				(ptql1->ptqDelayedAction->pcFormula,plpLinearPlan,ptql1->ptqDelayedAction->pbBindings);
			pcTag=pcTag->pcNext=ptql1->pcTag;
		}
		if(!pcTag)
			Message("No tag on queued action\n");
		break;
	}

//dfWaitEventTime+=GetInternalRunTime();

	return TRUE;
}

/* MarkTimeQueue

Description:
	A time queue element only survives if its progenitor world has been
	marked before garbage collection.
Note:
	MarkTimeQueue should only be called after the open, closed and parent
	plan lists have been marked!
*/

void MarkTimeQueue(void)
{
	TQP ptq;

	if(!tqHead.ptqNext)					// ignore uninitialized queue
		return;

	// unlink any entries that have lost their progenitors

	for(ptq=tqHead.ptqNext;ptq!=&tqHead;ptq=ptq->ptqNext)
	{
		if(ptq->plpProgenitor&&!ZoneMarkedQ(ptq->plpProgenitor))
		{
			ptq->ptqPrev->ptqNext=ptq->ptqNext;
			ptq->ptqNext->ptqPrev=ptq->ptqPrev;
		}
	}

	// mark remaining entries

	for(ptq=tqHead.ptqNext;ptq!=&tqHead;ptq=ptq->ptqNext)
	{
		ZoneMark(ptq);
		MarkFormula(ptq->pcTag);
		MarkFormula(ptq->pcFormula);
		MarkBindings(ptq->pbBindings);
	}
}

/* CopyBindings

Description:
	Copy a list of bindings.
*/

BINDINGP CopyBindings
(
	BINDINGP pbBindings
)
{
	BINDINGP pb,pb1,pb2;

	pb1=NULL;
	pb2=(BINDINGP)&pb1;
	
	for(pb=pbBindings;pb;pb=pb->pbNext)
	{
		pb2=pb2->pbNext=(BINDINGP)MemAlloc(sizeof(BINDING));
		pb2->pcVal=CopyFormula(pb->pcVal);
		pb2->pcVar=CopyFormula(pb->pcVar);
	}
	pb2->pbNext=NULL;	
	return pb1;
}

/* DumpQueue

Description:
	Display the current state of the time queue.
*/

void DumpQueue(void)
{
	TQP ptq;
	BINDINGP pb;

	if(!tqHead.ptqNext)					// ignore uninitialized queue
		return;
	
	CommandPuts("\nCurrent State of Time Queue\n\n",pfTraceStream);
	for(ptq=tqHead.ptqNext;ptq!=&tqHead;ptq=ptq->ptqNext)
	{
		CommandPrintf(pfTraceStream,"Time %f Progenitor world %d\n",
			ptq->dfTime,ptq->plpProgenitor?ptq->plpProgenitor->nWorldNumber:0);
		for(pb=ptq->pbBindings;pb;pb=pb->pbNext)
		{
			CommandPuts("(binding ",pfTraceStream);
			PrintTerm(pfTraceStream,pb->pcVar,0,0);
			CommandPuts(" ",pfTraceStream);
			PrintTerm(pfTraceStream,pb->pcVal,0,0);
			CommandPuts(")\n",pfTraceStream);
		}
		if(!ptq->pcFormula)
			CommandPrintf(pfTraceStream,"Inhibit ");
		PrintFormula(pfTraceStream,ptq->pcTag,0);
		if(ptq->pcFormula)
			PrintFormula(pfTraceStream,ptq->pcFormula,0);
	}
//	CommandPuts("\n",pfTraceStream);
}

/* DumpQueueEvents

Description:
	Display the entries in the time queue, which are germaine to the current
	world and its successors.
*/

void DumpQueueEvents
(
	LINEARPLANP plpLinearPlan
)
{
	TQP ptq;  
	LINEARPLANP plp;
	
	if(!tqHead.ptqNext)					// ignore uninitialized queue
		return;

	if(tqHead.ptqNext==&tqHead)
		return;

	//print out all events that come after the current world and will be utilized
	//in the future of this world. 

	CommandPuts("\nQueued events reachable from this world\n",pfTraceStream);
	for(ptq=tqHead.ptqNext;ptq!=&tqHead;ptq=ptq->ptqNext)
	{
		if(ptq->dfTime>plpLinearPlan->dfTime)
		{
			for(plp=plpLinearPlan;plp;plp=plp->plpParent)
			{
				if(plp==ptq->plpProgenitor||!ptq->plpProgenitor)
				{
					CommandPrintf(pfTraceStream,"\nTime %f Progenitor world %d %s ",
						ptq->dfTime,ptq->plpProgenitor?ptq->plpProgenitor->nWorldNumber:0,
						ptq->pcFormula?"Event":"Inhibit");
					PrintFormula(pfTraceStream,Instantiate(ptq->pcTag,ptq->pbBindings),0);
					if(ptq->pcFormula)
					{
						CommandPrintf(pfTraceStream,"Action:\n");
						PrintFormula(pfTraceStream,Instantiate(ptq->pcFormula,ptq->pbBindings),0);
					}
					break;
				}
			}
		}
	}
}

/* EvalReachableEvent

Description:
	Determine if there are any entries in the time queue reachable from the current
	world.
*/

BOOL EvalReachableEvent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	TQP ptq;  
	LINEARPLANP plp;
	
	if(!tqHead.ptqNext)					// ignore uninitialized queue
		return FALSE;

//	if(tqHead.ptqNext==&tqHead)
//		return FALSE;

	for(ptq=tqHead.ptqNext;ptq!=&tqHead;ptq=ptq->ptqNext)
	{
		if(ptq->dfTime>plpLinearPlan->dfTime)
		{
			for(plp=plpLinearPlan;plp;plp=plp->plpParent)
			{
				if(plp==ptq->plpProgenitor||!ptq->plpProgenitor)
					return TRUE;
			}
		}
	}
	return FALSE;
}

/* QueueDelete

Description:
	Unlink a queue entry (its progenitor has been discarded or pruned).
Notes:
	Disused.
*/

void QueueDelete
(
	LINEARPLANP plp
)
{
	TQP ptq;

	if(!tqHead.ptqNext)					// ignore uninitialized queue
		return;
	for(ptq=tqHead.ptqNext;ptq!=&tqHead;ptq=ptq->ptqNext)
	{
		if(ptq->plpProgenitor==plp)
		{
			ptq->ptqPrev->ptqNext=ptq->ptqNext;
			ptq->ptqNext->ptqPrev=ptq->ptqPrev;
			return;
		}
	}
}

/* ComputeCurrentTime

Description:
	Compute the current-time function.
	Current-time is the time of the successor world.
*/

CELLP ComputeCurrentTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeCurrentTime",FALSE);
	if(!plpSuccessorWorld)
		ErrorMessage("Current-time cannot be used in this context\n");
	pc=MakeFloatForm(plpSuccessorWorld->dfTime);
	EXIT("ComputeCurrentTime");
	return pc;
}

/* Instantiate

Description:
	Substitute values for all variables in a formula.
Notes:
	To avoid writing a lot of code, we do this the brute force way.
*/

CELLP Instantiate
(
	CELLP pcFormula,					// formula to instantiate
	BINDINGP pbBindings					// bindings list
)
{
	CELLP pc;

	pc=CopyFormula(pcFormula);
	return Instant(pc,pbBindings);
}

static CELLP Instant
(
	CELLP pcFormula,					// formula to instantiate
	BINDINGP pbBindings					// bindings list
)
{
	CELLP pc;
	FORMULAP pf;

	pf=pcFormula->pfForm;
	if(pf->pcVars)
		pbBindings=ExtendBindings(pf->pcVars,pf->pcVars,pbBindings);
	if(pf->pcGenLit)
	{
		for(pc=pf->pcGenLit;pc;pc=pc->pcNext)
		{
			if(VarQ(pc))
				pc->pfForm=LookupVar(pc,pbBindings)->pfForm;
			else
				Instant(pc,pbBindings);
		}
	}
	if(pf->pcArgs)
	{
		for(pc=pf->pcArgs;pc;pc=pc->pcNext)
		{
			if(VarQ(pc))
				pc->pfForm=LookupVar(pc,pbBindings)->pfForm;
			else
				Instant(pc,pbBindings);
		}
	}
	return pcFormula;
}

