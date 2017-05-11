/* save.c

Copyright C, 1996 - 2001  F. Bacchus

Description:
	This module contains various routines for garbage collection and
	copying data between zones.

	Normal operating procedure is to run in the permanent memory zone and
	to garbage collect at the start of planning.
	This simplifies keeping track of data... especially data that has been
	created by the user.

Procedures:
	1.	When the user calls reset-all we clear all dynamic memory and
	clear all global pointers.
	2.	At the start of planning, we mark the list of domains and all of
	the datastructures that define the current domain.  We discard everything
	else.
	3.  During planning, we switch to the scratch zone while manipulating
	formulas and copy the results to the permanent zone.
	4.  For debugging purposes, at the end of planning, we remark the domain
	information, and mark the current world list.  If all has gone well, there
	should be no memory discarded by garbage collection.
*/

#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "btree.h"
#include "crchash.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "hash.h"
#include "iface.h"
#include "list.h"
#include "makeform.h"
#include "oper.h"
#include "plan.h"
#include "queue.h"
#include "save.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "user.h"
#include "util.h"
#include "world.h"
#include "zone.h"

/* local function prototypes */

static void MarkStringTable
(
	void *pvBlock						/* pointer to hash block */
);
static void MarkSymbolTable
(
	void *pvBlock						/* pointer to hash block */
);

/* MarkDomainData

Description:
	Mark the following (lists of) data structures:

		the domain list -- (list of defined domains)
		all defined symbols -- (lists and formulas in the hash table)
		all domain formulas -- asiWorldSymbols
		all domain operators -- poOperators
		the initial world -- plpInitialWorld
		the goal world -- plpGoalWorld
*/

void MarkDomainData(void)
{
	OPERATORP po;
	MACROOPERATORP pmo;
	ELIDEDOPERATORP peo;
	int i;

	ENTER("SaveDomain",FALSE);

	/* mark the domain list */

	MarkDomains();

	/* mark all operators */

	for(po=poOperators;po;po=po->poNext)
		MarkOperator(po);

	for(pmo=pmoMacroOperators;pmo;pmo=pmo->pmoNext)
		MarkMacroOperator(pmo);

	for(peo=peoElidedOperators;peo;peo=peo->peoNext)
		MarkElidedOperator(peo);

	/* mark domain formulas */

	for(i=0;i<nNumberOfWorldSymbols;i++)
		MarkSymbolInfo(asiWorldSymbols+i);

	/* mark the symbol and string tables */

	ZoneMark(htSymbolTable.pavTable);
	HashScan(&htSymbolTable,MarkSymbolTable);
	ZoneMark(htStringTable.pavTable);
	HashScan(&htStringTable,MarkStringTable);

	/* mark the initial plan */

	MarkLinearPlan(plpInitialWorld);

	/* mark the control formula */

	MarkFormula(pcTLForm);

	/* mark the goal plan and goal formula */

	MarkLinearPlan(GetGoalWorld(FALSE));
	MarkFormula(pcGoalFormula);

	/* mark the plan list */

	MarkPlanList(plpInitialWorld);
	if(plpSuffixTail)
		MarkParentPlans(plpSuffixTail);

	/* mark the initial facts and initialization sequence */

	MarkFormulaList(pcInitialFacts);
	MarkFormulaList(pcInitializationSequence);

	/* mark the cost, priority and print-world formulas */

	MarkFormula(pcHeuristicFn);
	MarkFormula(pcPriorityFn);
	MarkFormula(pcPrintWorld);
	MarkFormula(pcGoalAddendum);
	MarkFormula(pcSSForm);

	/* mark statistics and plan names */

//	if(psStatisticsFile)
//		ZoneMark(psStatisticsFile);
//	if(psPlanName)
//		ZoneMark(psPlanName);

	/* mark global variables */

	MarkBindings(pbGlobalVariables);
	MarkFormulaList(pcSearchGlobalInit);
	MarkFormula(pcTrue);
	MarkFormula(pcFalse);

	/* mark the library list */

	MarkLibraries();

	/* mark the command we're executing */

	MarkCurrentCommand();

	/* mark the btree stack */

	MarkBTreeStack();

	/* mark the CRC hash table */

	MarkCRCHash();

	EXIT("MarkDomain");
}

/* MarkPlanList

Description:
	Given a list of plans, mark them and all their predecessors.
*/

void MarkPlanList
(
	LINEARPLANP plpList				/* list of plans */
)
{
	LINEARPLANP plp;

	ENTER("MarkPlanData",TRUE);
	for(plp=plpList;plp;plp=plp->plpNext)
		MarkLinearPlan(plp);
	EXIT("MarkPlanData");
}

/* MarkParentPlans

Description:
	Given a plan, mark all of its ancestors.
*/

void MarkParentPlans
(
	LINEARPLANP plpPlan				/* (current) plan */
)
{
	LINEARPLANP plp;

	ENTER("MarkParentPlans",TRUE);
	for(plp=plpPlan;plp;plp=plp->plpParent)
		MarkLinearPlan(plp);
	EXIT("MarkParentPlans");
}

/* MarkBindings

Description:
	Given a list of bindings, mark them.
*/

void MarkBindings
(
	BINDINGP pbBindings				/* list of bindings */
)
{
	BINDINGP pb;

	ENTER("MarkBindings",TRUE);
	for(pb=pbBindings;pb;pb=pb->pbNext)
	{
		if(ZoneMarkedQ(pb))
			break;
		ZoneMark(pb);
		if(pb->pgcContext)
		{
			ZoneMark(pb->pgcContext);
			if(pb->pgcContext->pfMark)
				(pb->pgcContext->pfMark)(pb->pgcContext);
		}
		MarkFormula(pb->pcVar);
		MarkFormula(pb->pcVal);
	}
	EXIT("MarkBindings");
}

/* CollectGarbage

Description:
	A debug shell routine for ZoneCollect...  discard all unmarked memory
*/

void CollectGarbage(void)
{
#ifdef _DEBUG
	ZoneStatistics(stderr,&zPermanent);
	CommandPrintf(stderr,"Collecting garbage\n");
#endif 
	ZoneCollect(&zPermanent);
#ifdef _DEBUG
	ZoneStatistics(stderr,&zPermanent);
#endif 
}

/* MarkSymbolTable

Description:
	Mark the contents of symbol table entry.
*/

static void MarkSymbolTable
(
	void *pvBlock						/* pointer to hash block */
)
{
	SYMBOLP ps;

	ps=(SYMBOLP)pvBlock;
	ZoneMark(ps->psName);
	if(ps->nType==ATOM_LISTP)
		MarkList(ps->uValue.plList);
	else if(ps->nType==ATOM_FORMULAP)
		MarkFormula(ps->uValue.pcFormula);
}

/* MarkStringTable

Description:
	Mark the contents of string table entry.
*/

static void MarkStringTable
(
	void *pvBlock						/* pointer to hash block */
)
{
	char **pps;

	pps=(char **)pvBlock;
	ZoneMark(*pps);
}

/* ZoneCopyDomainData

Description:
	ZoneCopy the following (lists of) data structures:

		the domain list -- (list of defined domains)
		all defined symbols -- (lists and formulas in the hash table)
		all domain formulas -- asiWorldSymbols
		all domain operators -- poOperators
		the initial plan -- plpInitialPlan
		the goal plan -- plpGoalPlan
*/

//void ZoneCopyDomainData(void)
//{
//	OPERATORP po;
//	int i;
//
//	ENTER("SaveDomain",FALSE);
//
//	/* ZoneCopy the domain list */
//
//	ZoneCopyDomains();
//
//	/* ZoneCopy all operators */
//
//	for(po=poOperators;po;po=po->poNext)
//		ZoneCopyOperator(po);
//
//	/* ZoneCopy domain formulas */
//
//	for(i=0;i<nNumberOfWorldSymbols;i++)
//		ZoneCopySymbolInfo(asiWorldSymbols+i);
//
//	/* ZoneCopy the symbol and string tables */
//
//	HashScan(psSymbolTable,nSymbolCount,sizeof(SYMBOL),ZoneCopySymbolTable);
//	HashScan(ppsStringTable,nStringCount,sizeof(char *),ZoneCopyStringTable);
//
//	/* ZoneCopy the initial plan */
//
//	ZoneCopyLinearPlan(plpInitialPlan);
//
//	/* ZoneCopy the control formula */
//
//	ZoneCopyFormula(pcTLForm);
//
//	/* ZoneCopy the goal plan and goal formula */
//
//	ZoneCopyLinearPlan(GetGoalPlan(FALSE));
//	ZoneCopyFormula(pcGoalFormula);
//
//	/* ZoneCopy the plan list */
//
//	ZoneCopyPlanList(plpInitialPlan);
//
//	/* ZoneCopy the initial facts and initialization sequence */
//
//	ZoneCopyFormulaList(pcInitialFacts);
//	ZoneCopyFormulaList(pcInitializationSequence);
//
//	/* ZoneCopy the cost, priority and print-world formulas */
//
//	ZoneCopyFormula(pcHeuristic);
//	ZoneCopyFormula(pcPriority);
//	ZoneCopyFormula(pcPrintWorld);
//	ZoneCopyFormula(pcGoalAddendum);
//	ZoneCopyFormula(pcSSForm);
//
//	/* ZoneCopy statistics and plan names */
//
////	if(psStatisticsFile)
////		ZoneZoneCopy(psStatisticsFile);
////	if(psPlanName)
////		ZoneZoneCopy(psPlanName);
//
//	/* ZoneCopy global variables */
//
//	ZoneCopyBindings(pbGlobalVariables);
//	ZoneCopyFormulaList(pcSearchGlobalInit);
//	ZoneCopyFormula(pcTrue);
//	ZoneCopyFormula(pcFalse);
//
//	/* ZoneCopy the library list */
//
//	ZoneCopyLibraries();
//
//	/* ZoneCopy the command we're executing */
//
//	ZoneCopyCurrentCommand();
//
//	EXIT("ZoneCopyDomain");
//}

/* ZoneRelocDomainData

Description:
	ZoneReloc the following (lists of) data structures:

		the domain list -- (list of defined domains)
		all defined symbols -- (lists and formulas in the hash table)
		all domain formulas -- asiWorldSymbols
		all domain operators -- poOperators
		the initial plan -- plpInitialPlan
		the goal plan -- plpGoalPlan
*/

//void ZoneRelocDomainData(void)
//{
//	OPERATORP po;
//	int i;
//
//	ENTER("SaveDomain",FALSE);
//
//	/* ZoneReloc the domain list */
//
//	ZoneRelocDomains();
//
//	/* ZoneReloc all operators */
//
//	for(po=poOperators;po;po=po->poNext)
//		ZoneRelocOperator(po);
//
//	/* ZoneReloc domain formulas */
//
//	for(i=0;i<nNumberOfWorldSymbols;i++)
//		ZoneRelocSymbolInfo(asiWorldSymbols+i);
//
//	/* ZoneReloc the symbol and string tables */
//
//	HashScan(psSymbolTable,nSymbolCount,sizeof(SYMBOL),ZoneRelocSymbolTable);
//	HashScan(ppsStringTable,nStringCount,sizeof(char *),ZoneRelocStringTable);
//
//	/* ZoneReloc the initial plan */
//
//	ZoneRelocLinearPlan(plpInitialPlan);
//
//	/* ZoneReloc the control formula */
//
//	ZoneRelocFormula(pcTLForm);
//
//	/* ZoneReloc the goal plan and goal formula */
//
//	ZoneRelocLinearPlan(GetGoalPlan(FALSE));
//	ZoneRelocFormula(pcGoalFormula);
//
//	/* ZoneReloc the plan list */
//
//	ZoneRelocPlanList(plpInitialPlan);
//
//	/* ZoneReloc the initial facts and initialization sequence */
//
//	ZoneRelocFormulaList(pcInitialFacts);
//	ZoneRelocFormulaList(pcInitializationSequence);
//
//	/* ZoneReloc the cost, priority and print-world formulas */
//
//	ZoneRelocFormula(pcHeuristic);
//	ZoneRelocFormula(pcPriority);
//	ZoneRelocFormula(pcPrintWorld);
//	ZoneRelocFormula(pcGoalAddendum);
//	ZoneRelocFormula(pcSSForm);
//
//	/* ZoneReloc statistics and plan names */
//
////	if(psStatisticsFile)
////		ZoneZoneReloc(psStatisticsFile);
////	if(psPlanName)
////		ZoneZoneReloc(psPlanName);
//
//	/* ZoneReloc global variables */
//
//	ZoneRelocBindings(pbGlobalVariables);
//	ZoneRelocFormulaList(pcSearchGlobalInit);
//
//	/* ZoneReloc the library list */
//
//	ZoneRelocLibraries();
//
//	/* ZoneReloc the command we're executing */
//
//	ZoneRelocCurrentCommand();
//
//	EXIT("ZoneRelocDomain");
//}

/* ZoneCopyBindings

Description:
	Given a list of bindings, ZoneCopy them.
*/

//void ZoneCopyBindings
//(
//	BINDINGP pbBindings				/* list of bindings */
//)
//{
//	BINDINGP pb;
//
//	ENTER("ZoneCopyBindings",TRUE);
//	for(pb=pbBindings;pb;pb=pb->pbNext)
//	{
//		ZoneZoneCopy(pb);
//		ZoneCopyFormula(pb->pcVar);
//		ZoneCopyFormula(pb->pcVal);
//	}
//	EXIT("ZoneCopyBindings");
//}

/* ZoneRelocBindings

Description:
	Given a list of bindings, ZoneReloc them.
*/

//void ZoneRelocBindings
//(
//	BINDINGP pbBindings				/* list of bindings */
//)
//{
//	BINDINGP pb,pb1;
//
//	ENTER("ZoneRelocBindings",TRUE);
//	for(pb=pbBindings;pb;pb=pb1)
//	{
//		pb1=pb->pbNext;
//		ZoneReloc(&pb->pbNext);
//		ZoneReloc(&pb);
//		ZoneReloc(&pb->pcVar);
//		ZoneRelocFormula(pb->pcVar);
//		ZoneReloc(&pb->pcVal);
//		ZoneRelocFormula(pb->pcVal);
//	}
//	EXIT("ZoneRelocBindings");
//}
