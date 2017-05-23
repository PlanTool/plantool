/* taxi.c

Description:
	Concurrent random problem generators for taxi world.
 */

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif /* WIN32 */

#include "../tlplan.h"
#include "../compute.h"
#include "../eval.h"
#include "../formula.h"
#include "../iface.h"
#include "../list.h"
#include "../makeform.h"
#include "../tl_tab.h"
#include "../tlparse.h"
#include "../util.h"

/* local function prototypes */

static CELLP MakeStaticLiterals(void);
static CELLP MakeDynamicLiterals(void);
static CELLP MakeFareNames
(
	int nCount
);
static CELLP MakeTaxiNames
(
	int nCount
);
static CELLP MakeAvailableLiterals
(
	CELLP pcTaxis						/* list of taxi names */
);
static CELLP MakeAtLiterals
(
	CELLP pcNames						/* list of names */
);
static CELLP MakeFareLiterals
(
	CELLP pcFares						/* list of fare names */
);
static CELLP MakeDestLiterals
(
	CELLP pcFares						/* list of fare names */
);
static CELLP MakeInitializationDoneLiteral(void);
static int MSRandx(void);

/* local data */

static int nSeed;						/* plan number */
static int nTaxis;						/* number of taxis */
static int nFares;						/* number of fares */
static int nTaxiDelay;					/* running taxi delay */
static int nFareDelay;					/* running fare delay */

/* Static Random Taxi World ----------------------------------------------------

Functions for generating a random taxi world initial state.

You can execute (set-initial-world (random-taxi-world <seed> 5 30))
to get an random initial world configuration with 5 taxis and 30 fares.

*/

/* StaticRandomTaxiWorld (Function)

Description:
	Generate a initial state information to simulate a real time run 
	of taxi world.
Call:
	(set-initial-world (random-taxi-world ?seed ?taxis ?fares))
*/

DECLSPECX CELLP StaticRandomTaxiWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int n;

	/* get the seed */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&n))
		return NULL;					/* message already printed */
	if(n!=-1)							/* default the seed if n==-1 */
	{
		nSeed=n;
		if(nSeed<0)
			nSeed=0;
		else if(nSeed>32767)
			nSeed=32767;
	}

	/* get the number of taxis */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nTaxis))
		return NULL;					/* message already printed */
	if(nTaxis<1||nTaxis>999)
	{
		ErrorMessage("random-taxi-world:  Number of taxis must be between 1 and 999.\n");
		return NULL;
	}

	/* get the number of fares */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nFares))
		return NULL;					/* message already printed */
	if(nFares<1||nFares>26*26)
	{
		ErrorMessage("random-taxi-world:  Number of fares must be between 1 and %d.\n",26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Taxis, %d Fares, Plan %d",nTaxis,nFares,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return MakeStaticLiterals();
}

/* MakeStaticLiterals (Local Function)

Description:
	Generate a list of literals representing the initial state of a static random problem.
*/

static CELLP MakeStaticLiterals(void)
{
	CELLP pcStart,pcEnd;
	CELLP pcFares;
	CELLP pcTaxis;

	pcFares=MakeFareNames(nFares);
	pcTaxis=MakeTaxiNames(nTaxis);

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd->pcNext=MakeAvailableLiterals(pcTaxis);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeAtLiterals(pcTaxis);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeFareLiterals(pcFares);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeAtLiterals(pcFares);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeDestLiterals(pcFares);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeInitializationDoneLiteral();
//	while(pcEnd->pcNext)
//		pcEnd=pcEnd->pcNext;

	return pcStart;
}

/* MakeFareNames (Local Function)

Description:
	Generate names for fares (we use 2 letter initials).
*/

static CELLP MakeFareNames
(
	int nCount
)
{
	CELLP pcStart,pcEnd;
	char buffer[8];
	LITVAL lv;
	int i;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	
	lv.psString=buffer;
	for(i=0;i<nCount;i++)
	{
		sprintf(buffer,"%c%c",'a'+i/26,'a'+i%26);
		pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
	}
	return pcStart;
}

/* MakeTaxiNames (Local Function)

Description:
	Generate taxi names (we use numbers).
*/

static CELLP MakeTaxiNames
(
	int nCount
)
{
	CELLP pcStart,pcEnd;
	char buffer[8];
	LITVAL lv;
	int i;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	
	lv.psString=buffer;
	for(i=0;i<nCount;i++)
	{
		sprintf(buffer,"taxi%d",i);
		pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
	}
	return pcStart;
}

/* MakeAvailableLiterals (Local Function)

Description:
	Generate a list of available predicates.
*/

static CELLP MakeAvailableLiterals
(
	CELLP pcTaxis						/* list of taxi names */
)
{
	CELLP pcStart,pcEnd;
	CELLP pc;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate an available literal for each taxi name */

	for(pc=pcTaxis;pc;pc=pc->pcNext)
	{
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"available"),
			CopyCell(pc));
	}
	return pcStart;
}

/* MakeAtLiterals (Local Function)

Description:
	Generate a list of random at predicates.
*/

static CELLP MakeAtLiterals
(
	CELLP pcNames						/* list of names */
)
{
	CELLP pcStart,pcEnd;
	CELLP pc,pc1;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate a random at literal for each name */

	for(pc=pcNames;pc;pc=pc->pcNext)
	{
		pc1=CopyCell(pc);
		pc1->pcNext=MakeIntegerForm(1+(MSRandx()%100));
		pc1->pcNext->pcNext=MakeIntegerForm(1+(MSRandx()%100));
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"at"),
			pc1);
	}
	return pcStart;
}

/* MakeFareLiterals (Local Function)

Description:
	Generate a list of fare predicates.
*/

static CELLP MakeFareLiterals
(
	CELLP pcFares						/* list of fare names */
)
{
	CELLP pcStart,pcEnd;
	CELLP pc;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate a fare literal for each fare name */

	for(pc=pcFares;pc;pc=pc->pcNext)
	{
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"fare"),
			CopyCell(pc));
	}
	return pcStart;
}

/* MakeDestLiterals (Local Function)

Description:
	Generate a list of random dest predicates.
*/

static CELLP MakeDestLiterals
(
	CELLP pcFares						/* list of fare names */
)
{
	CELLP pcStart,pcEnd;
	CELLP pc,pc1;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate a random dest literal for each name */

	for(pc=pcFares;pc;pc=pc->pcNext)
	{
		pc1=CopyCell(pc);
		pc1->pcNext=MakeIntegerForm(1+(MSRandx()%100));
		pc1->pcNext->pcNext=MakeIntegerForm(1+(MSRandx()%100));
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"dest"),
			pc1);
	}
	return pcStart;
}

/* MakeInitializationDoneLiteral (Local Function)

Description:
	Generate a single initialization done predicate.
*/

static CELLP MakeInitializationDoneLiteral(void)
{
	CELLP pc;

	pc=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"initialization-done"),NULL);
	return pc;
}

/* Dynamic Random Taxi World ---------------------------------------------------

Description:
	Generate a formula that will simulate a real time run of taxi world.
	The initial world is empty, and taxis "arrive-for-work" and fares
	"call-for-pickup" at random intervals, generating work for the planner.
Note:
	Unlike most other random world generators, this generator is a
	pseudo-predicate that is called from the command line to set up an
	initialization sequence.  It calls set-initial-world itself.
Call:
	(dynamic-random-taxi-world ?seed ?taxis ?fares)
*/

DECLSPECX BOOL DynamicRandomTaxiWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int n;

	/* get the seed */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&n))
		return NULL;					/* message already printed */
	if(n!=-1)							/* default the seed if n==-1 */
	{
		nSeed=n;
		if(nSeed<0)
			nSeed=0;
		else if(nSeed>32767)
			nSeed=32767;
	}

	/* get the number of taxis */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nTaxis))
		return FALSE;					/* message already printed */
	if(nTaxis<1||nTaxis>999)
	{
		ErrorMessage("random-taxi-world:  Number of taxis must be between 1 and 999.\n");
		return FALSE;
	}

	/* get the number of fares */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nFares))
		return FALSE;					/* message already printed */
	if(nFares<1||nFares>26*26)
	{
		ErrorMessage("random-taxi-world:  Number of fares must be between 1 and %d.\n",26*26);
		return FALSE;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Taxis, %d Fares, Plan %d",nTaxis,nFares,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}

	/* generate the initialization formula and execute it. */

	pc=MakeDynamicLiterals();
	(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	return TRUE;
}

/* MakeDynamicLiterals (Local Function)

Description:
	Generate a list of literals representing the initial state of a dynamic random problem.
*/

static CELLP MakeDynamicLiterals(void)
{
	CELLP pcStart1,pcEnd1;
	CELLP pcStart2,pcEnd2;
	CELLP pcFares;
	CELLP pcTaxis;
	CELLP pc,pc1,pc2,pc3;
	int n1,n2,n3,n4;

	nTaxiDelay=0;
	nFareDelay=0;
	pcFares=MakeFareNames(nFares);
	pcTaxis=MakeTaxiNames(nTaxis);

	pcStart1=NULL;
	pcEnd1=(CELLP)&pcStart1;

	// generate the taxi delayed actions

	for(pc=pcTaxis;pc;pc=pc->pcNext)
	{
		pcStart2=NULL;
		pcEnd2=(CELLP)&pcStart2;
		pcEnd2=pcEnd2->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"available"),
			CopyCell(pc));
		pc1=CopyCell(pc);
		n1=1+(MSRandx()%100);
		n2=1+(MSRandx()%100);
		pc1->pcNext=MakeIntegerForm(n1);
		pc1->pcNext->pcNext=MakeIntegerForm(n2);
		pcEnd2=pcEnd2->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"at"),
			pc1);
		pc3=MakeAddForm(FALSE,pcStart2);

		pcStart2=NULL;
		pcEnd2=(CELLP)&pcStart2;
		pcEnd2=pcEnd2->pcNext=CopyCell(pc);
		pcEnd2=pcEnd2->pcNext=MakeIntegerForm(n1);
		pcEnd2=pcEnd2->pcNext=MakeIntegerForm(n2);
		pc2=CompileDummyForm("arrive-for-work",pcStart2);

		nTaxiDelay+=1+(MSRandx()%100);
		pc1=MakeIntegerForm(nTaxiDelay);
		pcEnd1=pcEnd1->pcNext=MakeGlobalDelayedActionForm(FALSE,pc1,pc2,pc3);
	}

	// generate the fare delayed actions

	for(pc=pcFares;pc;pc=pc->pcNext)
	{
		pcStart2=NULL;
		pcEnd2=(CELLP)&pcStart2;
		pcEnd2=pcEnd2->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"fare"),
			CopyCell(pc));
		pc1=CopyCell(pc);
		n1=1+(MSRandx()%100);
		n2=1+(MSRandx()%100);
		pc1->pcNext=MakeIntegerForm(n1);
		pc1->pcNext->pcNext=MakeIntegerForm(n2);
		pcEnd2=pcEnd2->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"at"),
			pc1);
		pc1=CopyCell(pc);
		n3=1+(MSRandx()%100);
		n4=1+(MSRandx()%100);
		pc1->pcNext=MakeIntegerForm(n3);
		pc1->pcNext->pcNext=MakeIntegerForm(n4);
		pcEnd2=pcEnd2->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"dest"),
			pc1);
		pc3=MakeAddForm(FALSE,pcStart2);

		pcStart2=NULL;
		pcEnd2=(CELLP)&pcStart2;
		pcEnd2=pcEnd2->pcNext=CopyCell(pc);
		pcEnd2=pcEnd2->pcNext=MakeIntegerForm(n1);
		pcEnd2=pcEnd2->pcNext=MakeIntegerForm(n2);
		pcEnd2=pcEnd2->pcNext=MakeIntegerForm(n3);
		pcEnd2=pcEnd2->pcNext=MakeIntegerForm(n4);
		pc2=CompileDummyForm("call-for-pickup",pcStart2);

		nFareDelay+=1+(MSRandx()%100);
		pc1=MakeIntegerForm(nFareDelay);
		pcEnd1=pcEnd1->pcNext=MakeGlobalDelayedActionForm(FALSE,pc1,pc2,pc3);
	}

	// generate the initialization-done action

	pc=MakeInitializationDoneLiteral();
	pc3=MakeAddForm(FALSE,pc);
	pc2=CompileDummyForm("initialization-complete",NULL);
	pc1=MakeIntegerForm(MAX(nTaxiDelay,nFareDelay));
	pcEnd1=pcEnd1->pcNext=MakeGlobalDelayedActionForm(FALSE,pc1,pc2,pc3);

	pc=MakeAndForm(FALSE,pcStart1);
	pc1=MakeSetInitializationSequenceForm(FALSE,pc);
	pc1->pcNext=MakeSetInitialWorldForm(FALSE,NULL);
	pc=MakeAndForm(FALSE,pc1);

	return pc;
}

/* MSRandx

Description:
	Generate a random number.
	This algorithm is compatible with Microsoft's rand function.
*/

static int MSRandx(void)
{
	nSeed=nSeed*214013+2531011;			/* 0x00034DFD & 0x00269EC3 */
	return (nSeed>>16)&0x00007FFF;
}
