/* tyre.c

Description:
	(Non-random) tyre problem generator.

	We classify tyre problems, based on the number of subgoals (i.e. described
	predicates) in the goal world.  There are 4 tool goals, and each additional
	wheel generates 5 additional subgoals.
	Hence, for 
	       1 wheel we have up to 9 subgoals
           2 wheels we have up to 14 subgoals
           3                      19
           ...                    ...
           18                     94
Notes:
	The dope that wrote this domain didn't know the difference between wheel and
	a tire.  So sometimes a hub is a wheel and sometimes a tire is a wheel.
*/

#include <math.h>
#include <stdio.h>
//#include <stdlib.h>

#include "../tlplan.h"
//#include "../compute.h"
//#include "../eval.h"
#include "../formula.h"
#include "../list.h"
#include "../makeform.h"
#include "../tl_tab.h"
#include "../util.h"


/* local definitions */

#define MAX_WHEELS	18
#define MAX_HUBS	18

/* local function prototypes */

static CELLP InitialTools(void);
static CELLP GoalTools(void);
static CELLP NewWheels
(
	int nHubs							/* number of hubs */
);
static CELLP UsedWheels
(
	int nWheels							/* number of wheels */
);
static CELLP NewWheelGoals
(
	int nWheels							/* number of wheels */
);
static CELLP UsedWheelGoals
(
	int nWheels							/* number of wheels */
);

/* RandomTyreWorld

Description:
	Select an initial world state that allows numgoals different subgoals.
Call:
	(random-tyre-world <goals>)
*/

DECLSPECX CELLP RandomTyreWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nGoals;							/* number of goals */
	int nWheels;						/* number of different wheels required for nGoals */
	CELLP pcStart,pcEnd;				/* list head */
	CELLP pc;
	
	/* get the number of goals */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nGoals))
		return NULL;					/* message already printed */
	nWheels=MIN(MAX_WHEELS,MAX(1,(int)ceil((double)(nGoals-4)/5)));
	
	if(nWheels>MAX_WHEELS)
	{
		ErrorMessage("Too many goals: %d\n",nGoals);
		nWheels=MAX_WHEELS;
	}

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(pcEnd->pcNext=InitialTools();pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	for(pcEnd->pcNext=NewWheels(nWheels);pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	for(pcEnd->pcNext=UsedWheels(nWheels);pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	return pcStart;
}

/* RandomTyreGoal

Description:
	Select a subgoal list of size numgoals.
Call:
	(random-tyre-goal <goals>)
*/

DECLSPECX CELLP RandomTyreGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nGoals;
	int nWheels;						/* number of different wheels required for nGoals */
	CELLP pcStart,pcEnd;				/* list head */
	CELLP pc;
	
	/* get the number of goals */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nGoals))
		return NULL;					/* message already printed */
	nWheels=MIN(MAX_WHEELS,MAX(1,(int)ceil((double)(nGoals-4)/5)));
	
	if(nWheels>MAX_WHEELS)
	{
		ErrorMessage("Too many goals: %d\n",nGoals);
		nWheels=MAX_WHEELS;
	}

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(pcEnd->pcNext=GoalTools();pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	for(pcEnd->pcNext=NewWheelGoals(nWheels);pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	for(pcEnd->pcNext=UsedWheelGoals(nWheels);pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	return pcStart;
}

/* InitialTools

Description:
	Describe the initial tool state
*/

static CELLP InitialTools(void)
{
	CELLP pc1,pc2;
	LITVAL lv;
	CELLP pcStart,pcEnd;				/* list head */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* (container boot) */
	
	lv.psString="boot";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"container"),
		pc1);

	/* (object boot) (object jack) (object pump) (object wrench) (object nuts) */

	lv.psString="boot";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
		pc1);
	lv.psString="jack";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
		pc1);
	lv.psString="pump";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
		pc1);
	lv.psString="wrench";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
		pc1);
	lv.psString="nuts";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
		pc1);

	/* (nut nuts) */

	lv.psString="nuts";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"nut"),
		pc1);

	/* (unlocked boot) */

	lv.psString="boot";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"unlocked"),
		pc1);

	/* (in jack boot) (in pump boot) (in wrench boot) */

	lv.psString="jack";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	lv.psString="boot";
	pc2=MakeLiteralForm(ATOM_IDENT,lv);
	pc1->pcNext=pc2;
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
		pc1);
	lv.psString="pump";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	lv.psString="boot";
	pc2=MakeLiteralForm(ATOM_IDENT,lv);
	pc1->pcNext=pc2;
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
		pc1);
	lv.psString="wrench";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	lv.psString="boot";
	pc2=MakeLiteralForm(ATOM_IDENT,lv);
	pc1->pcNext=pc2;
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
		pc1);

	/* (closed boot) */

	lv.psString="boot";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"closed"),
		pc1);

	return pcStart;
}

/* GoalTools

Description:
	Describe the goal tool state
*/

static CELLP GoalTools(void)
{
	CELLP pc1,pc2;
	LITVAL lv;
	CELLP pcStart,pcEnd;				/* list head */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* (in jack boot) (in pump boot) (in wrench boot) */

	lv.psString="jack";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	lv.psString="boot";
	pc2=MakeLiteralForm(ATOM_IDENT,lv);
	pc1->pcNext=pc2;
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
		pc1);
	lv.psString="pump";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	lv.psString="boot";
	pc2=MakeLiteralForm(ATOM_IDENT,lv);
	pc1->pcNext=pc2;
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
		pc1);
	lv.psString="wrench";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	lv.psString="boot";
	pc2=MakeLiteralForm(ATOM_IDENT,lv);
	pc1->pcNext=pc2;
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
		pc1);

	/* (closed boot) */

	lv.psString="boot";
	pc1=MakeLiteralForm(ATOM_IDENT,lv);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"closed"),
		pc1);

	return pcStart;
}

/* NewWheels

Description:
	Describe all new wheels.
*/

static CELLP NewWheels
(
	int nWheels							/* number of wheels */
)
{
	int n;
	char acWheel[16];
	CELLP pcStart,pcEnd;				/* list of new wheels */
	CELLP pc1,pc2;
	LITVAL lv;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(n=0;n<nWheels;n++)
	{
		sprintf(acWheel,"good-tyre-%d",n);
	
		/* (object ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
			pc1);

		/* (wheel ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"wheel"),
			pc1);

		/* (in ?w boot) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString="boot";
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
			pc1);

		/* (intact ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"intact"),
			pc1);

		/* (flat ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"flat"),
			pc1);
	}
	return pcStart;
}

/* UsedWheels

Description:
	Describe all used wheels.
*/

static CELLP UsedWheels
(
	int nWheels							/* number of wheels */
)
{
	int n;
	char acWheel[16];
	char acHub[16];
	CELLP pcStart,pcEnd;				/* list of new wheels */
	CELLP pc1,pc2;
	LITVAL lv;

	if(nWheels>MAX_WHEELS)
	{
		ErrorMessage("Too many wheels: %d\n",nWheels);
		nWheels=MAX_WHEELS;
	}

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(n=0;n<nWheels;n++)
	{
		sprintf(acWheel,"flat-tyre-%d",n);
		sprintf(acHub,"hub-%d",n);

		/* (object ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
			pc1);

		/* (wheel ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"wheel"),
			pc1);

		/* (object ?h) */

		lv.psString=acHub;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),
			pc1);

		/* (hub ?h) */

		lv.psString=acHub;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"hub"),
			pc1);

		/* (flat ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"flat"),
			pc1);

		/* (on-ground ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on-ground"),
			pc1);

		/* (on-ground ?h) */

		lv.psString=acHub;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on-ground"),
			pc1);

		/* (on ?w ?h) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString=acHub;
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),
			pc1);

		/* (tight nuts ?w) */

		lv.psString="nuts";
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString=acWheel;
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"tight"),
			pc1);
	}
	return pcStart;
}

/* NewWheelGoals

Description:
	Describe all the new wheel goals.
*/

static CELLP NewWheelGoals
(
	int nWheels							/* number of wheels */
)
{
	int n;
	char acWheel[16];
	char acHub[16];
	CELLP pcStart,pcEnd;				/* list of new wheels */
	CELLP pc1,pc2;
	LITVAL lv;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(n=0;n<nWheels;n++)
	{
		sprintf(acWheel,"good-tyre-%d",n);
		sprintf(acHub,"hub-%d",n);

		/* (on ?w ?h) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString=acHub;
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),
			pc1);

		/* (tight nuts ?w) */

		lv.psString="nuts";
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString=acWheel;
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"tight"),
			pc1);

		/* (inflated ?w) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"inflated"),
			pc1);

		/* (on-ground ?h) */

		lv.psString=acHub;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on-ground"),
			pc1);
	}
	return pcStart;
}

/* UsedWheelGoals

Description:
	Describe all the used wheel goals.
*/

static CELLP UsedWheelGoals
(
	int nWheels							/* number of wheels */
)
{
	int n;
	CELLP pcStart,pcEnd;				/* list head */
	CELLP pc1,pc2;
	LITVAL lv;
	char acWheel[16];
	
	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(n=0;n<nWheels;n++)
	{
		sprintf(acWheel,"flat-tyre-%d",n);

		/* (in ?w boot) */

		lv.psString=acWheel;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString="boot";
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"in"),
			pc1);
	}
	return pcStart;
}
