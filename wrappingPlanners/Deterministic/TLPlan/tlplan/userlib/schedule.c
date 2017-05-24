/* schedule.c -- schedule world support */

/* Random Schedule Problems ----------------------------------------------------

(random-schedule-world size)
	Return a world list containing size objects.
(random-schedule-goal size)	-- Return a goal list of size new features
	to achieve. CALL (random-schedule-world) FIRST.

Functions for generating a random schedule world initial state and a goal
state. We generate tests by with the following parameters:

Given:
	numobjects: number of different objects to schedule.
	numgoals: the length of the goal-state.

(a) generate an initial world state, by randomly selecting values
	for each of the four features: shape, surface-condition,
	painted, and has-hole (the other predicates are always fixed).
(b) create a goal list by again selecting random features
	among the previous ones, not true in init state, for each object.
(c) generate a goal-state by randomly selecting numgoals from the
	goal list.
(d) run the planner with the initial world state in (a) and the goal
	state in (b).

We vary the size of the goal state using numgoals.
For Tlplan, although the depth of expansion is the same for goal-states
of equal length, those built from a small number of objects tend to be
a little bit easier in tlplan, because the overheads at the level of
action-application (i.e., node overhead) is smaller.
*/

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../tlplan.h"
//#include "../compute.h"
#include "../eval.h"
#include "../formula.h"
#include "../list.h"
#include "../makeform.h"
#include "../tl_tab.h"
#include "../util.h"

/* local data */

static CELLP pcScheduleWorld;			/* local pointer to initial world */
static int nSeed;						/* plan number */
static int nObjects;					/* number of objects */

static char *apsObjects[]=
{
	"a","b","c","d","e","f","g","h","i","j","k","l","m",
	"n","o","p","q","r","s","t","u","v","w","x","y","z",
	"aa","ab","ac","ad","ae","af","ag","ah","ai","aj","ak","al","am",
	"an","ao","ap","aq","ar","as","at","au","av","aw","ax","ay","az",
	"ba","bb","bc","bd","be","bf","bg","bh","bi","bj","bk","bl","bm",
	"bn","bo","bp","bq","br","bs","bt","bu","bv","bw","bx","by","bz",
	"ca","cb","cc","cd","ce","cf","cg","ch","ci","cj","ck","cl","cm",
	"cn","co","cp","cq","cr","cs","ct","cu","cv","cw","cx","cy","cz"
};
static char *apsColors[]=
{
	"red","black","blue","yellow"
};
static int anHoleSizes[]=
{
	1,2,3
};
static char *apsOrientations[]=
{
	"front","back"
};
static char *apsShapes[]=
{
	"oblong","circular","cylindrical"
};
static char *apsSurfaces[]=
{
	"polished","rough","smooth"
};
static char *apsPresses[]=
{
	"punch","drill-press"
};
static char *apsPainters[]=
{
	"spray-painter","immersion-painter"
};

/* local function prototypes */

static CELLP GetObjects
(
	int nCount							/* number of objects */
);
static CELLP ObjectPredicates
(
	CELLP pcObjects						/* object list */
);
static CELLP ToolPredicates(void);
static CELLP RandomWorldFeaturePredicates
(
	CELLP pcObjects
);
static int nSelectNot
(
	int anArray[],						/* selection array */
	int nCount,							/* size of array */
	int nExclude						/* item to exclude */
);
static char *psSelectNot
(
	char *apsArray[],					/* selection array */
	int nCount,							/* size of array */
	char *psExclude						/* item to exclude */
);
static CELLP SelectRandomPredicates
(
	int nCount,							/* number of predicates to pick */
	CELLP pcSet							/* set of predicates */
);

/* RandomScheduleWorld ---------------------------------------------------------

Description:
	Generate a randomly selected world, given the objects.
Call:
	(random-schedule-world <seed> <objects>)
*/

DECLSPECX CELLP RandomScheduleWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcWorld,pcEnd;				/* world list */
	CELLP pcObjects;					/* object list */
	int n;

	pcScheduleWorld=NULL;				/* initialize goal generator */

	/* get the plan number */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&n))
		return NULL;					/* message already printed */
	if(n!=-1)							/* default the seed if n==-1 */
	{
		nSeed=n;
		if(nSeed<0)
			nSeed=0;
		FibSeed(nSeed);
	}

	/* get the number of objects */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nObjects))
		return NULL;					/* message already printed */
	if(nObjects<1||nObjects>sizeof(apsObjects)/sizeof(char *))
	{
		ErrorMessage("random-blocks-world:  Object limit must between 1 and %d\n",
			sizeof(apsObjects)/sizeof(char *));
		return NULL;
	}

	/* generate the world description */
	
	pcWorld=NULL;
	pcEnd=(CELLP)&pcWorld;
	pcObjects=GetObjects(nObjects);
	for(pcEnd->pcNext=ToolPredicates();
		pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	for(pcEnd->pcNext=ObjectPredicates(pcObjects);
		pcEnd->pcNext;pcEnd=pcEnd->pcNext);
	for(pcEnd->pcNext=RandomWorldFeaturePredicates(pcObjects);
		pcEnd->pcNext;pcEnd=pcEnd->pcNext);

	pcScheduleWorld=pcWorld;			/* save a copy for goal generator */

	{
		FILE *ofs;

		ofs=fopen("foo.tmp","w");
		PrintFormulaList(ofs,pcWorld);
		fclose(ofs);
	}
	
	return pcWorld;
}

/* RandomScheduleGoal

Description:
	Generate a randomly selected list of features that can be achieved
	by tools but not already true in the world-list.
Call:
	(random-schedule-goal <features>)

*/

DECLSPECX CELLP RandomScheduleGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nFeatures;						/* number of features to achieve */
	CELLP pcStart,pcEnd;
	CELLP pcObj;
	CELLP pc1,pc2;
	LITVAL lv1,lv2;
	
	/* get the number of features */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nFeatures))
		return NULL;					/* message already printed */
	if(nFeatures<1||nFeatures>nObjects*5)
	{
		ErrorMessage("random-blocks-world:  Feature limit must between 1 and %d\n",
			nObjects*5);
		return NULL;
	}

	/* fill in the plan name */
	
	sprintf(acBuffer,"%d Objects, %d Features, Plan %d",nObjects,nFeatures,nSeed);
	psPlanName=StrAlloc(acBuffer);

	/* randomly change all of the predicates (except temperature) */
	
	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	for(pcObj=pcScheduleWorld;pcObj;pcObj=pcObj->pcNext)
	{
		pc1=pcObj->pfForm->pcArgs;
		pc2=pc1->pcNext;
		if(strcmp(IdentName(pcObj->pfForm),"has-hole")==0)
		{
			pc1=CopyCell(pc1);
			lv1.nInteger=nSelectNot(anHoleSizes,sizeof(anHoleSizes)/sizeof(int),
				pc2->pfForm->uValue.nInteger);
			pc2=pc1->pcNext=MakeLiteralForm(ATOM_INTEGER,lv1);
			lv2.psString=psSelectNot(apsOrientations,sizeof(apsOrientations)/sizeof(int),
				IdentName(pc1->pfForm));
			pc2->pcNext=MakeLiteralForm(ATOM_IDENT,lv2);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"has-hole"),pc1);
		}
		else if(strcmp(IdentName(pcObj->pfForm),"painted")==0)
		{
			pc1=CopyCell(pc1);
			lv1.psString=psSelectNot(apsColors,sizeof(apsColors)/sizeof(int),
				IdentName(pc1->pfForm));
			pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"painted"),pc1);
		}
		else if(strcmp(IdentName(pcObj->pfForm),"surface-condition")==0)
		{
			pc1=CopyCell(pc1);
			lv1.psString=psSelectNot(apsSurfaces,sizeof(apsSurfaces)/sizeof(int),
				IdentName(pc1->pfForm));
			pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"surface-condition"),pc1);
		}
		else if(strcmp(IdentName(pcObj->pfForm),"shape")==0)
		{
			if(strcmp(IdentName(pc2->pfForm),"cylindrical")!=0)
			{
				pc1=CopyCell(pc1);
				lv1.psString="cylindrical";
				pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
				pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"shape"),pc1);
			}
		}
	}

	/* select the specified number of random features */

	pcStart=SelectRandomPredicates(nFeatures,pcStart);

	{
		FILE *ofs;

		ofs=fopen("baz.tmp","w");
		PrintFormulaList(ofs,pcScheduleWorld);
		fclose(ofs);
	}
	return pcStart;
}

/* nSelectNot

Description:
	Randomly select a value from an integer array, with 1 exclusion.
*/

static int nSelectNot
(
	int anArray[],						/* selection array */
	int nCount,							/* size of array */
	int nExclude						/* item to exclude */
)
{
	int i,j;							/* loop indices */
	int *pnIndirect;					/* indirection array */

	/* get the index of the excluded item */
	
	pnIndirect=(int *)MemAlloc(nCount*sizeof(int));
	j=0;
	for(i=0;i<nCount;i++)
	{
		if(anArray[i]!=nExclude)
			pnIndirect[j++]=i;
	}
	return anArray[pnIndirect[FibRand()%j]];
}

/* psSelectNot

Description:
	Randomly select a value from an integer array, with 1 exclusion.
*/

static char *psSelectNot
(
	char *apsArray[],					/* selection array */
	int nCount,							/* size of array */
	char *psExclude						/* item to exclude */
)
{
	int i,j;							/* loop indices */
	int *pnIndirect;					/* indirection array */

	/* get the index of the excluded item */
	
	pnIndirect=(int *)MemAlloc(nCount*sizeof(int));
	j=0;
	for(i=0;i<nCount;i++)
	{
		if(strcmp(apsArray[i],psExclude))
			pnIndirect[j++]=i;
	}
	return apsArray[pnIndirect[FibRand()%j]];
}

/* GetObjects

Description:
	Generate nCount different objects.
*/

static CELLP GetObjects
(
	int nCount							/* number of objects */
)
{
	CELLP pcObjs,pcEnd;					/* object list */
	LITVAL lv;							/* literal value */
	int i;								/* loop index */

	pcObjs=NULL;
	pcEnd=(CELLP)&pcObjs;
	for(i=0;i<nCount;i++)
	{
		lv.psString=apsObjects[i];
		pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
	}
	return pcObjs;
}

/* ObjectPredicates

Description:
	Generate objects for the passed objects.
*/

static CELLP ObjectPredicates
(
	CELLP pcObjects						/* object list */
)
{
	CELLP pcStart,pcEnd;				/* object list */
	CELLP pc,pc1;						/* object pointer */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	for(pc=pcObjects;pc;pc=pc->pcNext)
	{
		pc1=CopyCell(pc);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"object"),pc1);
	}
	return pcStart;
}

/* ToolPredicates

Description:
	Generate a list of predicates that must be added to each initial world state
*/

static CELLP ToolPredicates(void)
{
	CELLP pcStart,pcEnd;				/* result list */
	CELLP pc1;							/* item pointers */
	LITVAL lv1,lv2;						/* literal values */
	int i,j;							/* loop indices */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	
	/* current-time */
	
	lv1.nInteger=0;
	pc1=MakeLiteralForm(ATOM_INTEGER,lv1);
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"schedule-current-time"),pc1);

	/* has-bit */
		
	for(i=0;i<sizeof(apsPresses)/sizeof(char *);i++)
	{
		for(j=0;j<sizeof(anHoleSizes)/sizeof(int);j++)
		{
			lv1.psString=apsPresses[i];
			pc1=MakeLiteralForm(ATOM_IDENT,lv1);
			lv2.nInteger=anHoleSizes[j];
			pc1->pcNext=MakeLiteralForm(ATOM_INTEGER,lv2);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"has-bit"),pc1);
		}
	}
	
	/* has-paint */

	for(i=0;i<sizeof(apsPainters)/sizeof(char *);i++)
	{
		for(j=0;j<sizeof(apsColors)/sizeof(char *);j++)
		{
			lv1.psString=apsPainters[i];
			pc1=MakeLiteralForm(ATOM_IDENT,lv1);
			lv2.psString=apsColors[j];
			pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv2);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"has-paint"),pc1);
		}
	}
	
	/* can-orient */

	for(i=0;i<sizeof(apsPresses)/sizeof(char *);i++)
	{
		for(j=0;j<sizeof(apsOrientations)/sizeof(char *);j++)
		{
			lv1.psString=apsPresses[i];
			pc1=MakeLiteralForm(ATOM_IDENT,lv1);
			lv2.psString=apsOrientations[j];
			pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv2);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"can-orient"),pc1);
		}
	}
	return pcStart;
}

/* RandomWorldFeaturePredicates

Description:
	Randomly select possible initial values for the features of objects.
*/

static CELLP RandomWorldFeaturePredicates
(
	CELLP pcObjects
)
{
	CELLP pcObj;
	CELLP pcStart,pcEnd;
	CELLP pc1,pc2;
	LITVAL lv1,lv2;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(pcObj=pcObjects;pcObj;pcObj=pcObj->pcNext)
	{
		/* temperature */
	
		pc1=CopyCell(pcObj);
		lv1.psString="cold";
		pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"temperature"),pc1);

		/* has-hole */

		pc1=CopyCell(pcObj);
		lv1.nInteger=anHoleSizes[FibRand()%(sizeof(anHoleSizes)/sizeof(int))];
		pc2=pc1->pcNext=MakeLiteralForm(ATOM_INTEGER,lv1);
		lv2.psString=apsOrientations[FibRand()%(sizeof(apsOrientations)/sizeof(char *))];
		pc2->pcNext=MakeLiteralForm(ATOM_IDENT,lv2);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"has-hole"),pc1);

		/* painted */

		pc1=CopyCell(pcObj);
		lv1.psString=apsColors[FibRand()%(sizeof(apsColors)/sizeof(char *))];
		pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"painted"),pc1);

		/* surface-condition */

		pc1=CopyCell(pcObj);
		lv1.psString=apsSurfaces[FibRand()%(sizeof(apsSurfaces)/sizeof(char *))];
		pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"surface-condition"),pc1);

		/* shape */

		pc1=CopyCell(pcObj);
		lv1.psString=apsShapes[FibRand()%(sizeof(apsShapes)/sizeof(char *))];
		pc1->pcNext=MakeLiteralForm(ATOM_IDENT,lv1);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"shape"),pc1);
	}
	return pcStart;
}	

/* SelectRandomPredicates

Description:
	Randomly select nCount predicates from a set.
*/

static CELLP SelectRandomPredicates
(
	int nCount,							/* number of predicates to pick */
	CELLP pcSet							/* set of predicates */
)
{
	CELLP pcStart,pcEnd;
	CELLP pcList;
	CELLP pc;
	int nLength;						/* size of set */
	int nLimit;							/* maximum number of predicates */
	int nRand;							/* random value */
	int i,j;							/* loop indices */

	/* calculate the maximum number of predicates */
	
	nLength=0;
	for(pc=pcSet;pc;pc=pc->pcNext)
		nLength++;
	nLimit=MIN(nCount,nLength);

	/* select predicates at random */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	pcList=CopyCellList(pcSet);
	for(i=nCount;i>0;i--)
	{
		nRand=(FibRand()%nLength)-1;	/* generate random index */
		if(nRand<0)
		{
			pcEnd=pcEnd->pcNext=pcList;
			pcList=pcList->pcNext;
		}
		else
		{
			pc=(CELLP)&pcList;			/* locate preceding entry in list */
			for(j=0;j<nRand;j++)
				pc=pc->pcNext;
			pcEnd=pcEnd->pcNext=pc->pcNext;	/* transfer to other list */
			if(pc->pcNext)
				pc->pcNext=pc->pcNext->pcNext;
		}
		nLength--;
	}
	pcEnd->pcNext=NULL;					/* terminate list */
	return pcStart;
}

/* MakeDescPredicate

Description:
	This routine generates a described predicate from 1 or more strings.
*/

CELLP MakeDescPredicate
(
	char *psPred,						/* name of predicate */
	int nCount,							/* number of arguments */
	...									/* names of arguments */
)
{
	CELLP pcArgs,pcEnd;					/* argument list */
	CELLP pcPred;						/* resulting predicate */
	LITVAL lv;							/* literal value */
	int i;								/* loop index */
	va_list pa;							/* argument pointer */

	/* convert arguments to literals */
	
	pcArgs=NULL;
	pcEnd=(CELLP)&pcArgs;
	va_start(pa,nCount);
	for(i=0;i<nCount;i++)
	{
		lv.psString=va_arg(pa,char *);
		pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
	}
	va_end(pa);

	/* create the actual predicate */

	pcPred=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,psPred),pcArgs);
	return pcPred;
}

