/* blocks.c

Description:
	User support routines for various blocks worlds.

Notes:
	The print routines are disused.  They remain as examples.
	However, they are out of date and probably will not work without fixing.
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

static CELLP MakeLiterals
(
	BOOL bHandEmpty,					// handempty flag
	CELLP pcTowers
);
static CELLP MakeGoalLiterals
(
	BOOL bHandEmpty,					// handempty flag
	CELLP pcTowers
);
static CELLP Make1OpsLiterals
(
	CELLP pcTowers
);
static CELLP Make1OpsGoalLiterals
(
	CELLP pcTowers
);
static CELLP RandomTowers
(
	int nLimit							/* number of blocks in problem */
);
static CELLP RandomNums
(
	int nLimit
);
static CELLP RandomBlocks
(
	int nCount
);
static CELLP MakeClearLiterals
(
	BOOL bTable,						/* generate (clear table) literal */
	CELLP pcTowers
);
static CELLP MakeOnTableLiterals
(
	CELLP pcTowers
);
static CELLP MakeOnLiterals
(
	CELLP pcTowers
);
static CELLP Make1OpsOnTableLiterals
(
	CELLP pcTowers
);
static int MSRandx(void);
static CELLP RandomBoundedTowers
(
	int nLimit,							/* number of blocks to generate */
	int nSpace							/* number of spaces on table */
);
static CELLP MakeBoundedLiterals
(
	CELLP pcTowers
);
static void PermuteTower
(
	CELLP pcTower						/* tower of blocks to permute */
);
static CELLP MakeTableSpaceLiteral
(
	CELLP *ppcTowers					/* pointer to tower list */
);
static CELLP MakeBoundedGoalLiterals
(
	CELLP pcTowers
);


/* local data */

static CELLP pcTower;					/* current tower */
static CELLP pcTowers;					/* accumulated towers */
static CELLP pcGoodTowers;				/* accumulated good tower block list */
static int nSeed;						/* plan number */

/* PushBlock (Pseudo predicate)

Description:
	Add a block to the top of a tower.
*/

DECLSPECX BOOL PushBlock
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pcTower)
		pcTower=pc;
	else
	{
		pc=CopyCell(pc);
		pc->pcNext=pcTower;
		pcTower=pc;
	}
	return TRUE;
}

/* PushSpace (Pseudo Predicate)

Description:
	Add an "unknown" block to the top of a tower.
Notes:
	This is primarily for goal worlds, where there may be an incomplete
	description of some towers.
*/

DECLSPECX BOOL PushSpace
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i;
	int nSpaces;

	if(!FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nSpaces))
		return TRUE;					/* message already printed */

	for(i=0;i<nSpaces;i++)
		PushBlock(MakeStringForm("?"),plpLinearPlan,pbBindings);
	return TRUE;
}

/* EndTower (Pseudo Predicate)

Description:
	The current tower is done... add it to the list of towers.
*/

DECLSPECX BOOL EndTower(void)
{
	int i;
	CELLP pc;

	if(pcTower)
	{
		/* count up the blocks in the tower, and put the count in an integer
		formula.  Use the (normally unused) argument list of the integer formula
		to point to the tower. */

		i=0;
		for(pc=pcTower;pc;pc=pc->pcNext)
			i++;
		pc=MakeIntegerForm(i);
		pc->pfForm->pcArgs=pcTower;
		pcTower=NULL;

		if(!pcTowers)
			pcTowers=pc;
		else
		{
			pc->pcNext=pcTowers;
			pcTowers=pc;
		}
	}
	return TRUE;
}

/* PrintTowers (Pseudo Predicate)

Description:
	Print all towers, in vertical format.
*/

DECLSPECX BOOL PrintTowers
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nMaxHeight;
	int i;
	CELLP pc;
	CELLP pcBlock;
	int nFile;
	FILE *pfFile;

	FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile);
	pfFile=HandleToFileStream(nFile);

	/* calculate the maximum height of the towers */

	nMaxHeight=0;
	for(pc=pcTowers;pc;pc=pc->pcNext)
	{
		if(pc->pfForm->uValue.nInteger>nMaxHeight)
			nMaxHeight=pc->pfForm->uValue.nInteger;
	}

	/* scan across towers, printing blanks or blocks */

	for(i=0;i<nMaxHeight;i++)
	{
		CommandPrintf(pfFile," ");
		for(pc=pcTowers;pc;pc=pc->pcNext)
		{
			if(pc->pfForm->uValue.nInteger<nMaxHeight)
			{
				pc->pfForm->uValue.nInteger++;
				CommandPrintf(pfFile,"      ");
			}
			else
			{
				pcBlock=pc->pfForm->pcArgs;
//				CommandPrintf(pfFile," %-5s",NameOf(pcBlock->pfForm));
				CommandPrintf(pfFile," %-5s",IdentName(pcBlock->pfForm));
				pc->pfForm->pcArgs=pcBlock->pcNext;
				pcBlock->pcNext=NULL;
			}
		}
		CommandPrintf(pfFile,"\n");
	}
	return TRUE;
}

/* ClearAccumulators (Pseudo Predicate)

Description:
	Deallocate lists of blocks accumulated during printing.
*/

DECLSPECX BOOL ClearAccumulators(void)
{
	pcTower=NULL;
	pcTowers=NULL;
	pcGoodTowers=NULL;
	return TRUE;
}

/* AccumulateGoodTowers (Pseudo Predicate)

Description:
	Save a list of good tower blocks.  (for Linear blocks world)
*/

DECLSPECX BOOL AccumulateGoodTowers
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	pc=CopyCell(pc);
	pc->pcNext=pcGoodTowers;
	pcGoodTowers=pc;
	return TRUE;
}

/* PrintGoodTowers (Pseudo Predicate)

Description:
	Print the blocks that have good-tower-below.  (for Linear blocks world)
*/

DECLSPECX BOOL PrintGoodTowers
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int nFile;
	FILE *pfFile;

	FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile);
	pfFile=HandleToFileStream(nFile);

	if(pcGoodTowers)
	{
		CommandPrintf(pfFile,"Good towers: ");
		for(pc=pcGoodTowers;pc;pc=pc->pcNext)
//			CommandPrintf(pfFile," %s",NameOf(pc->pfForm));
			CommandPrintf(pfFile," %s",IdentName(pc->pfForm));
		CommandPrintf(pfFile,"\n");
	}
	return TRUE;
}

/* Random Blocks Problems ------------------------------------------------------

Functions for generating a random blocks world initial state and goal state.

You can execute (set-initial-world (random-blocks-world <seed> 25))
to get an random initial world configuration with 25 blocks.

You can also use (set-goal (random-blocks-world <seed> 25))
to set up a random goal with 25 blocks.

(random-blocks-goal ...) is provided as sometimes you want to use incompletely
specified goals.

*/

/* RandomBlocksWorld (Function)

Description:
	Generate a list of literals describing a complete configuration of
	an nLimit blocks world.
*/

DECLSPECX CELLP RandomBlocksWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nLimit;
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks, Plan %d",nLimit,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return MakeLiterals(TRUE,RandomTowers(nLimit));
}

/* RandomBlocksGoal (Function)

Description:
	Generate a list of literals describing an incomplete configuration of
	an nCount blocks world.  In particular do not include any clear literals.
*/

DECLSPECX CELLP RandomBlocksGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nLimit;
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-goal:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks, Plan %d",nLimit,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return MakeGoalLiterals(TRUE,RandomTowers(nLimit));
}

/* MakeLiterals (Local Function)

Description:
	Generate a list of literals representing the state of the blocks.
*/

static CELLP MakeLiterals
(
	BOOL bHandEmpty,					// handempty flag
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd->pcNext=MakeClearLiterals(FALSE,pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeOnTableLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeOnLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	if(bHandEmpty)
		pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"handempty"),NULL);
	return pcStart;
}

/* MakeGoalLiterals (Local Function)

Description:
	Generate a list of literals representing the state of the blocks.
Notes:
	We do not generate any (clear C) or (ontable c) predicates.
*/

static CELLP MakeGoalLiterals
(
	BOOL bHandEmpty,					// handempty flag
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd->pcNext=MakeOnTableLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeOnLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	if(bHandEmpty)
		pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"handempty"),NULL);
	return pcStart;
}

/* RandomTowers (Local Function)

Description:
	Generate a list of block stacks at random.
Notes:
	Generates a list of sublists, each sublist is a list of blocks.
	Thus, (A B C) means on(A B) and on(B C).
	Each sublist is headed by an integer formula, which contains the
	size of the sublist.  The blocks in each stack are pointed to by
	the otherwise unused pfArgs pointers.
*/

static CELLP RandomTowers
(
	int nLimit							/* number of blocks in problem */
)
{
	CELLP pcStackLengths;				/* list of block stack lengths */
	CELLP pcBlocks;
	CELLP pc1,pc2;
	int i;

	/* set up stack lengths and random permutation of blocks */

	pcStackLengths=RandomNums(nLimit);
	pcBlocks=RandomBlocks(nLimit);

	/* subdivide random block list into stacks */

	for(pc1=pcStackLengths;pc1;pc1=pc1->pcNext)
	{
		pc1->pfForm->pcArgs=pcBlocks;	/* link block list as argument */
		pc2=(CELLP)&pc1->pfForm->pcArgs;
		for(i=0;i<pc1->pfForm->uValue.nInteger;i++)	/* locate last block in this stack */
			pc2=pc2->pcNext;
		pcBlocks=pc2->pcNext;			/* split block list */
		pc2->pcNext=NULL;
	}
	return pcStackLengths;
}

/* RandomNums (Local Function)

Description:
	Generate a list of numbers, such that their sum = limit.
	This is used to generate a set of random height towers for a
	blocks problem of size "limit".
*/

static CELLP RandomNums
(
	int nLimit
)
{
	CELLP pcStart,pcEnd;

	int nNewLimit;
	int nRand;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(nNewLimit=nLimit;nNewLimit>0;nNewLimit-=nRand)
	{
//		nRand=1+(FibRand()%nNewLimit);	/* random number between 1 and nNewLimit */
		nRand=1+(MSRandx()%nNewLimit);	/* random number between 1 and nNewLimit */
		pcEnd=pcEnd->pcNext=MakeIntegerForm(nRand);
	}
	return pcStart;
}

/* RandomBlocks (Local Function)

Description:
	Generate a random permutation of nCount blocks.
*/

static CELLP RandomBlocks
(
	int nCount
)
{
	CELLP pcStart,pcEnd;
	CELLP pcList,pc1;
	char buffer[8];
	LITVAL lv;
	int i,j;
	int nRand;

	/* generate an initial list of blocks */

	pcList=NULL;
	pcEnd=(CELLP)&pcList;
	
	lv.psString=buffer;
	if(nCount<=26)
	{
		for(i=0;i<nCount;i++)
		{
			sprintf(buffer,"%c",'a'+i);
			pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
		}
	}
	else if(nCount<=26*26)
	{
		for(i=0;i<nCount;i++)
		{
			sprintf(buffer,"%c%c",'a'+i/26,'a'+i%26);
			pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
		}
	}
	else if(nCount<=26*26*26)
	{
		for(i=0;i<nCount;i++)
		{
			sprintf(buffer,"%c%c%c",'a'+i/(26*26),'a'+(i/26%26),'a'+i%26);
			pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
		}
	}
	
	/* permute the list, by selecting elements at random */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(i=nCount;i>0;i--)
	{
//		nRand=(FibRand()%i)-1;			/* generate random index */
		nRand=(MSRandx()%i)-1;			/* generate random index */
		if(nRand<0)
		{
			pcEnd=pcEnd->pcNext=pcList;
			pcList=pcList->pcNext;
		}
		else
		{
			pc1=(CELLP)&pcList;			/* locate preceding entry in list */
			for(j=0;j<nRand;j++)
				pc1=pc1->pcNext;
			pcEnd=pcEnd->pcNext=pc1->pcNext;	/* transfer to other list */
			if(pc1->pcNext)
				pc1->pcNext=pc1->pcNext->pcNext;
		}
	}
	pcEnd->pcNext=NULL;					/* terminate list */
	return pcStart;
}

/* MakeClearLiterals (Local Function)

Description:
	Generate (clear C) literals.
*/

static CELLP MakeClearLiterals
(
	BOOL bTable,						/* generate (clear table) literal */
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;
	CELLP pc1,pc2;
	LITVAL lv;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate a clear predicate from the first block in each stack */

	for(pc1=pcTowers;pc1;pc1=pc1->pcNext)
	{
		pc2=pc1->pfForm->pcArgs;		/* point to first block in stack */
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"clear"),
			CopyFormula(pc2));
	}

	if(bTable)
	{
		lv.psString="table";
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"clear"),
			pc2);
	}
	return pcStart;
}

/* MakeOnTableLiterals (Local Function)

Description:
	Generate (ontable C) literals.
*/

static CELLP MakeOnTableLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;
	CELLP pc1,pc2;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate an ontable predicate from the last block in each stack */

	for(pc1=pcTowers;pc1;pc1=pc1->pcNext)
	{
		for(pc2=pc1->pfForm->pcArgs;pc2->pcNext;pc2=pc2->pcNext);	/* point to last block in stack */
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"ontable"),
			CopyFormula(pc2));
	}
	return pcStart;
}

/* MakeOnLiterals (Local Function)

Description:
	Generate (on A C) literals.
*/

static CELLP MakeOnLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;
	CELLP pc1,pc2,pc3;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate on predicates from each pair of blocks in each stack */

	for(pc1=pcTowers;pc1;pc1=pc1->pcNext)
	{
		for(pc2=pc1->pfForm->pcArgs;pc2->pcNext;pc2=pc2->pcNext)	/* point to each block in stack */
		{
			pc3=CopyFormula(pc2);
			pc3->pcNext=CopyFormula(pc2->pcNext);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),pc3);
		}
	}
	return pcStart;
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

/* RandomBoundedBlocksWorld (Function) -----------------------------------------

Description:
	Generate a list of literals describing a complete configuration of
	an nLimit, nSpace bounded blocks world.
*/

DECLSPECX CELLP RandomBoundedBlocksWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[80];
	CELLP pc;
	int nLimit;							/* number of blocks to generate */
	int nSpace;							/* number of spaces on the table */
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* get the number of positions */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nSpace))
		return NULL;					/* message already printed */
	if(nSpace<1||nSpace>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks %d Positions, Plan %d",nLimit,nSpace,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}

	return MakeBoundedLiterals(RandomBoundedTowers(nLimit,nSpace));
}

/* RandomBoundedBlocksGoal (Function)

Description:
	Generate a list of literals describing an incomplete configuration of
	an nLimit, nSpace bounded blocks world.
*/

DECLSPECX CELLP RandomBoundedBlocksGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[80];
	CELLP pc;
	int nLimit;							/* number of blocks to generate */
	int nSpace;							/* number of spaces on the table */
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* get the number of positions */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nSpace))
		return NULL;					/* message already printed */
	if(nSpace<1||nSpace>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks %d Positions, Plan %d",nLimit,nSpace,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}

	return MakeBoundedGoalLiterals(RandomBoundedTowers(nLimit,nSpace));
}

/* RandomBoundedTowers (Local Function)

Description:
  Return a list of nSpace towers, each tower is a list that might be nil.
  (A B C) means on(A B) and on(B C), () means an empty spot on table."
*/

static CELLP RandomBoundedTowers
(
	int nLimit,							/* number of blocks to generate */
	int nSpace							/* number of spaces on table */
)
{
	CELLP pc,pc1,pc2;
	CELLP pcEnd;
	CELLP pcTowers;
	CELLP pcBlocks;
	int i;
	char buffer[8];
	LITVAL lv;

	/* generate an initial list of blocks */

	pcBlocks=NULL;
	pcEnd=(CELLP)&pcBlocks;

	lv.psString=buffer;
	if(nLimit<=26)
	{
		for(i=0;i<nLimit;i++)
		{
			sprintf(buffer,"%c",'a'+i);
			pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
		}
	}
	else if(nLimit<=26*26)
	{
		for(i=0;i<nLimit;i++)
		{
			sprintf(buffer,"%c%c",'a'+i/26,'a'+i%26);
			pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
		}
	}
	else if(nLimit<=26*26*26)
	{
		for(i=0;i<nLimit;i++)
		{
			sprintf(buffer,"%c%c%c",'a'+i/(26*26),'a'+(i/26%26),'a'+i%26);
			pcEnd=pcEnd->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
		}
	}
	
	/* set up the desired number of towers */

	pcTowers=NULL;
	pcEnd=(CELLP)&pcTowers;
	for(i=0;i<nSpace;i++)
	{
		pcEnd=pcEnd->pcNext=MemAlloc(sizeof(CELL));
		pcEnd->pfForm=MemAlloc(sizeof(FORMULA));
	}

	/* populate the towers at random with blocks */

	for(pc1=pcBlocks;pc1;pc1=pc2)
	{
		pc2=pc1->pcNext;

		pc=pcTowers;
//		for(i=0;i<FibRand()%nSpace;i++)
		for(i=0;i<MSRandx()%nSpace;i++)
			pc=pc->pcNext;
		pc1->pcNext=pc->pfForm->pcArgs;
		pc->pfForm->pcArgs=pc1;
	}

	/* permute each tower */

	for(pc=pcTowers;pc;pc=pc->pcNext)
		PermuteTower(pc);
	return pcTowers;
}

/* MakeBoundedLiterals (Local Function)

Description:
	Return a list of literals representing a list of towers for bounded blocks
	world.
*/

static CELLP MakeBoundedLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd=pcEnd->pcNext=MakeTableSpaceLiteral(&pcTowers);
	pcEnd->pcNext=MakeLiterals(TRUE,pcTowers);
	return pcStart;
}

/* MakeBoundedGoalLiterals (Local Function)

Description:
	Return a list of goal literals representing a list of towers
	for bounded blocks world.
*/

static CELLP MakeBoundedGoalLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd=pcEnd->pcNext=MakeTableSpaceLiteral(&pcTowers);
	pcEnd->pcNext=MakeGoalLiterals(TRUE,pcTowers);
	return pcStart;
}

/* PermuteTower

Description:
	Permute the order of the blocks in a tower.
*/

static void PermuteTower
(
	CELLP pcTower						/* tower of blocks to permute */
)
{
	int nLength;						/* length of the tower */
	int nRand;							/* random selection */
	int i,j;							/* loop indices */
	CELLP pc;							/* block pointer */
	CELLP pcStart,pcEnd;				/* list head */
	CELLP pc1;							/* pointer to previous block */
	CELLP pc2;							/* pointer to current block */

	/* get the length of the tower */

	nLength=0;
	for(pc=pcTower->pfForm->pcArgs;pc;pc=pc->pcNext)
		nLength++;

	/* select items at random from list, and move to new list */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(i=nLength;i;i--)
	{
//		nRand=FibRand()%i;				/* random number between 0 and i-1 */
		nRand=MSRandx()%i;				/* random number between 0 and i-1 */
		pc1=(CELLP)&pcTower->pfForm->pcArgs;
		pc2=pcTower->pfForm->pcArgs;
		for(j=0;j<nRand;j++)			/* point to selected block */
		{
			pc1=pc2;
			pc2=pc2->pcNext;
		}
		pc1->pcNext=pc2->pcNext;		/* unlink block */
		pcEnd=pcEnd->pcNext=pc2;		/* link block to new list */
	}
	pcTower->pfForm->pcArgs=pcStart;
}

/* MakeTableSpaceLiteral (Local Function)

Description:
	Count up the number of empty towers and remove them from the list.
	Return a described table-space predicate, which is true for the number
	of empty towers counted.
*/

static CELLP MakeTableSpaceLiteral
(
	CELLP *ppcTowers					/* pointer to tower list */
)
{
	int nSpace;							/* the number of empty spaces */
	CELLP pc1;							/* pointer to previous tower */
	CELLP pc2;							/* pointer to current tower */
	CELLP pc3;							/* pointer to next tower */

	nSpace=0;
	pc1=(CELLP)ppcTowers;				/* set up previous block pointer */
	for(pc2=pc1->pcNext;pc2;pc2=pc3)
	{
		pc3=pc2->pcNext;
		if(!pc2->pfForm->pcArgs)		/* if tower empty */
		{
			nSpace++;
			pc1->pcNext=pc3;
		}
	}
	return MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"table-space"),
		MakeIntegerForm(nSpace));
}

/* Random2OpsBlocksWorld (Function) --------------------------------------------

Description:
	Generate a list of literals describing a complete configuration of
	an nLimit blocks world.
*/

DECLSPECX CELLP Random2OpsBlocksWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nLimit;
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks, Plan %d",nLimit,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return MakeLiterals(FALSE,RandomTowers(nLimit));
}

/* Random2OpsBlocksGoal (Function)

Description:
	Generate a list of literals describing an incomplete configuration of
	an nCount blocks world.  In particular do not include any ontable or clear
	literals.
*/

DECLSPECX CELLP Random2OpsBlocksGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nLimit;
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-goal:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks, Plan %d",nLimit,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return MakeGoalLiterals(FALSE,RandomTowers(nLimit));
}

/* Random1OpsBlocksWorld (Function) --------------------------------------------

Description:
	Generate a list of literals describing a complete configuration of
	an nLimit blocks world.
*/

DECLSPECX CELLP Random1OpsBlocksWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nLimit;
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-world:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks, Plan %d",nLimit,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return Make1OpsLiterals(RandomTowers(nLimit));
}

/* Random1OpsBlocksGoal (Function)

Description:
	Generate a list of literals describing an incomplete configuration of
	an nCount blocks world.  In particular do not include any ontable or clear
	literals.
*/

DECLSPECX CELLP Random1OpsBlocksGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[64];
	CELLP pc;
	int nLimit;
	int n;

	/* get the plan number */
	
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

	/* get the number of blocks */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nLimit))
		return NULL;					/* message already printed */
	if(nLimit<1||nLimit>26*26*26)
	{
		ErrorMessage("random-blocks-goal:  Block limit must between 1 and %d\n",26*26*26);
		return NULL;
	}

	/* fill in the plan name */
	
	if(n!=-1)
	{
		sprintf(acBuffer,"%d Blocks, Plan %d",nLimit,nSeed);
		psPlanName=StrAlloc(acBuffer);
	}
	return Make1OpsGoalLiterals(RandomTowers(nLimit));
}

/* Make1OpsLiterals (Local Function)

Description:
	Generate a list of literals representing the state of the blocks.
*/

static CELLP Make1OpsLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd->pcNext=MakeClearLiterals(TRUE,pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=Make1OpsOnTableLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	pcEnd->pcNext=MakeOnLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	return pcStart;
}

/* Make1OpsGoalLiterals (Local Function)

Description:
	Generate a list of literals representing the state of the blocks.
Notes:
	We do not generate any (clear C) or (ontable c) predicates.
*/

static CELLP Make1OpsGoalLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd->pcNext=MakeOnLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	return pcStart;
}

/* Make1OpsOnTableLiterals (Local Function)

Description:
	Generate (on C table) literals.
*/

static CELLP Make1OpsOnTableLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;
	CELLP pc1,pc2,pc3;
	LITVAL lv;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate an (on x table) predicate from the last block in each stack */

	lv.psString="table";
	pc3=MakeLiteralForm(ATOM_IDENT,lv);
	for(pc1=pcTowers;pc1;pc1=pc1->pcNext)
	{
		for(pc2=pc1->pfForm->pcArgs;pc2->pcNext;pc2=pc2->pcNext);	/* point to last block in stack */
		pc2->pcNext=pc3;
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),
			CopyFormulaList(pc2));
		pc2->pcNext=NULL;
	}
	return pcStart;
}

/* Relevant

Description:
	This is an external predicate for "relevant" world.  It is meant to
	be added to the "move" operator preconditions to prune redundant plans 
	(i.e. plans that are longer than they need to be).
	We apply several group-theoretic tests on the list of linear plans,
	from the root to the current world.
	Since we are called during operator expansion, we are able to look ahead
	one more world.
Call:
	(relevant ?a ?b ?c)
	This corresponds to (move ?a ?b ?c) i.e. move block ?a from ?b to ?c.
*/

DECLSPECX BOOL Relevant
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp;
	CELLP pc,pc1,pc2,pc3;
	CELLP pc4,pc5,pc6;
	CELLP pc7,pc8,pc9;

	// evaluate the arguments since they may be variables

	pc=pcFormula->pfForm->pcArgs;
	pc1=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pc1)
		TermError("relevant",pc,pbBindings);
	pc=pc->pcNext;
	pc2=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pc2)
		TermError("relevant",pc,pbBindings);
	pc=pc->pcNext;
	pc3=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pc3)
		TermError("relevant",pc,pbBindings);

	// don't put a block back where it came from (identity)

//	if(StringEqQ(NameOf(pc2->pfForm),NameOf(pc3->pfForm)))
	if(StringEqQ(IdentName(pc2->pfForm),IdentName(pc3->pfForm)))
		return FALSE;

	// find closure reductions

	for(plp=plpLinearPlan;plp&&plp->pcActionName;plp=plp->plpParent)
	{
		pc4=plp->pcActionName->pfForm->pcArgs;
		pc5=pc4->pcNext;
		pc6=pc5->pcNext;
		
		// first check for a closure
		
//		if(StringEqQ(NameOf(pc1->pfForm),NameOf(pc4->pfForm))&&
//			StringEqQ(NameOf(pc2->pfForm),NameOf(pc6->pfForm)))
		if(StringEqQ(IdentName(pc1->pfForm),IdentName(pc4->pfForm))&&
			StringEqQ(IdentName(pc2->pfForm),IdentName(pc6->pfForm)))
		{
			return FALSE;
		}

		// make sure the destination hasn't moved in the mean time
		
//		if(StringEqQ(NameOf(pc3->pfForm),NameOf(pc4->pfForm)))
		if(StringEqQ(IdentName(pc3->pfForm),IdentName(pc4->pfForm)))
			break;

		// make sure the moved block hasn't had anything put on it in the mean time

//		if(StringEqQ(NameOf(pc1->pfForm),NameOf(pc6->pfForm)))
		if(StringEqQ(IdentName(pc1->pfForm),IdentName(pc6->pfForm)))
			break;

		// make sure the destination hasn't had anything taken off of it in the mean time

//		if(StringEqQ(NameOf(pc3->pfForm),NameOf(pc5->pfForm))&&
//			stricmp(NameOf(pc3->pfForm),"table")!=0)
		if(StringEqQ(IdentName(pc3->pfForm),IdentName(pc5->pfForm))&&
			stricmp(IdentName(pc3->pfForm),"table")!=0)
			break;
	}

	// find other reductions

	plp=plpLinearPlan;
	if(plp&&plp->pcActionName)
	{
		pc4=plp->pcActionName->pfForm->pcArgs;
		pc5=pc4->pcNext;
		pc6=pc5->pcNext;

		plp=plp->plpParent;
		if(plp&&plp->pcActionName)
		{
			pc7=plp->pcActionName->pfForm->pcArgs;
			pc8=pc7->pcNext;
			pc9=pc8->pcNext;
//			if(StringEqQ(NameOf(pc1->pfForm),NameOf(pc7->pfForm))&&
//				!StringEqQ(NameOf(pc6->pfForm),NameOf(pc8->pfForm))&&
//				!StringEqQ(NameOf(pc4->pfForm),NameOf(pc8->pfForm)))
			if(StringEqQ(IdentName(pc1->pfForm),IdentName(pc7->pfForm))&&
				!StringEqQ(IdentName(pc6->pfForm),IdentName(pc8->pfForm))&&
				!StringEqQ(IdentName(pc4->pfForm),IdentName(pc8->pfForm)))
				return FALSE;
		}
	}
	
	return TRUE;
}
