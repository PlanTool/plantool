/* Support for 1OpsBounded Blocks ------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>

#include "../tlplan.h"
#include "../eval.h"
#include "../formula.h"
#include "../list.h"
#include "../makeform.h"
#include "../tl_tab.h"
#include "../util.h"

// local function prototypes

static CELLP MakeGoalLiterals
(
	CELLP pcTowers
);
static CELLP MakeBoundedLiterals
(
	int nSpace,
	CELLP pcTowers
);
static CELLP RandomBoundedTowers
(
	int nLimit,							/* number of blocks to generate */
	int nSpace							/* number of spaces on table */
);
static CELLP MakeBoundedGoalLiterals
(
	CELLP pcTowers
);
static void PermuteTower
(
	CELLP pcTower						/* tower of blocks to permute */
);
static CELLP MakeTableSpaceLiteral
(
	int nTowers,						// maximum number of towers
	CELLP pcTowers						// list of towers
);
static CELLP MakeInitialLiterals
(
	CELLP pcTowers
);
static CELLP MakeLiterals
(
	CELLP pcTowers
);
static int MSRandx(void);

/* local data */

static int nSeed;						/* plan number */

/* Random1OpsBoundedBlocksWorld (Function) -----------------------------------------

Description:
	Generate a list of literals describing a complete configuration of
	an nLimit, nSpace bounded blocks world.
*/

DECLSPECX CELLP Random1OpsBoundedBlocksWorld
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

	return MakeBoundedLiterals(nSpace,RandomBoundedTowers(nLimit,nSpace));
}

/* Random1OpsBoundedBlocksGoal (Function)

Description:
	Generate a list of literals describing an incomplete configuration of
	an nLimit, nSpace bounded blocks world.
*/

DECLSPECX CELLP Random1OpsBoundedBlocksGoal
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

/* MakeBoundedLiterals (Local Function)

Description:
	Return a list of literals representing a list of towers for bounded blocks
	world.
*/

static CELLP MakeBoundedLiterals
(
	int nSpace,
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;
	LITVAL lv;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd=pcEnd->pcNext=MakeTableSpaceLiteral(nSpace,pcTowers);
	lv.psString="table";
	pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"clear"),
		MakeLiteralForm(ATOM_IDENT,lv));		
	pcEnd->pcNext=MakeLiterals(pcTowers);

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

	pcEnd->pcNext=MakeLiterals(pcTowers);
	return pcStart;
}

/* MakeGoalLiterals (Local Function)

Description:
	Generate a list of literals representing the state of the blocks.
Notes:
	We do not generate any (clear C) or (on c table) predicates.
*/

static CELLP MakeGoalLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	pcEnd->pcNext=MakeLiterals(pcTowers);
	while(pcEnd->pcNext)
		pcEnd=pcEnd->pcNext;

	return pcStart;
}

/* MakeLiterals (Local Function)

Description:
	Generate (on A C) literals.
*/

static CELLP MakeLiterals
(
	CELLP pcTowers
)
{
	CELLP pcStart,pcEnd;
	CELLP pc1,pc2,pc3;
	LITVAL lv;

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	/* generate predicates from each pair of blocks in each stack */

	for(pc1=pcTowers;pc1;pc1=pc1->pcNext)
	{
		pc2=pc1->pfForm->pcArgs;		/* point to first block in stack */
		if(pc2)
		{
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"clear"),
				CopyFormula(pc2));

			for(;pc2->pcNext;pc2=pc2->pcNext)	/* point to each block in stack */
			{
				pc3=CopyFormula(pc2);
				pc3->pcNext=CopyFormula(pc2->pcNext);
				pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),pc3);
			}

			pc3=CopyFormula(pc2);
			lv.psString="table";
			pc3->pcNext=MakeLiteralForm(ATOM_IDENT,lv);
			pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),pc3);
		}
	}
	return pcStart;
}

/* MakeTableSpaceLiteral (Local Function)

Description:
	Generate a (= (table-space) n) literal.
*/

static CELLP MakeTableSpaceLiteral
(
	int nTowers,						// maximum number of towers
	CELLP pcTowers						// list of towers
)
{
	int nCount;
	CELLP pc;

	/* calculate the number of empty slots */

	nCount=0;
	for(pc=pcTowers;pc;pc=pc->pcNext)
		if(pc->pfForm->pcArgs)
			nCount++;

	/* generate the literal */

	pc=MakeEqForm(FALSE,MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"table-space"),NULL),
		MakeIntegerForm(nTowers-nCount));

	return pc;
}

/* MSRandx

Description:
	Generate a random number.
	This algorithm is compatible with Microsoft's rand function.
*/

static int MSRandx(void)
{
	nSeed=nSeed*214013+2531011;		/* 0x00034DFD & 0x00269EC3 */
	return (nSeed>>16)&0x00007FFF;
}
