// freecell.c

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "../tlplan.h"
#include "../compute.h"
#include "../eval.h"
#include "../formula.h"
#include "../iface.h"
#include "../list.h"
#include "../makeform.h"
#include "../makegen.h"
#include "../tllex.h"
#include "../tlparse.h"
#include "../tl_tab.h"
#include "../util.h"
#include "../world.h"
#include "../zone.h"

/* local data */

static CELLP pcStack;				/* current stack */
static CELLP pcStackEnd=(CELLP)&pcStack;
static CELLP pcStacks;				/* accumulated stacks */
static CELLP pcStacksEnd=(CELLP)&pcStacks;
static int nSeed;					/* game number */

static char *psSuit[]={"c","d","h","s"};
static char *psValue[]={"A","2","3","4","5","6","7","8","9","10","J","Q","K"};

/* local function prototypes */

static void EncodeCard
(
	int nIndex,							/* card index */
	char *pcBuffer						/* output card name buffer */
);

/* RandomFreeCellGame

Description:
	Random game generator for freecell.
Notes:
	This routine will generate the same games as Microsoft's freecell game, when
	called with the game number as a seed (and 52 cards and 8 stacks).
	Parameters are silently clamped to valid values:

	(random-freecell-game <seed> <cards>)

	seed -- [0,32767]
	maxcard -- [1,13]

Card enumeration used in cards.dll.
  
	C   D   H   S

A   0   1   2   3
2   4   5   6   7
3   8   9   10  11
4   12  13  14  15
5   16  17  18  19
6   20  21  22  23
7   24  25  26  27
8   28  29  30  31
9   32  33  34  35
10  36  37  38  39
J   40  41  42  43
Q   44  45  46  47
K   48  49  50  51

*/

DECLSPECX CELLP RandomFreeCellGame
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc,pc1;
	CELLP pcStart,pcEnd;
	char acBuffer[80];
	char acCard[4];						/* card name */
	char *ps;
	int array[52];						/* card array */
	int anTable[8][13];					/* game array */
	int *panTable[8];					/* game pointer array */
	int i,j,k,l,m;
	int nMaxCard;						/* number of cards per suit */
	int n;

	/* get the game number */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&n))
		return NULL;					/* message already printed */
	if(n!=-1)							/* default game number if n==-1 */
	{
		nSeed=n;
		if(nSeed<0)
			nSeed=0;
		else if(nSeed>32767)
			nSeed=32767;
	}

	/* fill in the plan name and plan file */
	
	sprintf(acBuffer,"Game %d",nSeed);
	psPlanName=StrAlloc(acBuffer);

	/* get the number of cards */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMaxCard))
		return NULL;					/* message already printed */
	if(nMaxCard<1)
		nMaxCard=1;
	else if(nMaxCard>13)
		nMaxCard=13;

	/* initialize arrays */

	for(i=0;i<52;i++)
		array[i]=i;
	for(i=0;i<8;i++)
	{
		panTable[i]=anTable[i];
		for(j=0;j<13;j++)
			anTable[i][j]=-1;
	}

	/* fill in the table array */
		
	i=52;
	for(m=0;;m++)						/* for all stack heights */
	{
		for(l=0;l<8;l++)				/* for all stacks */
		{
			nSeed=nSeed*214013+2531011;	/* 0x00034DFD & 0x00269EC3 */
			j=((nSeed>>16)&0x00007FFF)%i;
			k=array[j];
			array[j]=array[i-1];
			if(k/4<nMaxCard)
				*panTable[l]++=k;
			i--;
			if(i==0)
				goto MakeLiterals;
		}
	}

MakeLiterals:

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	ps=acCard;							/* keep ansi happy */

	for(i=0;i<8;i++)
	{
		for(j=0;j<13;j++)
		{
			if(anTable[i][j]==-1)		/* if no more cards in this stack */
				break;
			if(j==0)
			{
				EncodeCard(anTable[i][j],acCard);
				pc=MakeLiteralForm(ATOM_IDENT,*(LITVALP)&ps);
				pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"bottomcol"),
					pc);
			}
			if(anTable[i][j+1]==-1)
			{
				EncodeCard(anTable[i][j],acCard);
				pc=MakeLiteralForm(ATOM_IDENT,*(LITVALP)&ps);
				pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"clear"),
					pc);
			}
			else
			{
				EncodeCard(anTable[i][j+1],acCard);
				pc=MakeLiteralForm(ATOM_IDENT,*(LITVALP)&ps);
				EncodeCard(anTable[i][j],acCard);
				pc1=MakeLiteralForm(ATOM_IDENT,*(LITVALP)&ps);
				pc->pcNext=pc1;
				pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"on"),
					pc);
			}
		}
	}
	j=4*(nMaxCard-1);
	for(i=0;i<4;i++)
	{
		EncodeCard(j++,acCard);
		pc=MakeLiteralForm(ATOM_IDENT,*(LITVALP)&ps);
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"maxcard"),pc);
	}
		
	return pcStart;
}

/* EncodeCard

Description:
	Convert a card index into a card name.
*/

static void EncodeCard
(
	int nIndex,							/* card index */
	char *pcBuffer						/* output card name buffer */
)
{
	sprintf(pcBuffer,"%s%s",psSuit[nIndex%4],psValue[nIndex/4]);
}	

/* Print Routines -------------------------------------------------------------

	To print a world we evaluate the formula:  print-freecell-world

	(and
		 (clear-accumulators)
		 (forall (?card) (bottomcol ?card) (accumulate-stack ?card))
		 (print-stacks)

	When we evaluate clear-accumulators this sets the global variables 
	(i.e., static variables) *stacks* and *stack* to the empty lists)
	Then starting at the bottom, for every bottomcol card, we
	evaluate accumulate-stack.
	Accumulate-stack is a recursive predicate, that during
	its evaluatation pushes each card on to the current stack, stored
	in the *stack* variable. At the end of the accumulation (when
	we hit a clear stack, we evaluate (end-stack) which simply
	stores the accumulated stack in the list of stacks *stack*.
	Finally, we evalute print-stack which invokes a function that
	computes the maximum height stack, and then prints out the stack
	in a series of rows.
*/

/* StackCard (Pseudo predicate)

Description:
	Add a card to a stack
*/

DECLSPECX BOOL StackCard
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pc)
		TermError("push-card",pcFormula,pbBindings);
	pc=CopyCell(pc);
	pcStackEnd=pcStackEnd->pcNext=pc;
	return TRUE;
}

/* EndStack (Pseudo Predicate)

Description:
	The current stack is done... add it to the list of stacks
*/

DECLSPECX BOOL EndStack(void)
{
	int i;
	CELLP pc;

	if(pcStack)
	{
		/* count up the cards in the stack, and put the count in an integer
		formula.  Use the (normally unused) argument list of the integer formula
		to point to the stack. */

		i=0;
		for(pc=pcStack;pc;pc=pc->pcNext)
			i++;
		pc=MakeIntegerForm(i);
		pc->pfForm->pcArgs=pcStack;
		pcStack=NULL;
		pcStackEnd=(CELLP)&pcStack;

		pcStacksEnd=pcStacksEnd->pcNext=pc;
	}
	return TRUE;
}

/* PrintStacks (Pseudo Predicate)

Description:
	Print all card stacks
*/

DECLSPECX BOOL PrintStacks
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nMaxHeight;
	int i;
	CELLP pc;
	CELLP pcCard;
	int nFile;
	FILE *pfFile;

	FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile);
	pfFile=HandleToFileStream(nFile);

	/* calculate the maximum height of the stacks */

	nMaxHeight=0;
	for(pc=pcStacks;pc;pc=pc->pcNext)
	{
		if(pc->pfForm->uValue.nInteger>nMaxHeight)
			nMaxHeight=pc->pfForm->uValue.nInteger;
	}

	/* scan across stacks, printing blanks or cards */

	for(i=0;i<nMaxHeight;i++)
	{
		CommandPrintf(pfFile,"   ");
		for(pc=pcStacks;pc;pc=pc->pcNext)
		{
			if(pc->pfForm->uValue.nInteger>0)
			{
				pc->pfForm->uValue.nInteger--;
				pcCard=pc->pfForm->pcArgs;
				CommandPrintf(pfFile," %-3s",IdentName(pcCard->pfForm));
				pc->pfForm->pcArgs=pcCard->pcNext;
				pcCard->pcNext=NULL;
			}
			else
				CommandPrintf(pfFile,"    ");
		}
		CommandPrintf(pfFile,"\n");
	}
	CommandPrintf(pfFile,"\n");
	if(pcStacks)
	{
		pcStacks=NULL;
		pcStacksEnd=(CELLP)&pcStacks;
	}
	return TRUE;
}

