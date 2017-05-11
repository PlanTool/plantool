/* lloyd.c

The 8 tile board (this one's the oddball):

	S1 S2 S3
	S8 S0 S4
	S7 S6 S5

The 15 tile board:

	S1  S2  S3  S4
	S5  S6  S7  S8
	S9  S10 S11 S12
	S13 S14 S15 S16

The 24 tile board:

	S1  S2  S3  S4  S5
	S6  S7  S8  S9  S10
	S11 S12 S13 S14 S15
	S16 S17 S18 S19 S20
	S21 S22 S23 S24 S25

The 35 tile board:

	S1  S2  S3  S4  S5  S6
	S7  S8  S9  S10 S11 S12
	S13 S14 S15 S16 S17 S18
	S19 S20 S21 S22 S23 S24
	S25 S26 S27 S28 S29 S30
	S31 S32 S33 S34 S35 S36
*/

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/* Serpentine encoding of board positions */

char *apsEight[9]=
{
	"S1","S2","S3",
	"S4","S0","S8",
	"S7","S6","S5"
};
char *apsFifteen[16]=
{
	"S1","S2","S3","S4",
	"S8","S7","S6","S5",
	"S9","S10","S11","S12",
	"S16","S15","S14","S13"
};
char *apsTwentyFour[25]=
{
	"S1","S2","S3","S4","S5",
	"S10","S9","S8","S7","S6",
	"S11","S12","S13","S14","S15",
	"S20","S19","S18","S17","S16",
	"S21","S22","S23","S24","S25"
};
char *apsThirtyFive[36]=
{
	"S1","S2","S3","S4","S5","S6",
	"S12","S11","S10","S9","S8","S7",
	"S13","S14","S15","S16","S17","S18",
	"S24","S23","S22","S21","S20","S19",
	"S25","S26","S27","S28","S29","S30",
	"S36","S35","S34","S33","S32","S31"
};

/* RandomTilePuzzle

Description:
	Random world generator for Lloyd's tile puzzles.
Notes:
	Parameters are silently clamped to valid values:

	(random-tile-puzzle <seed> <tiles> <parity>)

	seed -- [0,32767]
	tiles -- [8,15,24,35]
	parity -- [0,1]

	We generate a random permutation of the board, starting from a "start-goal".
	We count parity changes as we permute the board.  If the resulting configuration
	doesn't have the desired parity, we swap the pieces in array locations 0 and 2.
  
	We convert the playing board to a linear array by enumerating a serpentine
	path.  The serpentine path preserves the parity of the manhatten metric.  So
	we can use a simple difference in positions in the linear array to calculate
	the number of parity changes.

	The parity rules are:

	1.  If we swap a tile or blank with itself, the parity remains the same.
	2.  If we swap a tile with another tile, the parity changes.
	3.  If we swap a tile and the blank, the parity changes if the manhatten distance
	between the positions is even.
*/

DECLSPECX CELLP RandomTilePuzzle
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc,pc1,pc2;					/* formula pointers */
	CELLP pcStart,pcEnd;				/* literal list head */
	char acBuffer[128];					/* string buffer */
	int i,j;							/* array indices */
	int nSwap;							/* swap buffer */
	int nSeed;							/* world number */
	int nTiles;							/* number of tiles */
	int nParity;						/* parity */
	int nCount;							/* number of parity changes */
	int anTiles[36];					/* tile array */
	LITVAL lv;							/* literal value */
	char **pps;							/* string array pointer */

	/* get the world number */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nSeed))
		return NULL;					/* message already printed */
	if(nSeed<0)
		nSeed=0;
	else if(nSeed>32767)
		nSeed=32767;

	/* get the number of tiles */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nTiles))
		return NULL;					/* message already printed */
	if(nTiles!=8&&nTiles!=15&&nTiles!=24&&nTiles!=35)
		nTiles=8;

	/* get the parity */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nParity))
		return NULL;					/* message already printed */
	if(nParity!=0&&nParity!=1)
		nParity=0;

	/* fill in the plan name */
	
	sprintf(acBuffer,"%d Tile Puzzle #%d (%s Parity)",
		nTiles,nSeed,nParity?"Odd":"Even");
	psPlanName=StrAlloc(acBuffer);

	/* generate the random puzzle */

	for(i=0;i<=nTiles;i++)				/* fill in the tile array */
		anTiles[i]=i;
	
	nCount=0;
	for(i=nTiles;i>0;i--)				/* permute the tile array */
	{
		nSeed=nSeed*214013+2531011;		/* 0x00034DFD & 0x00269EC3 */
		j=((nSeed>>16)&0x00007FFF)%(i+1);
		if(i!=j)
		{
			nSwap=anTiles[i];
			anTiles[i]=anTiles[j];
			anTiles[j]=nSwap;
			if(anTiles[i]&&anTiles[j])	/* if neither tile is blank */
				nCount++;
			else if(!((i-j)&1))			/* if manhatten distance is even */
				nCount++;
		}
	}

	if((nCount&1)!=nParity)				/* make sure the parity is correct */
	{
		nSwap=anTiles[0];
		anTiles[0]=anTiles[2];
		anTiles[2]=nSwap;
	}
	
	/* generate the literals */

	pcStart=NULL;						/* initialize literal list head */
	pcEnd=(CELLP)&pcStart;

	switch(nTiles)
	{
		case 8:
			pps=apsEight;
			break;
		case 15:
			pps=apsFifteen;
			break;
		case 24:
			pps=apsTwentyFour;
			break;
		case 35:
			pps=apsThirtyFive;
			break;
	}
	
	for(i=0;i<=nTiles;i++)
	{
		if(anTiles[i])
			sprintf(acBuffer,"P%d",anTiles[i]);
		else
			strcpy(acBuffer,"B");
		lv.psString=acBuffer;
		pc1=MakeLiteralForm(ATOM_IDENT,lv);
		lv.psString=pps[i];
		pc2=MakeLiteralForm(ATOM_IDENT,lv);
		pc1->pcNext=pc2;

		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"at"),
			pc1);
	}
	return pcStart;
}

