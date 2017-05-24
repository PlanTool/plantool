/* rubik.c

Description:
	Support routines for rubik's world.
*/

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

/* local definitions */

#define ABS(x) ((x)>0?(x):-(x))

#define X 0
#define Y 1
#define Z 2

typedef struct Context					/* random-sequence context */
{
	int nCount;							/* number of values left to generate */
	int nOffset;						/* lowest value to generate */
	int nScale;							/* range of values to generate */
}CONTEX, *CONTEXP;

/* local function prototypes */

static void BelongsAt
(
	char *psName,						/* the cubette's label */
	int *x,								/* the cubette's home position */
	int *y,
	int *z
);
static int CubetteParity
(
	int i,								/* current cubette position */
	int j,
	int k
);
static int MiddleDistance
(
	int i,								/* current cubette position */
	int j,
	int k
);
static int CornerDistance
(
	int i,								/* current cubette position */
	int j,
	int k
);


/* local data */

static char *psCubette[3][3][3];		/* cubette accumulator */
static int nX,nY,nZ;					/* accumulator indices */

/* Cubette rotations -------------------------------------------------------- */

DECLSPECX CELLP RotateX
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char *ps;
	char cTemp;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pc)
		TermError("rotate-x",pcFormula,pbBindings);
//	pc=CopyFormula(pc);					/* don't modify btree insitu */
	ps=IdentName(pc->pfForm);
	cTemp=ps[1];
	ps[1]=ps[2];
	ps[2]=cTemp;
	return pc;
}

DECLSPECX CELLP RotateY
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char *ps;
	char cTemp;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pc)
		TermError("rotate-y",pcFormula,pbBindings);
//	pc=CopyFormula(pc);					/* don't modify btree insitu */
	ps=IdentName(pc->pfForm);
	cTemp=ps[0];
	ps[0]=ps[2];
	ps[2]=cTemp;
	return pc;
}

DECLSPECX CELLP RotateZ
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char *ps;
	char cTemp;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pc)
		TermError("rotate-z",pcFormula,pbBindings);
//	pc=CopyFormula(pc);					/* don't modify btree insitu */
	ps=IdentName(pc->pfForm);
	cTemp=ps[0];
	ps[0]=ps[1];
	ps[1]=cTemp;
	return pc;
}

/* PrintCube (Pseudo Predicate) ------------------------------------------------

Description:
	Print an exploded view of the cube.
*/

DECLSPECX BOOL PrintCube
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i,j,k;
	FILE *pfStream;
	int nFile;

	FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile);
	pfStream=HandleToFileStream(nFile);

	for(k=2;k>=0;k--)					/* top */
	{
		CommandPrintf(pfStream,"        ");
		for(i=0;i<3;i++)
			CommandPrintf(pfStream,"%c ",psCubette[i][2][k][Y]);
		CommandPrintf(pfStream,"\n");
	}
	CommandPrintf(pfStream,"\n");
	for(j=2;j>=0;j--)
	{
		for(k=2;k>=0;k--)				/* left */
			CommandPrintf(pfStream,"%c ",psCubette[0][j][k][X]);
		CommandPrintf(pfStream,"  ");
		for(i=0;i<3;i++)				/* front */
			CommandPrintf(pfStream,"%c ",psCubette[i][j][0][Z]);
		CommandPrintf(pfStream,"  ");
		for(k=0;k<3;k++)				/* right */
			CommandPrintf(pfStream,"%c ",psCubette[2][j][k][X]);
		CommandPrintf(pfStream,"  ");
		for(i=2;i>=0;i--)				/* back */
			CommandPrintf(pfStream,"%c ",psCubette[i][j][2][Z]);
		CommandPrintf(pfStream,"\n");
	}
	CommandPrintf(pfStream,"\n");
	for(k=0;k<3;k++)					/* bottom */
	{
		CommandPrintf(pfStream,"        ");
		for(i=0;i<3;i++)
			CommandPrintf(pfStream,"%c ",psCubette[i][0][k][Y]);
		CommandPrintf(pfStream,"\n");
	}
	CommandPrintf(pfStream,"\n");
	return TRUE;
}
		
/* AccumulateCubettes (Pseudo Predicate)

Description:
	Save an array of cubette colors.  Since the cubettes are enumerated
	in alphabetical order, we can assign color strings to the correct cubette,
	without being told its name.
*/

DECLSPECX BOOL AccumulateCubettes
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	pc=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pc)
		TermError("accumulate-cubettes",pcFormula,pbBindings);
	psCubette[nX][nY][nZ]=StrAlloc(IdentName(pc->pfForm));
	if(++nZ>=3)
	{
		nZ=0;
		if(++nY>=3)
		{
			nY=0;
			if(++nX>=3)
				nX=nY=nZ=0;
		}
	}
	return TRUE;
}

/* RandomSequence (Generator) --------------------------------------------------

Description:
	(random-sequence ?x ?n ?min ?max) generates ?n random numbers between
	?min and ?max (inclusive).
*/

DECLSPECX BOOL RandomSequence
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CONTEXP pcContext;					/* context pointer */
	CELLP pc;	 						/* pointer to literal */
	int nCount;							/* count of numbers to generate */
	int nMin,nMax;						/* limits */

	/* initialization */

	if(!*ppvContext)
	{
		pc=pcGenLit->pfForm->pcArgs;
		if(!pc)
		{
			ErrorMessage("random-sequence:  No arguments\n");
			return FALSE;
		}
		pc=pc->pcNext;					/* skip over binding variable */
		if(!pc)
		{
			ErrorMessage("random-sequence:  No count\n");
			return FALSE;
		}
		if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nCount))
			return FALSE;				/* message already printed */
		pc=pc->pcNext;
		if(!pc)
		{
			ErrorMessage("random-sequence:  No limits\n");
			return FALSE;
		}
		if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMin))
			return FALSE;				/* message already printed */
		pc=pc->pcNext;
		if(!pc)
		{
			ErrorMessage("random-sequence:  No maximum limit\n");
			return FALSE;
		}
		if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMax))
			return FALSE;				/* message already printed */
		if(nMax<=nMin)
		{
			ErrorMessage("random-sequence:  Maximum limit must be greater than minimum limit\n");
			return FALSE;
		}
		pcContext=(CONTEXP)MemAlloc(sizeof(CONTEX));
		*ppvContext=(void *)pcContext;
		pcContext->nCount=nCount;
		pcContext->nOffset=nMin;
		pcContext->nScale=nMax-nMin+1;
	}

	/* iteration */

	pcContext=(CONTEXP)*ppvContext;
	if(pcContext->nCount--<=0)
	{
//		ZoneFree(pcContext);
		return FALSE;
	}
	lv.nInteger=FibRand()%pcContext->nScale+pcContext->nOffset;	/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* Heuristic Support -------------------------------------------------------- */

/* CubeDistance

Description:
	Calculate the sum of the distances of each cubette from its home position.
*/

DECLSPECX CELLP CubeDistance
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nDistance;

	nDistance=CornerDistance(0,0,0);
	nDistance+=CornerDistance(0,0,2);
	nDistance+=CornerDistance(0,2,0);
	nDistance+=CornerDistance(0,2,2);
	nDistance+=CornerDistance(2,0,0);
	nDistance+=CornerDistance(2,0,2);
	nDistance+=CornerDistance(2,2,0);
	nDistance+=CornerDistance(2,2,2);
	nDistance+=MiddleDistance(0,1,0);
	nDistance+=MiddleDistance(0,1,2);
	nDistance+=MiddleDistance(0,0,1);
	nDistance+=MiddleDistance(0,2,1);
	nDistance+=MiddleDistance(1,0,0);
	nDistance+=MiddleDistance(1,2,0);
	nDistance+=MiddleDistance(1,0,2);
	nDistance+=MiddleDistance(1,2,2);
	nDistance+=MiddleDistance(2,1,0);
	nDistance+=MiddleDistance(2,1,2);
	nDistance+=MiddleDistance(2,0,1);
	nDistance+=MiddleDistance(2,2,1);
	return MakeIntegerForm(nDistance);
}

/* CubeParity

Description:
	Calculate the sum of the parity changes required to return each cubette 
	to its home orientation.
*/

DECLSPECX CELLP CubeParity
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nParity;

	nParity=CubetteParity(0,0,0);
	nParity+=CubetteParity(0,0,2);
	nParity+=CubetteParity(0,2,0);
	nParity+=CubetteParity(0,2,2);
	nParity+=CubetteParity(2,0,0);
	nParity+=CubetteParity(2,0,2);
	nParity+=CubetteParity(2,2,0);
	nParity+=CubetteParity(2,2,2);
	nParity+=CubetteParity(0,1,0);
	nParity+=CubetteParity(0,1,2);
	nParity+=CubetteParity(0,0,1);
	nParity+=CubetteParity(0,2,1);
	nParity+=CubetteParity(1,0,0);
	nParity+=CubetteParity(1,2,0);
	nParity+=CubetteParity(1,0,2);
	nParity+=CubetteParity(1,2,2);
	nParity+=CubetteParity(2,1,0);
	nParity+=CubetteParity(2,1,2);
	nParity+=CubetteParity(2,0,1);
	nParity+=CubetteParity(2,2,1);
	return MakeIntegerForm(nParity);
}
	
/* CornerDistance (Local function)

Description:
	Calculate the distance (number of moves) between a corner cubette
	and its home location.  This is simply a count of the number of 
	unequal coordinates.
Notes:
	We use an array to simplify this calculation.  We calculate the
	absolute difference between the cubette position and its home position.  
	This maps the cube into a "difference" space.  In the difference space,
	the distance is the number of non-zero coordinates.
*/

static int nCornerDistance[3][3][3]=
{
	{{0,1,1},{1,2,2},{1,2,2}},
	{{1,2,2},{2,3,3},{2,3,3}},
	{{1,2,2},{2,3,3},{2,3,3}}
};

static int CornerDistance
(
	int i,								/* current cubette position */
	int j,
	int k
)
{
	int x,y,z;							/* cubette home position */

	BelongsAt(psCubette[i][j][k],&x,&y,&z);
	x=ABS(x-i);
	y=ABS(y-j);
	z=ABS(z-k);
	return nCornerDistance[x][y][z];
}

/* MiddleDistance (Local function)

Description:
	Calculate the distance (number of moves) between a middle cubette
	and its home location.
Notes:
	We use an array to simplify this calculation.  We calculate the
	absolute difference between the cubette position and its home position.  
	This maps the cube into a "difference" space.  If 3 coordinates are 2,
	that's impossible, and we mark that with a distance of 4.  If 2 coordinates 
	are 2, then the distance is 3.  If only one coordinate is 2, the distance
	is 2.  If all coordinates are 0, the distance is 0.  The remaining coordinates
	are all at a distance of 1.
*/

static int nMiddleDistance[3][3][3]=
{
	{{0,1,2},{1,1,2},{2,2,3}},
	{{1,1,2},{1,1,2},{2,2,3}},
	{{2,2,3},{2,2,3},{3,3,4}}
};

static int MiddleDistance
(
	int i,								/* current cubette position */
	int j,
	int k
)
{
	int x,y,z;							/* cubette home position */

	BelongsAt(psCubette[i][j][k],&x,&y,&z);
	x=ABS(x-i);
	y=ABS(y-j);
	z=ABS(z-k);
	return nMiddleDistance[x][y][z];
}

/* CubetteParity

Description:
	Calculate the orientation parity count for a cubette.
Notes:
	The minimum number of parity operations required to properly orient
	a cubette is 0, 1 or 2.  We calculate this number by counting the
	number of differences between the cubette's current label and its
	home label.
*/

static char *nCubeColors[3]=
{
	"rxo",
	"yxw",
	"gxb"
};

static int CubetteParity
(
	int i,								/* current cubette position */
	int j,
	int k
)
{
	char acColors[4];					/* cubette home label */
	char *psName;						/* current cubette label */
	int nParity;						/* parity count */
	int x,y,z;							/* cubette home position */

	/* regenerate the home label */
	
	BelongsAt(psCubette[i][j][k],&x,&y,&z);
	acColors[0]=nCubeColors[0][x];
	acColors[1]=nCubeColors[1][y];
	acColors[2]=nCubeColors[2][z];
	acColors[3]=0;

	/* if the parity is non-zero, it's one less than the number of differences. */

	psName=psCubette[i][j][k];
	nParity=0;
	if(acColors[0]!=psName[0])
		nParity++;
	if(acColors[1]!=psName[1])
		nParity++;
	if(acColors[2]!=psName[2])
		nParity++;
	if(nParity)
		nParity--;
	else if(i!=x||j!=y||k!=z)			/* if parity is 0, but we're not home */
		nParity=2;
	return nParity;
}

/* BelongsAt (Local Function)

Description:
	Determine the home location of a cubette, given its label.
*/

static void BelongsAt
(
	char *psName,						/* the cubette's label */
	int *x,								/* the cubette's home position */
	int *y,
	int *z
)
{
	int i;

	*x=*y=*z=1;
	for(i=0;i<3;i++)
	{
		switch(tolower(psName[i]))
		{
			case 'r':
				*x=0;
				break;
			case 'o':
				*x=2;
				break;
			case 'y':
				*y=0;
				break;
			case 'w':
				*y=2;
				break;
			case 'g':
				*z=0;
				break;
			case 'b':
				*z=2;
				break;
		}
	}
}




