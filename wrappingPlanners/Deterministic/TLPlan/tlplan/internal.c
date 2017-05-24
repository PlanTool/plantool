/* internal.c

Copyright C, 1997 - 99  F. Bacchus

Routines to evaluate internal functions on worlds.
--------------------------------------------------------------------------------
*/

#include <assert.h>
#include <float.h>
#include <math.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "btree.h"
#include "compute.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "internal.h"
#include "isaac.h"
#include "makeform.h"
#include "makegen.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "zone.h"

/* local structure definitions */

// heap macros

#define Parent(i) ((i)>>1)
#define Left(i) ((i)<<1)
#define Right(i) (((i)<<1)+1)


/* global data */

//double dfGeneratorTime;


/* local function prototypes */

static void MarkNearestFirst
(
	NFCONTEXTP pgc						// pointer to context block 
);
static void MarkNearestFirstEx
(
	NFXCONTEXTP pgc						// pointer to context block 
);
static BOOL InsertVertex
(
	CELLP pcVertex,						// vertex to insert
	int *pnVertexIndices,				// vertex index array (name sorted order)
	CELLP *ppcVertices,					// vertex pointer array (allocation order)
	int *pnCount,						// number of vertices in array
	int *pnIndex						// returned index of vertex
);
static void MarkClosestFirst
(
	CFCONTEXTP pgc						// pointer to context block 
);
static void VHeapInsert
(
	CFCONTEXTP pgc,						// context pointer
	int nVertex							// vertex allocation index to insert into heap 
);
static int VHeapExtractMin
(
	CFCONTEXTP pgc						// context pointer
);
static void VHeapify
(
	CFCONTEXTP pgc,						// context pointer
	int nVertex							// vertex to put in order
);
static void VHeapPercolate
(
	CFCONTEXTP pgc,						// context pointer
	int nNode							// node to put in order
);
static int VHeapCompareFn
(
	CFCONTEXTP pgc,						// context pointer
	int nVertex1,
	int nVertex2
);
static void MarkClosestFirstEx
(
	CFXCONTEXTP pgc						// pointer to context block 
);
static void VHeapInsertEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nVertex							// vertex allocation index to insert into heap 
);
static int VHeapExtractMinEx
(
	CFXCONTEXTP pgc						// context pointer
);
static void VHeapifyEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nVertex							// vertex to put in order
);
static void VHeapPercolateEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nNode							// node to put in order
);
static int VHeapCompareFnEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nVertex1,
	int nVertex2
);
static int LFCompare
(
	BTREEP *ppbt1,						/* btree node to compare */
	BTREEP *ppbt2						/* btree node to compare */
);
static void MarkLowestFirst
(
	LFCONTEXTP pgc						// pointer to context block 
);
static double TermToDouble
(
	CELLP pcValue
);
static void MarkAllPairsShortestPath
(
	APSPCONTEXTP pgc						// pointer to context block 
);

/* Arithmetic Routines --------------------------------------------------------- */

/* ComputePlus

Description:
	Add zero or more numbers.
*/

CELLP ComputePlus
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfSum;
	BOOL bInteger;
	int nSum;

	ENTER("ComputePlus",TRUE);
	bInteger=TRUE;
	dfSum=0.0;
	nSum=0;

	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-plus",pcFormula,pbBindings);
		if(bInteger)
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					nSum+=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfSum=nSum+pcValue->pfForm->uValue.dfFloat;
					bInteger=FALSE;
					break;
				default:
					ErrorMessage("ComputePlus:  Invalid or non-numeric argument\n");
					pc=MakeIntegerForm(0);
					EXIT("ComputePlus");
					return pc;
			}
		}
		else
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					dfSum+=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfSum+=pcValue->pfForm->uValue.dfFloat;
					break;
				default:
					ErrorMessage("ComputePlus:  Invalid or non-numeric argument\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputePlus");
					return pc;
			}
		}
	}
	if(bInteger)
		pc=MakeIntegerForm(nSum);
	else
		pc=MakeFloatForm(dfSum);
	EXIT("ComputePlus");
	return pc;
}

/* ComputeMinus

Description:
	Subtract zero or more numbers.
*/

CELLP ComputeMinus
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfDifference;
	BOOL bInteger;
	int nDifference;

	ENTER("ComputeMinus",TRUE);
	bInteger=TRUE;
	nDifference=0;
	dfDifference=0.0;

	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeIntegerForm(0);
		EXIT("ComputeMinus");
		return pc;
	}
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-minus",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			nDifference=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfDifference=pcValue->pfForm->uValue.dfFloat;
			bInteger=FALSE;
			break;
		default:
			ErrorMessage("ComputeMinus:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeMinus");
			return pc;
	}
	if(!pc->pcNext)
	{
		if(bInteger)
			pc=MakeIntegerForm(-nDifference);
		else
			pc=MakeFloatForm(-dfDifference);
		EXIT("ComputeMinus");
		return pc;
	}

	for(pc=pc->pcNext;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-minus",pcFormula,pbBindings);
		if(bInteger)
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					nDifference-=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfDifference=nDifference-pcValue->pfForm->uValue.dfFloat;
					bInteger=FALSE;
					break;
				default:
					ErrorMessage("ComputeMinus:  Invalid or non-numeric argument\n");
					pc=MakeIntegerForm(0);
					EXIT("ComputeMinus");
					return pc;
			}
		}
		else
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					dfDifference-=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfDifference-=pcValue->pfForm->uValue.dfFloat;
					break;
				default:
					ErrorMessage("ComputeMinus:  Invalid or non-numeric argument\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputeMinus");
					return pc;
			}
		}
	}
	if(bInteger)
		pc=MakeIntegerForm(nDifference);
	else
		pc=MakeFloatForm(dfDifference);
	EXIT("ComputeMinus");
	return pc;
}

/* ComputeMultiply

Description:
	Multiply zero or more numbers.
*/

CELLP ComputeMultiply
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	BOOL bInteger;
	int nProduct;
	double dfProduct;

	ENTER("ComputeMultiply",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeIntegerForm(0);
		EXIT("ComputeMultiply");
		return pc;
	}

	bInteger=TRUE;
	nProduct=1;
	dfProduct=1.0;

	for(;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-multiply",pcFormula,pbBindings);
		if(bInteger)
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					nProduct*=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfProduct=nProduct*pcValue->pfForm->uValue.dfFloat;
					bInteger=FALSE;
					break;
				default:
					ErrorMessage("ComputeMultiply:  Invalid or non-numeric argument\n");
					pc=MakeIntegerForm(0);
					EXIT("ComputeMultiply");
					return pc;
			}
		}
		else
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					dfProduct*=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfProduct*=pcValue->pfForm->uValue.dfFloat;
					break;
				default:
					ErrorMessage("ComputeMultiply:  Invalid or non-numeric argument\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputeMultiply");
					return pc;
			}
		}
	}
	if(bInteger)
		pc=MakeIntegerForm(nProduct);
	else
		pc=MakeFloatForm(dfProduct);
	EXIT("ComputeMultiply");
	return pc;
}

/* ComputeDivide

Description:
	Divide zero or more numbers.
*/

CELLP ComputeDivide
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	BOOL bInteger;
	int nQuotient,nDivisor;
	double dfQuotient,dfDivisor;

	ENTER("ComputeDivide",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeIntegerForm(0);
		EXIT("ComputeDivide");
		return pc;
	}

	bInteger=TRUE;
	nQuotient=1;
	dfQuotient=1.0;
	nDivisor=1;
	dfDivisor=1.0;

	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-divide",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			nQuotient=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfQuotient=pcValue->pfForm->uValue.dfFloat;
			bInteger=FALSE;
			break;
		default:
			ErrorMessage("ComputeDivide:  Invalid or non-numeric argument\n");
			pc=MakeIntegerForm(0);
			EXIT("ComputeDivide");
			return pc;
	}
	if(!pc->pcNext)
	{
		if((bInteger&&nQuotient==0)||
			(!bInteger&&dfQuotient==0.0))
		{
			ErrorMessage("ComputeDivide:  Division by zero\n");
			if(bInteger)
				pc=MakeIntegerForm(0);
			else
				pc=MakeFloatForm(0.0);
			EXIT("ComputeDivide");
			return pc;
		}
		if(bInteger)
			pc=MakeIntegerForm(1/nQuotient);
		else
			pc=MakeFloatForm(1.0/dfQuotient);
		EXIT("ComputeDivide");
		return pc;
	}

	for(pc=pc->pcNext;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-divide",pcFormula,pbBindings);
		if(bInteger)
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					nDivisor=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfDivisor=pcValue->pfForm->uValue.dfFloat;
					dfQuotient=nQuotient;
					bInteger=FALSE;
					break;
				default:
					ErrorMessage("ComputeDivide:  Invalid or non-numeric argument\n");
					pc=MakeIntegerForm(0);
					EXIT("ComputeDivide");
					return pc;
			}
			if((bInteger&&nDivisor==0)||
				(!bInteger&&dfDivisor==0.0))
			{
				ErrorMessage("ComputeDivide:  Division by zero\n");
				if(bInteger)
					pc=MakeIntegerForm(0);
				else
					pc=MakeFloatForm(0.0);
				EXIT("ComputeDivide");
				return pc;
			}
			if(bInteger)
				nQuotient/=nDivisor;
			else
				dfQuotient/=dfDivisor;
		}
		else
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					dfDivisor=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfDivisor=pcValue->pfForm->uValue.dfFloat;
					break;
				default:
					ErrorMessage("ComputeDivide:  Invalid or non-numeric argument\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputeDivide");
					return pc;
			}
			if(dfDivisor==0.0)
			{
				ErrorMessage("ComputeDivide:  Division by zero\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeDivide");
				return pc;
			}
			dfQuotient/=dfDivisor;
		}
	}
	if(bInteger)
		pc=MakeIntegerForm(nQuotient);
	else
		pc=MakeFloatForm(dfQuotient);
	EXIT("ComputeDivide");
	return pc;
}

/* ComputeMod

Description:
	Remainder one or more numbers.
*/

CELLP ComputeMod
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfRemainder,dfDivisor;

	ENTER("ComputeMod",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeMod");
		return pc;
	}
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-mod",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			dfRemainder=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfRemainder=pcValue->pfForm->uValue.dfFloat;
			break;
		default:
			ErrorMessage("ComputeMod:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeMod");
			return pc;
	}

	for(pc=pc->pcNext;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-mod",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				dfDivisor=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				dfDivisor=pcValue->pfForm->uValue.dfFloat;
				break;
			default:
				ErrorMessage("ComputeMod:  Invalid or non-numeric argument\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeMod");
				return pc;
		}
		if(dfDivisor==0.0)
		{
			ErrorMessage("ComputeMod:  Division by zero\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeMod");
			return pc;
		}
		dfRemainder=fmod(dfRemainder,dfDivisor);
	}
	pc=MakeFloatForm(dfRemainder);
	EXIT("ComputeMod");
	return pc;
}

/* ComputeMax

Description:
	Find the maximum of zero or more numbers.
*/

CELLP ComputeMax
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfMax;

	ENTER("ComputeMax",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeMax");
		return pc;
	}
	dfMax=-DBL_MAX;
	for(;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-max",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				if(dfMax<pcValue->pfForm->uValue.nInteger)
					dfMax=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				if(dfMax<pcValue->pfForm->uValue.dfFloat)
					dfMax=pcValue->pfForm->uValue.dfFloat;
				break;
			default:
				ErrorMessage("ComputeMax:  Invalid or non-numeric argument\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeMax");
				return pc;
		}
	}
	pc=MakeFloatForm(dfMax);
	EXIT("ComputeMax");
	return pc;
}

/* ComputeMin

Description:
	Find the minimum of zero or more numbers.
*/

CELLP ComputeMin
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfMin;

	ENTER("ComputeMin",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeMin");
		return pc;
	}
	dfMin=DBL_MAX;
	for(;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-min",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				if(dfMin>pcValue->pfForm->uValue.nInteger)
					dfMin=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				if(dfMin>pcValue->pfForm->uValue.dfFloat)
					dfMin=pcValue->pfForm->uValue.dfFloat;
				break;
			default:
				ErrorMessage("ComputeMin:  Invalid or non-numeric argument\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeMin");
				return pc;
		}
	}
	pc=MakeFloatForm(dfMin);
	EXIT("ComputeMin");
	return pc;
}

CELLP ComputeConditionalExp
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	
	ENTER("ComputeConditionlExp",TRUE);

	pc=pcFormula->pfForm->pcArgs;

	if((*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
	    pc=pc->pcNext;
	else 
	    pc=pc->pcNext->pcNext;

	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);

	switch(pcValue->pfForm->nType)
	{
	    case ATOM_INTEGER:
		pc=MakeIntegerForm(pcValue->pfForm->uValue.nInteger);
		break;
	    case ATOM_FLOAT:
		pc=MakeFloatForm(pcValue->pfForm->uValue.dfFloat);
		break;
	    default:
		ErrorMessage("conditional-exp:  Invalid or non-numeric argument\n");
		pc=MakeFloatForm(0.0);
	}
	
	EXIT("ComputeConditionalExp");

	return pc;
}


/* ComputeExpt

Description:
	Power of zero or more numbers.
*/

CELLP ComputeExpt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfValue,dfMultiplier;
	double nPower;
	int i;

	ENTER("ComputeExpt",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeExpt");
		return pc;
	}
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-expt",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			dfValue=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfValue=pcValue->pfForm->uValue.dfFloat;
			break;
		default:
			ErrorMessage("ComputeExpt:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeExpt");
			return pc;
	}
	if(!pc->pcNext)
	{
		pc=MakeFloatForm(1.0);
		EXIT("ComputeExpt");
		return pc;
	}
	for(pc=pc->pcNext;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-expt",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				nPower=pcValue->pfForm->uValue.nInteger;
				if(!nPower)
					dfValue=1.0;
				else if(nPower<0)
				{
					if(dfValue!=0.0)
					{
						dfMultiplier=dfValue;
						for(i=-1;i>nPower;i--)
							dfValue*=dfMultiplier;
					}
					if(dfValue==0.0)
					{
						ErrorMessage("ComputeExpt:  Division by zero\n");
						pc=MakeFloatForm(0.0);
						EXIT("ComputeExpt");
						return pc;
					}
					dfValue=1.0/dfValue;
				}
				else
				{
					dfMultiplier=dfValue;
					for(i=1;i<nPower;i++)
						dfValue*=dfMultiplier;
				}
				break;
			case ATOM_FLOAT:
				if(dfValue<0.0)
				{
					ErrorMessage("ComputeExpt:  Non-integer power of negative value\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputeExpt");
					return pc;
				}
				if(dfValue!=0.0)
					dfValue=exp(log(dfValue)*pcValue->pfForm->uValue.dfFloat);
				else
				{
					if(pcValue->pfForm->uValue.dfFloat==0.0)
						dfValue=1.0;
				}
				break;
			default:
				ErrorMessage("ComputeExpt:  Invalid or non-numeric argument\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeExpt");
				return pc;
		}
	}
	pc=MakeFloatForm(dfValue);
	EXIT("ComputeExpt");
	return pc;
}

/* ComputeSqrt

Description:
	Take the square root of a number.
*/

CELLP ComputeSqrt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfValue;

	ENTER("ComputeSqrt",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeSqrt");
		return pc;
	}
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-sqrt",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			dfValue=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfValue=pcValue->pfForm->uValue.dfFloat;
			break;
		default:
			ErrorMessage("ComputeSqrt:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeSqrt");
			return pc;
	}
	if(dfValue<0.0)
	{
		ErrorMessage("ComputeSqrt:  Value less than zero\n");
		pc=MakeFloatForm(0.0);
		EXIT("ComputeSqrt");
		return pc;
	}
	pc=MakeFloatForm(sqrt(dfValue));
	EXIT("ComputeSqrt");
	return pc;
}

/* ComputeAbs

Description:
	Take the absolute value of one or more numbers.
*/

CELLP ComputeAbs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfValue;

	ENTER("ComputeAbs",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeAbs");
		return pc;
	}
	if(!pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-abs",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				dfValue=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				dfValue=pcValue->pfForm->uValue.dfFloat;
				break;
			default:
				ErrorMessage("ComputeAbs:  Invalid or non-numeric argument\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeAbs");
				return pc;
		}
		if(dfValue<0.0)
			dfValue=-dfValue;
		pc=MakeFloatForm(dfValue);
		EXIT("ComputeAbs");
		return pc;
	}
	dfValue=0.0;
	for(;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-abs",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				dfValue+=pcValue->pfForm->uValue.nInteger*pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				dfValue+=pcValue->pfForm->uValue.dfFloat*pcValue->pfForm->uValue.dfFloat;
				break;
			default:
				ErrorMessage("ComputeAbs:  Invalid or non-numeric argument\n");
				pc=MakeFloatForm(0.0);
				EXIT("ComputeAbs");
				return pc;
		}
	}
	pc=MakeFloatForm(sqrt(dfValue));
	EXIT("ComputeAbs");
	return pc;
}

/* ComputeExp

Description:
	Take the exponential of an integer.
*/

CELLP ComputeExp
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfValue;

	ENTER("ComputeExp",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
		dfValue=0.0;
	else
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-exp",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				dfValue=exp(pcValue->pfForm->uValue.nInteger);
				break;
			case ATOM_FLOAT:
				dfValue=exp(pcValue->pfForm->uValue.dfFloat);
				break;
			default:
				ErrorMessage("ComputeExp:  Invalid or non-numeric argument\n");
				dfValue=0.0;
		}
	}
	pc=MakeFloatForm(dfValue);
	EXIT("ComputeExp");
	return pc;
}

/* ComputeLog

Description:
	Take the logarithm of a number.
*/

CELLP ComputeLog
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfValue,dfBase;

	ENTER("ComputeLog",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(0.0);
		EXIT("ComputeLog");
		return pc;
	}
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-log",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			dfValue=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfValue=pcValue->pfForm->uValue.dfFloat;
			break;
		default:
			ErrorMessage("ComputeLog:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeLog");
			return pc;
	}
	if(!pc->pcNext)
	{
		if(dfValue<=0.0)
		{
			ErrorMessage("ComputeLog:  Value must be positive\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeLog");
			return pc;
		}
		pc=MakeFloatForm(log(dfValue));
		EXIT("ComputeLog");
		return pc;
	}
	pc=pc->pcNext;
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-log",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			dfBase=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfBase=pcValue->pfForm->uValue.dfFloat;
			break;
		default:
			ErrorMessage("ComputeLog:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeLog");
			return pc;
	}
	if(dfBase<=1.0)
	{
		ErrorMessage("ComputeLog:  Base must be greater than one\n");
		pc=MakeFloatForm(0.0);
	}
	else
		pc=MakeFloatForm(log(dfValue)/log(dfBase));
	EXIT("ComputeLog");
	return pc;
}

/* ComputeRand

Description:
	Generate a random number.
*/

CELLP ComputeRand
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeRand",TRUE);
	pc=MakeIntegerForm((*prCurrentRNG->pfRNG)());
	EXIT("ComputeRand");
	return pc;
}

/* ComputeRandom

Description:
	Generate a random number between min and max.
*/

CELLP ComputeRandom
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	double dfMin,dfMax;

	ENTER("ComputeRandom",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfMin))
	{
		ErrorMessage("ComputeRandom:  Invalid or non-numeric argument\n");
		pc=MakeFloatForm(0.0);
		EXIT("ComputeRandom");
		return pc;
	}
	pc=pc->pcNext;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfMax))
	{
		ErrorMessage("ComputeRandom:  Invalid or non-numeric argument\n");
		pc=MakeFloatForm(0.0);
		EXIT("ComputeRandom");
		return pc;
	}
	pc=MakeFloatForm((dfMax-dfMin)*(unsigned)(*prCurrentRNG->pfRNG)()/prCurrentRNG->dfMaxValue+dfMin);
	EXIT("ComputeRandom");
	return pc;
}

/* EvalSeed

Description:
	Seed the random number generator.
Notes:
	This is implemented as a pseudo-predicate, since it returns no value.
*/

BOOL EvalSeed
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int nSeed;

	ENTER("EvalSeed",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nSeed))
	{
		ErrorMessage("EvalSeed:  Invalid or non-numeric argument\n");
		EXIT("EvalSeed");
		return FALSE;
	}
	(*prCurrentRNG->pfSeed)(nSeed);
	EXIT("EvalSeed");
	return TRUE;
}

/* ComputeRound

Description:
	Round a number to the nearest integer.
*/

CELLP ComputeRound
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	int nValue;

	ENTER("ComputeRound",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
		nValue=0;
	else
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-round",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				nValue=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				if(pcValue->pfForm->uValue.dfFloat<0.0)
					nValue=(int)(pcValue->pfForm->uValue.dfFloat-0.5);
				else
					nValue=(int)(pcValue->pfForm->uValue.dfFloat+0.5);
				break;
			default:
				ErrorMessage("ComputeRound:  Invalid or non-numeric argument\n");
				nValue=0;
		}
	}
	pc=MakeIntegerForm(nValue);
	EXIT("ComputeRound");
	return pc;
}

/* ComputeInt

Description:
	Truncate a number toward zero.
*/

CELLP ComputeInt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	int nValue;

	ENTER("ComputeInt",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
		nValue=0;
	else
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-int",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				nValue=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				nValue=(int)(pcValue->pfForm->uValue.dfFloat);
				break;
			default:
				ErrorMessage("ComputeInt:  Invalid or non-numeric argument\n");
				nValue=0;
		}
	}
	pc=MakeIntegerForm(nValue);
	EXIT("ComputeInt");
	return pc;
}

/* ComputeFloor

Description:
	Round a number to the next lowest integer.
*/

CELLP ComputeFloor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	int nValue;

	ENTER("ComputeFloor",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
		nValue=0;
	else
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-floor",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				nValue=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				if(pcValue->pfForm->uValue.dfFloat<0.0)
				{
					if(pcValue->pfForm->uValue.dfFloat<(int)pcValue->pfForm->uValue.dfFloat)
						nValue=(int)(pcValue->pfForm->uValue.dfFloat-1.0);
					else
						nValue=(int)(pcValue->pfForm->uValue.dfFloat);
				}
				else
					nValue=(int)(pcValue->pfForm->uValue.dfFloat);
				break;
			default:
				ErrorMessage("ComputeFloor:  Invalid or non-numeric argument\n");
				nValue=0;
		}
	}
	pc=MakeIntegerForm(nValue);
	EXIT("ComputeFloor");
	return pc;
}

/* ComputeCeil

Description:
	Round a number to the next highest integer.
*/

CELLP ComputeCeil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	int nValue;

	ENTER("ComputeCeil",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
		nValue=0;
	else
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-ceil",pcFormula,pbBindings);
		switch(pcValue->pfForm->nType)
		{
			case ATOM_INTEGER:
				nValue=pcValue->pfForm->uValue.nInteger;
				break;
			case ATOM_FLOAT:
				if(pcValue->pfForm->uValue.dfFloat>0.0)
				{
					if(pcValue->pfForm->uValue.dfFloat>(int)pcValue->pfForm->uValue.dfFloat)
						nValue=(int)(pcValue->pfForm->uValue.dfFloat+1.0);
					else
						nValue=(int)(pcValue->pfForm->uValue.dfFloat);
				}
				else
					nValue=(int)(pcValue->pfForm->uValue.dfFloat);
				break;
			default:
				ErrorMessage("ComputeCeil:  Invalid or non-numeric argument\n");
				nValue=0;
		}
	}
	pc=MakeIntegerForm(nValue);
	EXIT("ComputeCeil");
	return pc;
}

/* Boolean Relations ----------------------------------------------------------- */

/* EvalLt

Description:
	Less than on two numbers
*/

BOOL EvalLt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	double df1,df2;

	ENTER("EvalLt",TRUE);
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&df1))
	{
		ErrorMessage("EvalLt:  Invalid or non-numeric argument\n");
		EXIT("EvalLt");
		return FALSE;
	}
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings,&df2))
	{
		ErrorMessage("EvalLt:  Invalid or non-numeric argument\n");
		EXIT("EvalLt");
		return FALSE;
	}
	EXIT("EvalLt");
	return df1<df2;
}

/* GenerateLt

Description:
	Generate integers less than a value.
Note:
	This generator does not terminate on its own.
Call:
	(< ?x 20)
*/

BOOL GenerateLt
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CELLP pc; 							/* pointer to literal */
	double dfVal;						/* initial value */
	ICONTEXTP pgc;						/* generator context pointer */

	if(!*ppvContext)					/* initialize */
	{
		if(!FormulaToDouble(pcGenLit->pfForm->pcArgs->pcNext,
			plpLinearPlan,pbBindings,&dfVal))
			return FALSE;				/* message already printed */
		pgc=(ICONTEXTP)MemAlloc(sizeof(ICONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		if(dfVal>floor(dfVal))
			pgc->nValue=(int)floor(dfVal);
		else
			pgc->nValue=(int)floor(dfVal)-1;
	}
	else
		pgc=(ICONTEXTP)*ppvContext;

	lv.nInteger=pgc->nValue--;			/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* EvalLe

Description:
	Less than or equal on two numbers
*/

BOOL EvalLe
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	double df1,df2;

	ENTER("EvalLe",TRUE);
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&df1))
	{
		ErrorMessage("EvalLe:  Invalid or non-numeric argument\n");
		EXIT("EvalLe");
		return FALSE;
	}
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings,&df2))
	{
		ErrorMessage("EvalLe:  Invalid or non-numeric argument\n");
		EXIT("EvalLe");
		return FALSE;
	}
	EXIT("EvalLe");
	return df1<=df2;
}

/* GenerateLe

Description:
	Generate integers less than or equal to a value.
Note:
	This generator does not terminate on its own.
Call:
	(<= ?x 20)
*/

BOOL GenerateLe
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CELLP pc; 							/* pointer to literal */
	double dfVal;						/* initial value */
	ICONTEXTP pgc;						/* generator context pointer */

	if(!*ppvContext)					/* initialize */
	{
		if(!FormulaToDouble(pcGenLit->pfForm->pcArgs->pcNext,
			plpLinearPlan,pbBindings,&dfVal))
			return FALSE;				/* message already printed */
		pgc=(ICONTEXTP)MemAlloc(sizeof(ICONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->nValue=(int)floor(dfVal);
	}
	else
		pgc=(ICONTEXTP)*ppvContext;

	lv.nInteger=pgc->nValue--;			/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* EvalGt

Description:
	Evaluate a > relation.
*/

BOOL EvalGt
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	double df1,df2;

	ENTER("EvalGt",TRUE);
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&df1))
	{
		ErrorMessage("EvalGt:  Invalid or non-numeric argument\n");
		EXIT("EvalGt");
		return FALSE;
	}
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings,&df2))
	{
		ErrorMessage("EvalGt:  Invalid or non-numeric argument\n");
		EXIT("EvalGt");
		return FALSE;
	}
	EXIT("EvalGt");
	return df1>df2;
}

/* GenerateGt

Description:
	Generate integers greater than a value.
Note:
	This generator does not terminate on its own.
Call:
	(> ?x 20)
*/

BOOL GenerateGt
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CELLP pc; 							/* pointer to literal */
	double dfVal;						/* initial value */
	ICONTEXTP pgc;						/* generator context pointer */

	if(!*ppvContext)					/* initialize */
	{
		if(!FormulaToDouble(pcGenLit->pfForm->pcArgs->pcNext,
			plpLinearPlan,pbBindings,&dfVal))
			return FALSE;				/* message already printed */
		pgc=(ICONTEXTP)MemAlloc(sizeof(ICONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		if(dfVal<ceil(dfVal))
			pgc->nValue=(int)ceil(dfVal);
		else
			pgc->nValue=(int)ceil(dfVal)+1;
	}
	else
		pgc=(ICONTEXTP)*ppvContext;

	lv.nInteger=pgc->nValue++;			/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* EvalGe

Description:
	Evaluate a >= relation.
*/

BOOL EvalGe
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	double df1,df2;

	ENTER("EvalGe",TRUE);
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&df1))
	{
		ErrorMessage("EvalGe:  Invalid or non-numeric argument\n");
		EXIT("EvalGe");
		return FALSE;
	}
	if(!FormulaToDouble(pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings,&df2))
	{
		ErrorMessage("EvalGe:  Invalid or non-numeric argument\n");
		EXIT("EvalGe");
		return FALSE;
	}
	EXIT("EvalGe");
	return df1>=df2;
}

/* GenerateGe

Description:
	Generate integers greater than or equal to a value.
Note:
	This generator does not terminate on its own.
Call:
	(>= ?x 20)
*/

BOOL GenerateGe
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CELLP pc; 							/* pointer to literal */
	double dfVal;						/* initial value */
	ICONTEXTP pgc;						/* generator context pointer */

	if(!*ppvContext)					/* initialize */
	{
		if(!FormulaToDouble(pcGenLit->pfForm->pcArgs->pcNext,
			plpLinearPlan,pbBindings,&dfVal))
			return FALSE;				/* message already printed */
		pgc=(ICONTEXTP)MemAlloc(sizeof(ICONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->nValue=(int)ceil(dfVal);
	}
	else
		pgc=(ICONTEXTP)*ppvContext;

	lv.nInteger=pgc->nValue++;	/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* Integer Generators ---------------------------------------------------------- */

/* GeneratePosInt

Description:
	Generate the positive integers.
Notes:
	This generator does not terminate on its own.
	We use the calling quantifier's context pointer to store our context,
	so we don't need to allocate a context block.
Call:
	(pos-int ?x)
*/

BOOL GeneratePosInt
(
	CELLP pcGenLit,						/* genlit as a formula (ignored) */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CELLP pc; 							/* pointer to literal */

	lv.nInteger=++(*(int *)ppvContext);	/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* GenerateLtPosInt

Description:
	Generate the positive integers less than a given number
Notes:
	We use the calling quantifier's context pointer to store our context,
	so we don't need to allocate a context block.
Call:
	(<-pos-int ?x 20)
*/

BOOL GenerateLtPosInt
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	CELLP pc; 							/* pointer to literal */
	double dfVal;

	if(!*ppvContext)					/* initialize */
	{
		if(!FormulaToDouble(pcGenLit->pfForm->pcArgs->pcNext,
			plpLinearPlan,pbBindings,&dfVal))
			return FALSE;				/* message already printed */
		if(dfVal>floor(dfVal))
			*(int *)ppvContext=(int)floor(dfVal)+1;
		else
			*(int *)ppvContext=(int)floor(dfVal);
	}
	lv.nInteger=--(*(int *)ppvContext);	/* generate next integer */
	if(lv.nInteger<=0)					/* check termination condition */
		return FALSE;
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* additional generators and evaluators ------------------------------------- */

/* GenerateIsBetween

Description:
	Generate the integers between two (inclusive) limits.
Call:
	(is-between ?x a b)
*/

BOOL GenerateIsBetween
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LITVAL lv; 							/* generic value (a union) */
	LCONTEXTP pgc;				/* context pointer */
	CELLP pc; 							/* pointer to literal */
	int nMin,nMax;						/* limits */

	/* initialization */

	if(!*ppvContext)
	{
		pc=pcGenLit->pfForm->pcArgs;
		if(!pc)
		{
			ErrorMessage("is-between:  No arguments\n");
			return FALSE;
		}

		pc=pc->pcNext;					/* skip over binding variable */
		if(!pc)
		{
			ErrorMessage("is-between:  No limits\n");
			return FALSE;
		}
		if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMin))
			return FALSE;				/* message already printed */

		pc=pc->pcNext;
		if(!pc)
		{
			ErrorMessage("is-between:  No maximum limit\n");
			return FALSE;
		}
		if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMax))
			return FALSE;				/* message already printed */

		pgc=(LCONTEXTP)MemAlloc(sizeof(LCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->nNext=nMin;
		pgc->nLast=nMax;
	}
	else
		pgc=(LCONTEXTP)*ppvContext;

	/* iteration */

	if(pgc->nNext>pgc->nLast)
		return FALSE;
	lv.nInteger=pgc->nNext++;	/* generate next integer */
	pc=MakeLiteralForm(ATOM_INTEGER,lv);	/* convert to formula */
	SetVarX(pcVars,pc,pbBindings);		/* bind to passed variable */
	return TRUE;
}

/* EvalIsBetween

Description:
	Determine if a number is between between two (inclusive) limits.
Call:
	(is-between ?x a b)
*/

BOOL EvalIsBetween
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc; 							/* pointer to literal */
	double dfVal;
	double dfMin,dfMax;					/* limits */

	/* initialization */

	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		ErrorMessage("is-between:  No arguments\n");
		return FALSE;
	}
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfVal))
		return FALSE;					/* message already printed */
	
	pc=pc->pcNext;
	if(!pc)
	{
		ErrorMessage("is-between:  No limits\n");
		return FALSE;
	}
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfMin))
		return FALSE;					/* message already printed */

	pc=pc->pcNext;
	if(!pc)
	{
		ErrorMessage("is-between:  No maximum limit\n");
		return FALSE;
	}
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfMax))
		return FALSE;					/* message already printed */

	if(dfMin<=dfVal&&dfVal<=dfMax)
		return TRUE;
	return FALSE;
}

/* GenerateInTheSet

Description:
	Generate the elements of a set, provided as arguments.
Call:
	(in-the-set (?x ?y) aa ab ba bb ...)
*/

BOOL GenerateInTheSet
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	CELLP pc1,pc2;

	/* initialization */
	
	if(!*ppvContext)
	{
		*(CELLP *)ppvContext=pcGenLit->pfForm->pcArgs;
		if(!*ppvContext)
		{
			ErrorMessage("in-the-set:  No values\n");
			return FALSE;
		}
	}
	else if(*ppvContext==(void *)-1)
		return FALSE;

	/* iteration */

	pc2=*(CELLP *)ppvContext;			/* point to next element */
	for(pc1=pcVars;pc1;pc1=pc1->pcNext)
	{
		if(!pc2)						/* check termination condition */
			return FALSE;
		SetVarX(pc1,CopyCell(pc2),pbBindings);	/* bind to passed variables */
		pc2=pc2->pcNext;
	}
	
	*(CELLP *)ppvContext=pc2;			/* advance context */
	if(!*ppvContext)					/* flag end of list */
		*ppvContext=(void *)-1;
	return TRUE;
}

/* EvalInTheSet

Description:
	Determine if the first argument is equal to one of the others.
*/

BOOL EvalInTheSet
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pcVars;
	CELLP pcVals;
	CELLP pcNext;
	CELLP pc,pc1,pc2;

	ENTER("EvalInTheSet",TRUE);
	b=FALSE;
	pc=pcFormula->pfForm->pcVars;
	pcVars=ComputeTerms(pc,plpLinearPlan,pbBindings);
	if(!pcVars)
		TermError("eval-in-the-set",pcFormula,pbBindings);
	else
	{
		for(pc1=pcFormula->pfForm->pcArgs;pc1;)
		{
			pcVals=NULL;
			pcNext=(CELLP)&pcVals;
			for(pc2=pcVars;pc2;pc2=pc2->pcNext)
			{
				pcNext=pcNext->pcNext=CopyCell(ComputeTerm(pc1,plpLinearPlan,pbBindings));
				pc1=pc1->pcNext;
			}
			if(FormulaListEqQ(pcVars,pcVals))
			{
				EXIT("EvalInTheSet");
				return TRUE;
			}
			if(!pc1)
				break;
		}
	}
	EXIT("EvalInTheSet");
	return FALSE;
}

/* GenerateNearestFirst --------------------------------------------------------

Description:
	Do a breadth first search on a graph described by an edge-predicate.
	Enumerate the reachable vertices of the graph in order of distance (number of edges).
Call:
	(nearest-first <edge-predicate> start-vertex ?next-vertex ?next-distance)
Notes:
	This routine uses a simple breadth first graph search to generate the
	vertices of a graph in nearest first order, relative to the starting vertex.
	The algorithm is complicated somewhat by the fact that we only have a list of edges,
	and no list of vertices to work with.  
	
	The auxiliary routine, InsertVertex is used to create a list of unique vertices on the fly.
	Each time a vertex is encountered, it is passed to InsertVertex.  InsertVertex returns
	TRUE iff the vertex hasn't been seen before.  
	
	If a vertex is new, then breadth-first-search adds the vertex to its queue or open list.
	Each time a vertex is retrieved from the open list, it is expanded by processing all
	of its adjacent vertices.  Then the retrieved vertex is passed back to the caller.
Data Structures:
	4 arrays are allocated.  pnVertexIndices is maintained in name sorted order, while
	the other arrays are all maintained in allocation order, or in the order that vertices
	were first encountered.
  
	pnVertexIndices - This is the vertex indirection array.  InsertVertex maintains
		this array in name sorted order, so it can determine if a vertex has been seen
		before and report this to GenerateNearestFirst.
	ppcVertices - This is the vertex literal pointer array.  InsertVertex uses the literal
		names to determine if a vertex has been seen before.  GenerateNearestFirst uses
		the literal names to search the edge predicate for more vertices;
	pnDistances - This is the vertex distance array.  GenerateNearestFirst returns this
		value as one of its arguments
	pnQueue - This is the open list.  GenerateNearestFirst maintains a list of vertex
		allocation indices in this array.  Do we even need this array, or is it simply
		the identity transformation?
*/

BOOL GenerateNearestFirst
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	NFCONTEXTP pgc;						/* context pointer */
	CELLP pcEdge;						/* edge predicate */
	CELLP pcStart;						/* start vertex */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarDistance;				/* distance variable */
	
	CELLP pcDistance;					/* returned distance */
	SYMBOLINFOP psi;					/* symbol info pointer */

	int nCount;							/* maximum array size required */
	int nIndex;							/* vertex index (allocation order) */
	void *pvStack;						/* btree search stack context */
	CELLP pcArgs;						/* argument list for edge btree search */
	CELLP pcTuple;						/* edge tuple pointer */
	int nOpenIndex;						/* index of current vertex (allocation order) */

	/* initialization */

	if(!*ppvContext)
	{
		// edge predicate must be a described predicate... 

		pcEdge=pcGenLit->pfForm->pcArgs;
		if(!pcEdge)
		{
			ErrorMessage("nearest-first:  No edge predicate\n");
			return FALSE;
		}
		if(pcEdge->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("nearest-first:  Edge argument is not a symbol (described predicate)\n");
			return FALSE;
		}
		psi=pcEdge->pfForm->uValue.psiSymbolInfo;
		if(psi->nSymbolType!=SYMBOL_PREDICATE)
		{
			ErrorMessage("nearest-first:  Edge argument is not a predicate\n");
			return FALSE;
		}
		if(psi-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("nearest-first:  Edge predicate is not described\n");
			return FALSE;
		}
		if(psi->nArity!=2)
		{
			ErrorMessage("nearest-first:  Edge predicate must have arity 2\n");
			return FALSE;
		}

		// fetch start vertex

		pcStart=pcEdge->pcNext;
		if(!pcStart)
		{
			ErrorMessage("nearest-first:  No start vertex\n");
			return FALSE;
		}

		// fetch vertex variable
		
		pcVarVertex=pcStart->pcNext;
		if(!pcVarVertex)
		{
			ErrorMessage("nearest-first:  No vertex variable\n");
			return FALSE;
		}

		// fetch distance variable
		
		pcVarDistance=pcVarVertex->pcNext;
		if(!pcVarDistance)
		{
			ErrorMessage("nearest-first:  No distance variable\n");
			return FALSE;
		}

		// allocate the generator context block
		
		pgc=(NFCONTEXTP)MemAlloc(sizeof(NFCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->pfMark=(void (*)(GCONTEXTP))MarkNearestFirst;
		
		// initialize arrays

		pgc->pbtEdge=plpLinearPlan->apbtWorld[psi-asiWorldSymbols];
		nCount=BTreeCount(pgc->pbtEdge,0)+1;	// at most nEdges+1 vertices can be reached

		pgc->pnVertexIndices=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->ppcVertices=(CELLP *)MemAlloc(sizeof(CELLP)*nCount);
		pgc->pnDistances=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->pnQueue=(int *)MemAlloc(sizeof(int)*nCount);

		pgc->pnNext=pgc->pnQueue-1;
		pgc->pnFree=pgc->pnQueue;
		pgc->nVertices=0;		// number of vertices

		pgc->pcVarVertex=pcVarVertex;
		pgc->pcVarDistance=pcVarDistance;
		
		// put start vertex on open list

		pcStart=ComputeTerm(pcStart,plpLinearPlan,pbBindings);	// dereference any variables
		InsertVertex(pcStart,			// add start vertex to vertex array
			pgc->pnVertexIndices,
			pgc->ppcVertices,
			&pgc->nVertices,
			&nIndex);
		pgc->pnDistances[nIndex]=0;	// starting distance
		*pgc->pnFree++=nIndex;	// put vertex on the open list
	}
	else
		pgc=(NFCONTEXTP)*ppvContext;

	// enumerate reachable vertices

	while(pgc->pnNext<pgc->pnFree)	// search all open vertices
	{
		nOpenIndex=*++pgc->pnNext;	// fetch next vertex from open list

		// assemble search argument list for this vertex
			
		pcArgs=CopyCell(pgc->ppcVertices[nOpenIndex]);
		pcArgs->pcNext=pgc->pcVarVertex;

		// search edge btree for adjacent vertices
			
		pvStack=0;
		BTreeGeneratorWithBoundVars(pgc->pbtEdge,&pvStack,pcArgs);			// Initialize btree search
		for(pcTuple=BTreeGeneratorWithBoundVars(NULL,&pvStack,pcArgs);pcTuple;
			pcTuple=BTreeGeneratorWithBoundVars(NULL,&pvStack,pcArgs))
		{
			if(InsertVertex(pcTuple->pcNext,	// add next vertex to vertex array
				pgc->pnVertexIndices,
				pgc->ppcVertices,
				&pgc->nVertices,
				&nIndex))
			{
				pgc->pnDistances[nIndex]=pgc->pnDistances[nOpenIndex]+1;	// calculate distance from start
				*pgc->pnFree++=nIndex;	// put vertex on the open list
			}
		}

		// return next vertex and distance from start

		if(nOpenIndex)					// don't enumerate start vertex
		{
			SetVarX(pgc->pcVarVertex,pgc->ppcVertices[nOpenIndex],pbBindings);
			pcDistance=MakeIntegerForm(pgc->pnDistances[nOpenIndex]);
			SetVarX(pgc->pcVarDistance,pcDistance,pbBindings);
			return TRUE;
		}
	}
	return FALSE;						// no more vertices to enumerate
}

/* MarkNearestFirst

Description:
	Mark the data structures associated with a nearest first context block.
*/

static void MarkNearestFirst
(
	NFCONTEXTP pgc						// pointer to context block 
)
{
	ZoneMark(pgc->ppcVertices);
	ZoneMark(pgc->pnDistances);
	ZoneMark(pgc->pnQueue);
}

/* GenerateNearestFirstEx ------------------------------------------------------

Description:
	Do a breadth first search on a graph described by an edge-predicate.
	Enumerate the reachable vertices of the graph in order of distance (number of edges).
Call:
	(nearest-first-ex (?loc-from ?loc-to) (<edge-predicate> ?arg1 ?arg2 ...) 
		start-vertex ?next-vertex ?next-distance)
Notes:
	This routine uses a simple breadth first graph search to generate the
	vertices of a graph in nearest first order, relative to the starting vertex.
	The algorithm is complicated somewhat by the fact that we only have a list of edges,
	and no list of vertices to work with.  
	
	The auxiliary routine, InsertVertex is used to create a list of unique vertices on the fly.
	Each time a vertex is encountered, it is passed to InsertVertex.  InsertVertex returns
	TRUE iff the vertex hasn't been seen before.  
	
	If a vertex is new, then breadth-first-search adds the vertex to its queue or open list.
	Each time a vertex is retrieved from the open list, it is expanded by processing all
	of its adjacent vertices.  Then the retrieved vertex is passed back to the caller.
Data Structures:
	4 arrays are allocated.  pnVertexIndices is maintained in name sorted order, while
	the other arrays are all maintained in allocation order, or in the order that vertices
	were first encountered.
  
	pnVertexIndices - This is the vertex indirection array.  InsertVertex maintains
		this array in name sorted order, so it can determine if a vertex has been seen
		before and report this to GenerateNearestFirst.
	ppcVertices - This is the vertex literal pointer array.  InsertVertex uses the literal
		names to determine if a vertex has been seen before.  GenerateNearestFirst uses
		the literal names to search the edge predicate for more vertices;
	pnDistances - This is the vertex distance array.  GenerateNearestFirst returns this
		value as one of its arguments
	pnQueue - This is the open list.  GenerateNearestFirst maintains a list of vertex
		allocation indices in this array.  Do we even need this array, or is it simply
		the identity transformation?
*/

BOOL GenerateNearestFirstEx
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	NFXCONTEXTP pgc;					/* context pointer */
	CELLP pcLocal;						/* local variables */
	CELLP pcEdge;						/* edge predicate */
	CELLP pcStart;						/* start vertex */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarDist;					/* distance variable */
	
	CELLP pcEdgeCount;					/* returned edge-count */
	SYMBOLINFOP psi;					/* symbol info pointer */

	int nCount;							/* maximum array size required */
	int nIndex;							/* vertex index (allocation order) */
	CELLP pcArgs;						/* argument list for edge btree search */
	CELLP pc;							/* temporary pointer */
	int nOpenIndex;						/* index of current vertex (allocation order) */

	BTREEP pbt;							/* btree pointer */

	/* initialization */

	if(!*ppvContext)
	{
		// fetch the local variable list
		
		pcLocal=pcGenLit->pfForm->pcArgs;

		// edge predicate must be a described predicate... 

		pcEdge=pcLocal->pcNext;
		if(!pcEdge)
		{
			ErrorMessage("nearest-first-ex:  No edge predicate\n");
			return FALSE;
		}
		if(pcEdge->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("nearest-first-ex:  Edge argument is not a symbol (described predicate)\n");
			return FALSE;
		}
		psi=pcEdge->pfForm->uValue.psiSymbolInfo;
		if(psi->nSymbolType!=SYMBOL_PREDICATE)
		{
			ErrorMessage("nearest-first-ex:  Edge argument is not a predicate\n");
			return FALSE;
		}
		if(psi-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("nearest-first-ex:  Edge predicate is not described\n");
			return FALSE;
		}
		if(psi->nArity<2)
		{
			ErrorMessage("nearest-first-ex:  Edge predicate arity must be atleast 2\n");
			return FALSE;
		}

		// fetch start vertex

		pcStart=pcEdge->pcNext;
		if(!pcStart)
		{
			ErrorMessage("nearest-first-ex:  No start vertex\n");
			return FALSE;
		}

		// fetch vertex variable
		
		pcVarVertex=pcStart->pcNext;
		if(!pcVarVertex)
		{
			ErrorMessage("nearest-first-ex:  No vertex variable\n");
			return FALSE;
		}

		// fetch distance variable
		
		pcVarDist=pcVarVertex->pcNext;
		if(!pcVarDist)
		{
			ErrorMessage("nearest-first-ex:  No distance variable\n");
			return FALSE;
		}

		// allocate the generator context block
		
		pgc=(NFXCONTEXTP)MemAlloc(sizeof(NFXCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->pfMark=(void (*)(GCONTEXTP))MarkNearestFirstEx;
		
		// save our parsed context
		
		pgc->pcLocal=pcLocal;
		pgc->pcEdge=pcEdge;
		pgc->pcVarVertex=pcVarVertex;
		pgc->pcVarDist=pcVarDist;

		// allocate BFS context

		pbt=plpLinearPlan->apbtWorld[psi-asiWorldSymbols];
		nCount=BTreeCount(pbt,0)+1;		// at most nEdges+1 vertices can be reached

		pgc->pnVertexIndices=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->ppcVertices=(CELLP *)MemAlloc(sizeof(CELLP)*nCount);
		pgc->pnEdgeCounts=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->pnQueue=(int *)MemAlloc(sizeof(int)*nCount);

		pgc->pnNext=pgc->pnQueue-1;
		pgc->pnFree=pgc->pnQueue;
		pgc->nVertices=0;				// number of vertices

		// put start vertex on open list

		pcStart=ComputeTerm(pcStart,plpLinearPlan,pbBindings);	// dereference any variables
		InsertVertex(pcStart,			// add start vertex to vertex array
			pgc->pnVertexIndices,
			pgc->ppcVertices,
			&pgc->nVertices,
			&nIndex);
		pgc->pnEdgeCounts[nIndex]=0;	// starting edge-count
		*pgc->pnFree++=nIndex;	// put vertex on the open list
	}
	else
		pgc=(NFXCONTEXTP)*ppvContext;

	// enumerate reachable vertices

	while(pgc->pnNext<pgc->pnFree)	// search all open vertices
	{
		CELL c1,c2;						// temporary cells

		nOpenIndex=*++pgc->pnNext;	// fetch next vertex from open list

		// assemble search argument list for this vertex
			
//		pcArgs=CopyCell(pgc->ppcVertices[nOpenIndex]);
//		pcArgs->pcNext=pgc->pcVarVertex;

		pcLocal=CopyCell2(&c1,pgc->pcLocal);
		pcLocal->pcNext=pcLocal->pfForm->pcArgs;
		pcArgs=CopyCell2(&c2,pgc->ppcVertices[nOpenIndex]);
		pcArgs->pcNext=pcLocal->pfForm->pcArgs;
	
		pbBindings=ExtendBindings(pcLocal,pcArgs,pbBindings);	/* push */

		// search edge btree for adjacent vertices
			
		while(GenerateDescPredicate(pgc->pcEdge,(void *) &pbBindings->pgcContext,NULL,plpLinearPlan,pbBindings))
		{
			// add next vertex to vertex array
			
			pc=ComputeTerm(pcLocal->pfForm->pcArgs,plpLinearPlan,pbBindings);
			if(InsertVertex(pc,
				pgc->pnVertexIndices,
				pgc->ppcVertices,
				&pgc->nVertices,
				&nIndex))
			{
				pgc->pnEdgeCounts[nIndex]=pgc->pnEdgeCounts[nOpenIndex]+1;	// calculate distance from start
				*pgc->pnFree++=nIndex;	// put vertex on the open list
			}
		}

		// return next vertex and distance from start

		if(nOpenIndex)					// don't enumerate start vertex
		{
			SetVarX(pgc->pcVarVertex,pgc->ppcVertices[nOpenIndex],pbBindings);
			pcEdgeCount=MakeIntegerForm(pgc->pnEdgeCounts[nOpenIndex]);
			SetVarX(pgc->pcVarDist,pcEdgeCount,pbBindings);
			return TRUE;
		}
	}
	return FALSE;						// no more vertices to enumerate
}

/* MarkNearestFirstEx

Description:
	Mark the data structures associated with a nearest first context block.
*/

static void MarkNearestFirstEx
(
	NFXCONTEXTP pgc						// pointer to context block 
)
{
	int i;

	ZoneMark(pgc->ppcVertices);
	for(i=0;i<pgc->nVertices;i++)
		MarkFormula(pgc->ppcVertices[i]);
	ZoneMark(pgc->pnEdgeCounts);
	ZoneMark(pgc->pnQueue);
}

/* InsertVertex

Description:
	If the new vertex isn't already in the array, it is added.
Returns:
	TRUE if the vertex has not been seen before
Notes:
	This routine uses binary search to maintain a sorted array of unique vertices
	for the GenerateNearestFirst and GenerateClosestFirst routines.
	The vertex index array is always assumed to be large enough to accept another vertex.
*/

static BOOL InsertVertex
(
	CELLP pcVertex,						// vertex to insert
	int *pnVertexIndices,				// vertex index array (name sorted order)
	CELLP *ppcVertices,					// vertex pointer array (allocation order)
	int *pnCount,						// number of vertices in array
	int *pnIndex						// returned index of vertex
)
{
	int nLeft;							// index of lower bound (inclusive)
	int nRight;							// index of upper bound (exclusive)
	int nMiddle;						// index of middle
	int nFlag;							// comparison value

	// search for vertex in array

	nLeft=0;
	nRight=*pnCount;
	while(nLeft<nRight)
	{
		nMiddle=(nLeft+nRight)/2;
		if(StringEqQ(pcVertex->pfForm->psName,ppcVertices[pnVertexIndices[nMiddle]]->pfForm->psName))
		{
			*pnIndex=pnVertexIndices[nMiddle];	// return allocation index
			return FALSE;				// we found an old entry
		}
		nFlag=strcmp(pcVertex->pfForm->psName,ppcVertices[pnVertexIndices[nMiddle]]->pfForm->psName);
		if(nFlag<0)						// less than
			nRight=nMiddle;
		else							// greater than
			nLeft=nMiddle+1;
	}

	// vertex not found, insert new one

	memmove(pnVertexIndices+nLeft+1,pnVertexIndices+nLeft,(*pnCount-nLeft)*sizeof(int));
	ppcVertices[*pnCount]=pcVertex;
	*pnIndex=pnVertexIndices[nLeft]=(*pnCount)++;
	return TRUE;
}

/* GenerateClosestFirst

This version assumed only 2 arguments for the edge predicate and cost function.

Description:
	Do a best first search on a graph described by an edge-predicate and cost function.
	Enumerate the reachable vertices of the graph in order of lowest cost.
Call:
	(closest-first <edge-predicate> <cost-function> start-vertex ?var-vertex ?var-cost [?var-edge-count])
Notes:
	This routine uses a best-first graph search to generate the
	vertices of a graph in nearest first order, relative to the starting vertex.
	The algorithm is complicated somewhat by the fact that we only have a list of edges,
	and no list of vertices to work with.  
	
	The auxiliary routine, InsertVertex is used to create a list of unique vertices on the fly.
	Each time a vertex is encountered, it is passed to InsertVertex.  InsertVertex returns
	TRUE iff the vertex hasn't been seen before.  
	
	If a vertex is new, then best-first-search adds the vertex to its heap or open list.
	Each time a vertex is retrieved from the open list, it is expanded by processing all
	of its adjacent vertices.  Then the retrieved vertex is passed back to the caller.
Data Structures:
	5 arrays are allocated.  pnVertexIndices is maintained in name sorted order, while
	the other arrays are all maintained in allocation order, or in the order that vertices
	were first encountered.
  
	pnVertexIndices - This is the vertex indirection array.  InsertVertex maintains
		this array in name sorted order, so it can determine if a vertex has been seen
		before and report this to GenerateClosestFirst.
	ppcVertices - This is the vertex literal pointer array.  InsertVertex uses the literal
		names to determine if a vertex has been seen before.  GenerateClosestFirst uses
		the literal names to search the edge predicate for more vertices;
	pnDistances - This is the edge count array.  GenerateClosestFirst optionally returns this
		value as one of its arguments
	pdfCosts - This is the total cost array.  GenerateClosestFirst returns this
		value as one of its arguments
	pnHeap - This is the open list.  GenerateClosestFirst maintains a list of vertex
		allocation indices in this array.
*/

BOOL GenerateClosestFirst
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	CFCONTEXTP pgc;						/* context pointer */
	CELLP pcEdge;						/* edge predicate */
	CELLP pcCost;						/* cost function */
	CELLP pcStart;						/* start vertex */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarCost;					/* vertex total cost variable */
	CELLP pcVarDist;					/* (optional) edge count variable */
	
	SYMBOLINFOP psiEdge;				/* edge symbol info pointer */
	SYMBOLINFOP psiCost;				/* cost symbol info pointer */

	int nCount;							/* maximum array size required */
	int nHeapLength;					/* heap size required */
	int nIndex;							/* vertex index (allocation order) */
	void *pvStack;						/* btree search stack context */
	CELLP pcArgs;						/* argument list for edge btree search */
	CELLP pcTuple;						/* edge tuple pointer */
	int nOpenIndex;						/* index of current vertex (allocation order) */
	BTREEP pbt;							/* cost function btree pointer */
	double dfCost;						/* cost function value */
	int nEdgeCount;						/* edge-count value */
	CELLP pcEdgeCount;					/* edge-count */
	int nNode;							/* heap node number */

	/* initialization */

	if(!*ppvContext)
	{
		// edge predicate must be a described predicate... 

		pcEdge=pcGenLit->pfForm->pcArgs;
		if(!pcEdge)
		{
			ErrorMessage("closest-first:  No edge predicate\n");
			return FALSE;
		}
		if(pcEdge->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("closest-first:  Edge argument is not a symbol (described predicate)\n");
			return FALSE;
		}
		psiEdge=pcEdge->pfForm->uValue.psiSymbolInfo;
		if(psiEdge->nSymbolType!=SYMBOL_PREDICATE)
		{
			ErrorMessage("closest-first:  Edge argument is not a predicate\n");
			return FALSE;
		}
		if(psiEdge-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("closest-first:  Edge predicate is not described\n");
			return FALSE;
		}
		if(psiEdge->nArity!=2)
		{
			ErrorMessage("closest-first:  Edge predicate must have arity 2\n");
			return FALSE;
		}

		// cost function must be a described function... 

		pcCost=pcEdge->pcNext;
		if(!pcCost)
		{
			ErrorMessage("closest-first:  No cost function\n");
			return FALSE;
		}
		if(pcCost->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("closest-first:  Cost argument is not a symbol (described function)\n");
			return FALSE;
		}
		psiCost=pcCost->pfForm->uValue.psiSymbolInfo;
		if(psiCost->nSymbolType!=SYMBOL_FUNCTION)
		{
			ErrorMessage("closest-first:  Cost argument is not a function\n");
			return FALSE;
		}
		if(psiCost-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("closest-first:  Cost function is not described\n");
			return FALSE;
		}
		if(psiCost->nArity!=2)
		{
			ErrorMessage("closest-first:  Cost function must have arity 2\n");
			return FALSE;
		}

		// fetch start vertex

		pcStart=pcCost->pcNext;
		if(!pcStart)
		{
			ErrorMessage("closest-first:  No start vertex\n");
			return FALSE;
		}

		// fetch vertex variable
		
		pcVarVertex=pcStart->pcNext;
		if(!pcVarVertex)
		{
			ErrorMessage("closest-first:  No vertex variable\n");
			return FALSE;
		}

		// fetch total cost variable
		
		pcVarCost=pcVarVertex->pcNext;
		if(!pcVarCost)
		{
			ErrorMessage("closest-first:  No cost variable\n");
			return FALSE;
		}

		// fetch edge-count variable
		
		pcVarDist=pcVarCost->pcNext;

		// allocate the generator context block
		
		pgc=(CFCONTEXTP)MemAlloc(sizeof(CFCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->pfMark=(void (*)(GCONTEXTP))MarkClosestFirst;

		// initialize arrays

		pgc->pbtEdge=plpLinearPlan->apbtWorld[psiEdge-asiWorldSymbols];
		pgc->pbtCost=plpLinearPlan->apbtWorld[psiCost-asiWorldSymbols];
		nCount=BTreeCount(pgc->pbtEdge,0)+1;	// at most nEdges+1 vertices can be reached
		for(nHeapLength=1;nHeapLength<nCount;nHeapLength<<=1);	// heap length must be power of 2

		pgc->pnVertexIndices=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->ppcVertices=(CELLP *)MemAlloc(sizeof(CELLP)*nCount);
		pgc->pdfCosts=(double *)MemAlloc(sizeof(double)*nCount);
		pgc->pnEdgeCounts=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->pnHeap=(int *)MemAlloc(sizeof(int)*nHeapLength);

		pgc->nHeapLength=nHeapLength;	// heap length
		pgc->nHeapCount=0;				// heap empty
		pgc->nVertices=0;				// number of vertices

		pgc->pcVarVertex=pcVarVertex;
		pgc->pcVarCost=pcVarCost;
		pgc->pcVarDist=pcVarDist;
		
		// put start vertex on open list

		pcStart=ComputeTerm(pcStart,plpLinearPlan,pbBindings);	// dereference any variables
		InsertVertex(pcStart,			// add start vertex to vertex array
			pgc->pnVertexIndices,
			pgc->ppcVertices,
			&pgc->nVertices,
			&nIndex);
		pgc->pdfCosts[nIndex]=0.0;		// starting cost
		pgc->pnEdgeCounts[nIndex]=0;	// starting edge-count
		VHeapInsert(pgc,nIndex);		// put vertex on the open list
	}
	else
		pgc=(CFCONTEXTP)*ppvContext;

	// enumerate reachable vertices

	while((nOpenIndex=VHeapExtractMin(pgc))!=-1)	// search all open vertices
	{
		// assemble search argument list for this vertex
			
		pcArgs=CopyCell(pgc->ppcVertices[nOpenIndex]);
		pcArgs->pcNext=pgc->pcVarVertex;

		// search edge btree for adjacent vertices
			
		pvStack=0;
		BTreeGeneratorWithBoundVars(pgc->pbtEdge,&pvStack,pcArgs);	// Initialize btree search
		for(pcTuple=BTreeGeneratorWithBoundVars(NULL,&pvStack,pcArgs);pcTuple;
			pcTuple=BTreeGeneratorWithBoundVars(NULL,&pvStack,pcArgs))
		{
			// calculate the total cost to get to this vertex

			pbt=BTreeSearch(pcTuple,pgc->pbtCost);
			if(!pbt)
			{
				ErrorMessage("closest-first:  Cost function does not have cost for every edge.\n");
				return FALSE;
			}
			if(!FormulaToDouble(pbt->pcValue,plpLinearPlan,pbBindings,&dfCost))
			{
				ErrorMessage("closest-first:  Cost function has null or non-numeric value.\n");
				return FALSE;
			}
			dfCost+=pgc->pdfCosts[nOpenIndex];
			nEdgeCount=pgc->pnEdgeCounts[nOpenIndex]+1;

			// insert or "adjust" the vertex

			if(InsertVertex(pcTuple->pcNext,	// add next vertex to vertex array
				pgc->pnVertexIndices,pgc->ppcVertices,&pgc->nVertices,&nIndex))
			{
				pgc->pdfCosts[nIndex]=dfCost;	// store the total cost
				pgc->pnEdgeCounts[nIndex]=nEdgeCount;	// store the total edge-count
				VHeapInsert(pgc,nIndex);	// put vertex on the open list
			}
			else						// this is an old vertex, see if it needs to be "adjusted"
			{
				if(dfCost<pgc->pdfCosts[nIndex]||	// if this path is cheaper
					(dfCost==pgc->pdfCosts[nIndex]&&nEdgeCount<pgc->pnEdgeCounts[nIndex]))
				{
					pgc->pdfCosts[nIndex]=dfCost;	// store the new total cost
					pgc->pnEdgeCounts[nIndex]=nEdgeCount;	// store the total edge-count
					for(nNode=0;nNode<pgc->nHeapCount;nNode++)
						if(pgc->pnHeap[nNode]==nIndex)
							break;
					if(nNode==pgc->nHeapCount)	// if vertex not on open list (this can't happen!)
						Message("closest-first:  Node index not found in heap.\n");
					else
						VHeapPercolate(pgc,nNode+1);	// fix up heap
				}
			}
		}

		// return next vertex, cost (and edge-count)

		if(nOpenIndex)					// don't enumerate start vertex
		{
			SetVarX(pgc->pcVarVertex,pgc->ppcVertices[nOpenIndex],pbBindings);
			pcCost=MakeFloatForm(pgc->pdfCosts[nOpenIndex]);
			SetVarX(pgc->pcVarCost,pcCost,pbBindings);
			if(pgc->pcVarDist)
			{
				pcEdgeCount=MakeIntegerForm(pgc->pnEdgeCounts[nOpenIndex]);
				SetVarX(pgc->pcVarDist,pcEdgeCount,pbBindings);
			}
			return TRUE;
		}
	}

	return FALSE;						// no more vertices to enumerate
}

/* MarkClosestFirst

Description:
	Mark the data structures associated with a closest-first context block.
*/

static void MarkClosestFirst
(
	CFCONTEXTP pgc						// pointer to context block 
)
{
	ZoneMark(pgc->ppcVertices);
	ZoneMark(pgc->pdfCosts);
	ZoneMark(pgc->pnEdgeCounts);
	ZoneMark(pgc->pnHeap);
}

// Vertex Heap Support Routines ------------------------------------------------

/* VHeapInsert

Description:
	Insert a single node into a heap.  We insert the node at the end of the
	heap, then shift as far towards the start as it will go.
*/

static void VHeapInsert
(
	CFCONTEXTP pgc,						// context pointer
	int nVertex							// vertex allocation index to insert into heap 
)
{
	int i;

	assert(pgc->nHeapCount<pgc->nHeapLength);
	pgc->nHeapCount++;
	for(i=pgc->nHeapCount;i>1&&VHeapCompareFn(pgc,pgc->pnHeap[Parent(i)-1],nVertex)==1;i=Parent(i))
		pgc->pnHeap[i-1]=pgc->pnHeap[Parent(i)-1];
	pgc->pnHeap[i-1]=nVertex;
}

/* VHeapExtractMin

Description:
	tract the vertex with the smallest cost.
*/

static int VHeapExtractMin
(
	CFCONTEXTP pgc						// context pointer
)
{
	int nMin;							// allocation index of vertex with minimum cost

	if(pgc->nHeapCount<1)
		return -1;
	nMin=pgc->pnHeap[0];
	--pgc->nHeapCount;
	pgc->pnHeap[0]=pgc->pnHeap[pgc->nHeapCount];
	VHeapify(pgc,1);
	return nMin;
}

/* VHeapify

Description:
	Move a single node to its proper location in the heap.
*/

static void VHeapify
(
	CFCONTEXTP pgc,						// context pointer
	int nNode							// node to put in order
)
{
	int n;								// temporary vertex index
	int nSmallest;						// index of smallest node
	int nLeft,nRight;					// child node indices

	nLeft=Left(nNode);
	nRight=Right(nNode);
	if(nLeft<=pgc->nHeapCount&&VHeapCompareFn(pgc,pgc->pnHeap[nLeft-1],pgc->pnHeap[nNode-1])==-1)
		nSmallest=nLeft;
	else
		nSmallest=nNode;
	if(nRight<=pgc->nHeapCount&&VHeapCompareFn(pgc,pgc->pnHeap[nRight-1],pgc->pnHeap[nSmallest-1])==-1)
		nSmallest=nRight;
	if(nSmallest!=nNode)
	{
		n=pgc->pnHeap[nNode-1];			// exchange nodes
		pgc->pnHeap[nNode-1]=pgc->pnHeap[nSmallest-1];
		pgc->pnHeap[nSmallest-1]=n;
		VHeapify(pgc,nSmallest);		// heapify smallest node
	}
}

/* VHeapPercolate

Description:
	We have just reduced the value of a node in the heap.
	Move it up until the heap property is restored.
*/

static void VHeapPercolate
(
	CFCONTEXTP pgc,						// context pointer
	int nNode							// node to put in order
)
{
	int n;								// temporary vertex index
	int nParent;						// index of smallest node

	while(nNode>1)
	{
		nParent=Parent(nNode);
		if(VHeapCompareFn(pgc,pgc->pnHeap[nParent-1],pgc->pnHeap[nNode-1])==1)
		{
			n=pgc->pnHeap[nNode-1];		// exchange nodes
			pgc->pnHeap[nNode-1]=pgc->pnHeap[nParent-1];
			pgc->pnHeap[nParent-1]=n;
			nNode=nParent;				// continue
		}
		else
			return;
	}
}

/* VHeapCompareFn

Description:
	This routine is used to compare vertices for best-first search.
*/

static int VHeapCompareFn
(
	CFCONTEXTP pgc,						// context pointer
	int nVertex1,						// vertex allocation index
	int nVertex2						// vertex allocation index
)
{
	/* test the cost... cheaper vertices sort earlier */

	if(pgc->pdfCosts[nVertex1]<pgc->pdfCosts[nVertex2])
		return -1;
	if(pgc->pdfCosts[nVertex1]>pgc->pdfCosts[nVertex2])
		return 1;
	if(pgc->pnEdgeCounts[nVertex1]<pgc->pnEdgeCounts[nVertex2])
		return -1;
	if(pgc->pnEdgeCounts[nVertex1]>pgc->pnEdgeCounts[nVertex2])
		return 1;
	return 0;
}

//static void VHeapDump
//(
//	CFCONTEXTP pgc
//)
//{
//	int i;
//	int nVertex;
//
//	// display each vertex in the heap and its cost
//
//	for(i=0;i<pgc->nHeapCount;i++)
//	{
//		nVertex=pgc->pnHeap[i];
//		Message("(%d %s %f) ",nVertex,pgc->ppcVertices[nVertex]->pfForm->psName,pgc->pdfCosts[nVertex]);
//	}
//	Message("\n");
//
//	// verify that the heap is valid
//
//	for(i=1;i<pgc->nHeapCount;i++)
//	{
//		if(pgc->pdfCosts[pgc->pnHeap[Parent(i+1)-1]]>pgc->pdfCosts[pgc->pnHeap[i]])
//			Message("Heap invalid (%d>%d) (%f>%f)\n",Parent(i+1)-1,i,
//				pgc->pdfCosts[pgc->pnHeap[Parent(i+1)-1]],pgc->pdfCosts[pgc->pnHeap[i]]);
//	}
//}

/* GenerateClosestFirstEx ------------------------------------------------------

Description:
	Do a best first search on a graph described by an edge-predicate and cost function.
	Enumerate the reachable vertices of the graph in order of lowest cost.
Call:
	(closest-first 
		(?var-from ?var-to)
		(<edge-predicate> arg1 arg2 ...)
		(<cost-function> arg1 arg2 ...) 
		<start-vertex> ?var-vertex ?var-cost [?var-edge-count])
Notes:
	This routine uses a best-first graph search to generate the
	vertices of a graph in nearest first order, relative to the starting vertex.
	The algorithm is complicated somewhat by the fact that we only have a list of edges,
	and no list of vertices to work with.  
	
	The auxiliary routine, InsertVertex is used to create a list of unique vertices on the fly.
	Each time a vertex is encountered, it is passed to InsertVertex.  InsertVertex returns
	TRUE iff the vertex hasn't been seen before.  
	
	If a vertex is new, then best-first-search adds the vertex to its heap or open list.
	Each time a vertex is retrieved from the open list, it is expanded by processing all
	of its adjacent vertices.  Then the retrieved vertex is passed back to the caller.
Data Structures:
	5 arrays are allocated.  pnVertexIndices is maintained in name sorted order, while
	the other arrays are all maintained in allocation order, or in the order that vertices
	were first encountered.
  
	pnVertexIndices - This is the vertex indirection array.  InsertVertex maintains
		this array in name sorted order, so it can determine if a vertex has been seen
		before and report this to GenerateClosestFirst.
	ppcVertices - This is the vertex literal pointer array.  InsertVertex uses the literal
		names to determine if a vertex has been seen before.  GenerateClosestFirst uses
		the literal names to search the edge predicate for more vertices;
	pnDistances - This is the edge count array.  GenerateClosestFirst optionally returns this
		value as one of its arguments
	pdfCosts - This is the total cost array.  GenerateClosestFirst returns this
		value as one of its arguments
	pnHeap - This is the open list.  GenerateClosestFirst maintains a list of vertex
		allocation indices in this array.
*/

BOOL GenerateClosestFirstEx
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list (ignored) */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	CFXCONTEXTP pgc;					/* context pointer */
	CELLP pcLocal;						/* local variables */
	CELLP pcEdge;						/* edge predicate */
	CELLP pcCost;						/* cost function */
	CELLP pcStart;						/* start vertex */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarCost;					/* vertex total cost variable */
	CELLP pcVarDist;					/* (optional) edge count variable */
	
	SYMBOLINFOP psiEdge;				/* edge symbol info pointer */
//	SYMBOLINFOP psiCost;				/* cost symbol info pointer */

	int nCount;							/* maximum array size required */
	int nIndex;							/* vertex index (allocation order) */
	CELLP pcArgs;						/* argument list for edge btree search */
	CELLP pc;							/* cell pointer */
	int nOpenIndex;						/* index of current vertex (allocation order) */
	BTREEP pbt;							/* cost function btree pointer */
	double dfCost;						/* cost function value */
	int nEdgeCount;						/* edge-count value */
	CELLP pcEdgeCount;					/* edge-count */
	int nNode;							/* heap node number */

	/* initialization */

	if(!*ppvContext)
	{
		// fetch the local variable list
		
		pcLocal=pcGenLit->pfForm->pcArgs;

		// edge predicate must be a described predicate... 

		pcEdge=pcLocal->pcNext;
		if(!pcEdge)
		{
			ErrorMessage("closest-first-ex:  No edge predicate\n");
			return FALSE;
		}
		if(pcEdge->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("closest-first-ex:  Edge argument is not a symbol (described predicate)\n");
			return FALSE;
		}
		psiEdge=pcEdge->pfForm->uValue.psiSymbolInfo;
		if(psiEdge->nSymbolType!=SYMBOL_PREDICATE)
		{
			ErrorMessage("closest-first-ex:  Edge argument is not a predicate\n");
			return FALSE;
		}
		if(psiEdge-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("closest-first-ex:  Edge predicate is not described\n");
			return FALSE;
		}
		if(psiEdge->nArity<2)
		{
			ErrorMessage("closest-first-ex:  Edge predicate arity must be atleast 2\n");
			return FALSE;
		}

		// fetch the cost function 

		pcCost=pcEdge->pcNext;
		if(!pcCost)
		{
			ErrorMessage("closest-first-ex:  No cost function\n");
			return FALSE;
		}
//		if(pcCost->pfForm->nType!=ATOM_SYMBOLINFOP)
//		{
//			ErrorMessage("closest-first-ex:  Cost argument is not a symbol (described function)\n");
//			return FALSE;
//		}
//		psiCost=pcCost->pfForm->uValue.psiSymbolInfo;
//		if(psiCost->nSymbolType!=SYMBOL_FUNCTION)
//		{
//			ErrorMessage("closest-first-ex:  Cost argument is not a function\n");
//			return FALSE;
//		}

		// fetch start vertex

		pcStart=pcCost->pcNext;
		if(!pcStart)
		{
			ErrorMessage("closest-first-ex:  No start vertex\n");
			return FALSE;
		}

		// fetch vertex variable
		
		pcVarVertex=pcStart->pcNext;
		if(!pcVarVertex)
		{
			ErrorMessage("closest-first-ex:  No vertex variable\n");
			return FALSE;
		}

		// fetch total cost variable
		
		pcVarCost=pcVarVertex->pcNext;
		if(!pcVarCost)
		{
			ErrorMessage("closest-first-ex:  No cost variable\n");
			return FALSE;
		}

		// fetch edge-count variable (optional)
		
		pcVarDist=pcVarCost->pcNext;

		// allocate the generator context block
		
		pgc=(CFXCONTEXTP)MemAlloc(sizeof(CFXCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->pfMark=(void (*)(GCONTEXTP))MarkClosestFirstEx;

		// save our parsed context
		
		pgc->pcLocal=pcLocal;
		pgc->pcEdge=pcEdge;
		pgc->pcCost=pcCost;
		pgc->pcVarVertex=pcVarVertex;
		pgc->pcVarCost=pcVarCost;
		pgc->pcVarDist=pcVarDist;

		// allocate BFS context

		pbt=plpLinearPlan->apbtWorld[psiEdge-asiWorldSymbols];
		nCount=BTreeCount(pbt,0)+1;		// at most nEdges+1 vertices can be reached

		pgc->pnVertexIndices=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->ppcVertices=(CELLP *)MemAlloc(sizeof(CELLP)*nCount);
		pgc->pdfCosts=(double *)MemAlloc(sizeof(double)*nCount);
		pgc->pnEdgeCounts=(int *)MemAlloc(sizeof(int)*nCount);
		pgc->pnHeap=(int *)MemAlloc(sizeof(int)*nCount);

		pgc->nHeapLength=nCount;		// heap empty
		pgc->nHeapCount=0;				// heap empty
		pgc->nVertices=0;				// number of vertices

		// put start vertex on open list

		pcStart=ComputeTerm(pcStart,plpLinearPlan,pbBindings);	// dereference any variables
		InsertVertex(pcStart,			// add start vertex to vertex array
			pgc->pnVertexIndices,
			pgc->ppcVertices,
			&pgc->nVertices,
			&nIndex);
		pgc->pdfCosts[nIndex]=0.0;		// starting cost
		pgc->pnEdgeCounts[nIndex]=0;	// starting edge-count
		VHeapInsertEx(pgc,nIndex);		// put vertex on the open list
	}
	else
		pgc=(CFXCONTEXTP)*ppvContext;

	// enumerate reachable vertices

	while((nOpenIndex=VHeapExtractMinEx(pgc))!=-1)	// search all open vertices
	{
		CELL c1,c2;						// temporary cells

		// assemble search argument list for this vertex
			
		pcLocal=CopyCell2(&c1,pgc->pcLocal);
		pcLocal->pcNext=pcLocal->pfForm->pcArgs;
		pcArgs=CopyCell2(&c2,pgc->ppcVertices[nOpenIndex]);
		pcArgs->pcNext=pcLocal->pfForm->pcArgs;
	
		pbBindings=ExtendBindings(pcLocal,pcArgs,pbBindings);	/* push */

		// search edge btree for adjacent vertices
			
		while(GenerateDescPredicate(pgc->pcEdge,(void *)&pbBindings->pgcContext,NULL,plpLinearPlan,pbBindings))
		{
			// calculate the total cost to get to this vertex

			pcCost=ComputeTerm(pgc->pcCost,plpLinearPlan,pbBindings);
			if(!FormulaToDouble(pcCost,plpLinearPlan,pbBindings,&dfCost))
			{
				ErrorMessage("closest-first:  Cost function has null or non-numeric value.\n");
				return FALSE;
			}
			dfCost+=pgc->pdfCosts[nOpenIndex];
			nEdgeCount=pgc->pnEdgeCounts[nOpenIndex]+1;

			// insert or "adjust" the vertex

			pc=ComputeTerm(pcLocal->pfForm->pcArgs,plpLinearPlan,pbBindings);
			if(InsertVertex(pc,			// add next vertex to vertex array
				pgc->pnVertexIndices,pgc->ppcVertices,&pgc->nVertices,&nIndex))
			{
				pgc->pdfCosts[nIndex]=dfCost;	// store the total cost
				pgc->pnEdgeCounts[nIndex]=nEdgeCount;	// store the total edge-count
				VHeapInsertEx(pgc,nIndex);	// put vertex on the open list
			}
			else						// this is an old vertex, see if it needs to be "adjusted"
			{
				if(dfCost<pgc->pdfCosts[nIndex]||	// if this path is cheaper
					(dfCost==pgc->pdfCosts[nIndex]&&nEdgeCount<pgc->pnEdgeCounts[nIndex]))
				{
					pgc->pdfCosts[nIndex]=dfCost;	// store the new total cost
					pgc->pnEdgeCounts[nIndex]=nEdgeCount;	// store the total edge-count
					for(nNode=0;nNode<pgc->nHeapCount;nNode++)
						if(pgc->pnHeap[nNode]==nIndex)
							break;
					if(nNode==pgc->nHeapCount)	// if vertex not on open list (this can't happen!)
						Message("closest-first:  Node index not found in heap.\n");
					else
						VHeapPercolateEx(pgc,nNode+1);	// fix up heap
				}
			}
		}

		// return next vertex, cost (and edge-count)

		if(nOpenIndex)					// don't enumerate start vertex
		{
			SetVarX(pgc->pcVarVertex,pgc->ppcVertices[nOpenIndex],pbBindings);
			pcCost=MakeFloatForm(pgc->pdfCosts[nOpenIndex]);
			SetVarX(pgc->pcVarCost,pcCost,pbBindings);
			if(pgc->pcVarDist)
			{
				pcEdgeCount=MakeIntegerForm(pgc->pnEdgeCounts[nOpenIndex]);
				SetVarX(pgc->pcVarDist,pcEdgeCount,pbBindings);
			}
			return TRUE;
		}
	}

	return FALSE;						// no more vertices to enumerate
}

/* MarkClosestFirstEx

Description:
	Mark the data structures associated with a closest-first-ex context block.
*/

static void MarkClosestFirstEx
(
	CFXCONTEXTP pgc						// pointer to context block 
)
{
	int i;

	ZoneMark(pgc->ppcVertices);
	for(i=0;i<pgc->nVertices;i++)
		MarkFormula(pgc->ppcVertices[i]);
	ZoneMark(pgc->pdfCosts);
	ZoneMark(pgc->pnEdgeCounts);
	ZoneMark(pgc->pnHeap);
}

// Vertex Heap Support Routines ------------------------------------------------

/* VHeapInsertEx

Description:
	Insert a single node into a heap.  We insert the node at the end of the
	heap, then shift as far towards the start as it will go.
*/

static void VHeapInsertEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nVertex							// vertex allocation index to insert into heap 
)
{
	int i;

	assert(pgc->nHeapCount<pgc->nHeapLength);
	pgc->nHeapCount++;
	for(i=pgc->nHeapCount;i>1&&VHeapCompareFnEx(pgc,pgc->pnHeap[Parent(i)-1],nVertex)==1;i=Parent(i))
		pgc->pnHeap[i-1]=pgc->pnHeap[Parent(i)-1];
	pgc->pnHeap[i-1]=nVertex;
}

/* VHeapExtractMinEx

Description:
	Extract the vertex with the smallest cost.
*/

static int VHeapExtractMinEx
(
	CFXCONTEXTP pgc						// context pointer
)
{
	int nMin;							// allocation index of vertex with minimum cost

	if(pgc->nHeapCount<1)
		return -1;
	nMin=pgc->pnHeap[0];
	--pgc->nHeapCount;
	pgc->pnHeap[0]=pgc->pnHeap[pgc->nHeapCount];
	VHeapifyEx(pgc,1);
	return nMin;
}

/* VHeapifyEx

Description:
	Move a single node to its proper location in the heap.
*/

static void VHeapifyEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nNode							// node to put in order
)
{
	int n;								// temporary vertex index
	int nSmallest;						// index of smallest node
	int nLeft,nRight;					// child node indices

	nLeft=Left(nNode);
	nRight=Right(nNode);
	if(nLeft<=pgc->nHeapCount&&VHeapCompareFnEx(pgc,pgc->pnHeap[nLeft-1],pgc->pnHeap[nNode-1])==-1)
		nSmallest=nLeft;
	else
		nSmallest=nNode;
	if(nRight<=pgc->nHeapCount&&VHeapCompareFnEx(pgc,pgc->pnHeap[nRight-1],pgc->pnHeap[nSmallest-1])==-1)
		nSmallest=nRight;
	if(nSmallest!=nNode)
	{
		n=pgc->pnHeap[nNode-1];			// exchange nodes
		pgc->pnHeap[nNode-1]=pgc->pnHeap[nSmallest-1];
		pgc->pnHeap[nSmallest-1]=n;
		VHeapifyEx(pgc,nSmallest);		// heapify smallest node
	}
}

/* VHeapPercolateEx

Description:
	We have just reduced the value of a node in the heap.
	Move it up until the heap property is restored.
*/

static void VHeapPercolateEx
(
	CFXCONTEXTP pgc,					// context pointer
	int nNode							// node to put in order
)
{
	int n;								// temporary vertex index
	int nParent;						// index of smallest node

	while(nNode>1)
	{
		nParent=Parent(nNode);
		if(VHeapCompareFnEx(pgc,pgc->pnHeap[nParent-1],pgc->pnHeap[nNode-1])==1)
		{
			n=pgc->pnHeap[nNode-1];		// exchange nodes
			pgc->pnHeap[nNode-1]=pgc->pnHeap[nParent-1];
			pgc->pnHeap[nParent-1]=n;
			nNode=nParent;				// continue
		}
		else
			return;
	}
}

/* VHeapCompareFnEx

Description:
	This routine is used to compare vertices for best-first search.
*/

static int VHeapCompareFnEx
(
	CFXCONTEXTP pgc,						// context pointer
	int nVertex1,						// vertex allocation index
	int nVertex2						// vertex allocation index
)
{
	/* test the cost... cheaper vertices sort earlier */

	if(pgc->pdfCosts[nVertex1]<pgc->pdfCosts[nVertex2])
		return -1;
	if(pgc->pdfCosts[nVertex1]>pgc->pdfCosts[nVertex2])
		return 1;
	if(pgc->pnEdgeCounts[nVertex1]<pgc->pnEdgeCounts[nVertex2])
		return -1;
	if(pgc->pnEdgeCounts[nVertex1]>pgc->pnEdgeCounts[nVertex2])
		return 1;
	return 0;
}

//static void VHeapDumpEx
//(
//	CFXCONTEXTP pgc
//)
//{
//	int i;
//	int nVertex;
//
//	// display each vertex in the heap and its cost
//
//	for(i=0;i<pgc->nHeapCount;i++)
//	{
//		nVertex=pgc->pnHeap[i];
//		Message("(%d %s %f) ",nVertex,pgc->ppcVertices[nVertex]->pfForm->psName,pgc->pdfCosts[nVertex]);
//	}
//	Message("\n");
//
//	// verify that the heap is valid
//
//	for(i=1;i<pgc->nHeapCount;i++)
//	{
//		if(pgc->pdfCosts[pgc->pnHeap[Parent(i+1)-1]]>pgc->pdfCosts[pgc->pnHeap[i]])
//			Message("Heap invalid (%d>%d) (%f>%f)\n",Parent(i+1)-1,i,
//				pgc->pdfCosts[pgc->pnHeap[Parent(i+1)-1]],pgc->pdfCosts[pgc->pnHeap[i]]);
//	}
//}

/* GenerateLowestFirst ---------------------------------------------------------

Description:
	Calculate the cardinality of a described function, and allocate a pointer
	array.  Point to all of the btree entries.  Sort the array according to
	function value.  Generate the function tuples in sorted order.
Syntax:
	(lowest-first <function> arg1 arg2 ...)
*/

BOOL GenerateLowestFirst
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	LFCONTEXTP pgc;						/* context pointer */
	CELLP pcFunction;					/* described function */
	CELLP pcVarList;					/* pointer to variable list */
	BTREEP pbtFunction;					/* function btree pointer */
	SYMBOLINFOP psi;					/* symbol info pointer */

	int nCount;							/* maximum array size required */
	CELLP pcVar;						/* pointer to variable */
	CELLP pcVal;						/* pointer to value */

	/* initialization */

	if(!*ppvContext)
	{
		// function must be a described function... 

		pcFunction=pcGenLit->pfForm->pcArgs;
		if(!pcFunction)
		{
			ErrorMessage("lowest-first:  No function argument\n");
			return FALSE;
		}
		if(pcFunction->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("lowest-first:  Function argument is not a symbol (described function)\n");
			return FALSE;
		}
		psi=pcFunction->pfForm->uValue.psiSymbolInfo;
		if(psi->nSymbolType!=SYMBOL_FUNCTION)
		{
			ErrorMessage("lowest-first:  Function argument is not a function\n");
			return FALSE;
		}
		if(psi-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("lowest-first:  Function is not described\n");
			return FALSE;
		}

		// fetch first variable
		
		pcVarList=pcFunction->pcNext;
		if(!pcVarList)
		{
			ErrorMessage("lowest-first:  No variables\n");
			return FALSE;
		}

		// allocate the generator context block
		
		pbtFunction=plpLinearPlan->apbtWorld[psi-asiWorldSymbols];
		nCount=BTreeCount(pbtFunction,0);

		pgc=(LFCONTEXTP)MemAlloc(sizeof(LFCONTEXT)+(nCount-1)*sizeof(BTREEP));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->pfMark=(void (*)(GCONTEXTP))MarkLowestFirst;

		// initialize context

		pgc->pcVarList=pcVarList;
		pgc->nCount=nCount;
		pgc->nNext=0;
		BTreeEnumerate(pbtFunction,pgc->apbtIndirect);
		qsort(pgc->apbtIndirect,nCount,sizeof(BTREEP),(int (*)(const void *, const void *))LFCompare);
	}
	else
		pgc=(LFCONTEXTP)*ppvContext;

	// return next tuple

	if(pgc->nNext>=pgc->nCount)
		return FALSE;					// no more tuples to generate
	for(pcVar=pgc->pcVarList,pcVal=pgc->apbtIndirect[pgc->nNext++]->pcKey;pcVar;pcVar=pcVar->pcNext,pcVal=pcVal->pcNext)
		SetVarX(pcVar,pcVal,pbBindings);
	return TRUE;
}

/* MarkLowestFirst

Description:
	Mark the data structures associated with a lowest-first context block.
*/

static void MarkLowestFirst
(
	LFCONTEXTP pgc						// pointer to context block 
)
{
	ZoneMark(pgc->apbtIndirect);
}

/* LFCompare

Description:
	Compare two described function values for qsort.
*/

static int LFCompare
(
	BTREEP *ppbt1,						/* btree node to compare */
	BTREEP *ppbt2						/* btree node to compare */
)
{
	double df1,df2;

	df1=TermToDouble((*ppbt1)->pcValue);
	df2=TermToDouble((*ppbt2)->pcValue);
	if(df1<df2)
		return -1;
	if(df1>df2)
		return 1;
	return 0;
}

/* TermToDouble

Description:
	Convert a literal term to floating point.
*/

static double TermToDouble
(
	CELLP pcValue
)
{
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			return (double)pcValue->pfForm->uValue.nInteger;
		case ATOM_FLOAT:
			return pcValue->pfForm->uValue.dfFloat;
		default:
			ErrorMessage("TermToDouble:  Argument must be numeric %s\n",pcValue->pfForm->psName);
			return 0.0;
	}
}

/* GenerateAllPairsShortestPath ------------------------------------------------

Description:
	Find the shortest paths between all pairs of points on a graph.
	Enumerate the reachable vertices of the graph in order of lowest cost.
Call:
	(all-pairs-shortest-path 
		<vertex-predicate>
		<cost-function>
		?var-vertex1					// enumerated starting vertex
		?var-vertex2					// enumerated ending vertex
		?var-cost						// minimum cost
		?var-next)						// successor vertex on minimum cost path

Notes:
	We use the Floyd-Warshall algorithm.
*/

#define INDEX(a,b) (a*nCount+b)			/* 2D array index macro */

BOOL GenerateAllPairsShortestPath
(
	CELLP pcGenLit,						/* genlit as a formula */
	void **ppvContext, 					/* user context pointer */
	CELLP pcVars, 						/* free variable list */
	LINEARPLANP plpLinearPlan,	 		/* current plan */
	BINDINGP pbBindings 				/* current bindings */
)
{
	APSPCONTEXTP pgc;					/* context pointer */
	CELLP pcVertex;						/* vertex predicate */
	CELLP pcCost;						/* cost function */
	CELLP pcVarVertex1;					/* vertex1 variable */
	CELLP pcVarVertex2;					/* vertex2 variable */
	CELLP pcVarCost;					/* vertex total cost variable */
	CELLP pcVarNext;					/* next vertex variable */
	int nCount=0;							/* maximum array size required */
	
	int i,j,k;

	SYMBOLINFOP psiVertex;				/* vertex symbol info pointer */
	SYMBOLINFOP psiCost;				/* cost symbol info pointer */
	BTREEP pbtVertex;					/* vertex predicate btree pointer */
	BTREEP pbtCost;						/* cost function btree pointer */
	CELLP pcTuple;						/* cost search key */
	CELL c1;							/* temporary cell on stack */
	BTREEP pbt;							/* temporary btree pointer */
	double dfCost;						/* path cost */

	/* initialization */

	if(!*ppvContext)
	{
		// fetch vertex predicate -- must be described

		pcVertex=pcGenLit->pfForm->pcArgs;
		if(!pcVertex)
		{
			ErrorMessage("all-pairs-shortest-path:  No Vertex predicate\n");
			return FALSE;
		}
		if(pcVertex->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("all-pairs-shortest-path:  Vertex argument is not a symbol (described predicate)\n");
			return FALSE;
		}
		psiVertex=pcVertex->pfForm->uValue.psiSymbolInfo;
		if(psiVertex->nSymbolType!=SYMBOL_PREDICATE)
		{
			ErrorMessage("all-pairs-shortest-path:  Vertex argument is not a predicate\n");
			return FALSE;
		}
		if(psiVertex-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("all-pairs-shortest-path:  Vertex predicate is not described\n");
			return FALSE;
		}
		if(psiVertex->nArity!=1)
		{
			ErrorMessage("all-pairs-shortest-path:  Vertex predicate must have arity 1\n");
			return FALSE;
		}

		// fetch cost function -- must be described... 

		pcCost=pcVertex->pcNext;
		if(!pcCost)
		{
			ErrorMessage("all-pairs-shortest-path:  No cost function\n");
			return FALSE;
		}
		if(pcCost->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("all-pairs-shortest-path:  Cost argument is not a symbol (described function)\n");
			return FALSE;
		}
		psiCost=pcCost->pfForm->uValue.psiSymbolInfo;
		if(psiCost->nSymbolType!=SYMBOL_FUNCTION)
		{
			ErrorMessage("all-pairs-shortest-path:  Cost argument is not a function\n");
			return FALSE;
		}
		if(psiCost-asiWorldSymbols>=nNumberOfWorldSymbols)
		{
			ErrorMessage("all-pairs-shortest-path:  Cost function is not described\n");
			return FALSE;
		}
		if(psiCost->nArity!=2)
		{
			ErrorMessage("all-pairs-shortest-path:  Cost function must have arity 2\n");
			return FALSE;
		}

		// fetch vertex1 variable
		
		pcVarVertex1=pcCost->pcNext;
		if(!pcVarVertex1)
		{
			ErrorMessage("all-pairs-shortest-path:  No vertex1 variable\n");
			return FALSE;
		}

		// fetch vertex2 variable
		
		pcVarVertex2=pcVarVertex1->pcNext;
		if(!pcVarVertex2)
		{
			ErrorMessage("all-pairs-shortest-path:  No vertex2 variable\n");
			return FALSE;
		}

		// fetch total cost variable
		
		pcVarCost=pcVarVertex2->pcNext;
		if(!pcVarCost)
		{
			ErrorMessage("all-pairs-shortest-path:  No cost variable\n");
			return FALSE;
		}

		// fetch next vertex variable
		
		pcVarNext=pcVarCost->pcNext;
		if(!pcVarNext)
		{
			ErrorMessage("all-pairs-shortest-path:  No next variable\n");
			return FALSE;
		}

		// allocate the generator context block
		
		pgc=(APSPCONTEXTP)MemAlloc(sizeof(APSPCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		pgc->pfMark=(void (*)(GCONTEXTP))MarkAllPairsShortestPath;

		// initialize arrays

		pbtVertex=plpLinearPlan->apbtWorld[psiVertex-asiWorldSymbols];
		pbtCost=plpLinearPlan->apbtWorld[psiCost-asiWorldSymbols];
		nCount=BTreeCount(pbtVertex,0);

		pgc->nCount=nCount;
		pgc->apbtVertices=(BTREEP *)MemAlloc(nCount*sizeof(BTREEP));
		pgc->pdfCosts=(double *)MemAlloc(nCount*nCount*sizeof(double));	
		pgc->pnNext=(int *)MemAlloc(nCount*nCount*sizeof(int));	

		pgc->pcVarVertex1=pcVarVertex1;
		pgc->pcVarVertex2=pcVarVertex2;
		pgc->pcVarCost=pcVarCost;
		pgc->pcVarNext=pcVarNext;

		pgc->i=0;
		pgc->j=0;

		// fill in the vertex pointer array

		BTreeEnumerate(pbtVertex,pgc->apbtVertices);

		// initialize the arrays
	
		for(i=0;i<nCount;i++)
		{
			for(j=0;j<nCount;j++)
			{
				if(i==j)
					pgc->pdfCosts[INDEX(i,j)]=0.0;	// our cost function may be missing the i==j entries
				else
				{
					pcTuple=CopyCell2(&c1,pgc->apbtVertices[i]->pcKey);
					pcTuple->pcNext=pgc->apbtVertices[j]->pcKey;
					pbt=BTreeSearch(pcTuple,pbtCost);
					if(!pbt)
					{
						ErrorMessage("all-pairs-shortest-path:  Cost function does not have cost for every edge.\n");
						return FALSE;
					}
					if(!FormulaToDouble(pbt->pcValue,plpLinearPlan,pbBindings,&dfCost))
					{
						ErrorMessage("all-pairs-shortest-path:  Cost function has null or non-numeric value.\n");
						return FALSE;
					}
					pgc->pdfCosts[INDEX(i,j)]=dfCost;
				}
				pgc->pnNext[INDEX(i,j)]=j;		// best so far
			}
		}

		// debug

//		for(i=0;i<nCount;i++)
//			Message("%s ",pgc->apbtVertices[i]->pcKey->pfForm->psName);
//		Message("\n");
//
//		for(i=0;i<nCount;i++)
//		{
//			for(j=0;j<nCount;j++)
//				Message("%.2f ",pgc->pdfCosts[INDEX(i,j)]);
//			Message("\n");
//		}
//		Message("\n");

		// find best costs and next vertices for all pairs
	
		for(k=0;k<nCount;k++)
		{
			for(i=0;i<nCount;i++)
			{
				for(j=0;j<nCount;j++)
				{
					if(pgc->pdfCosts[INDEX(i,k)]+pgc->pdfCosts[INDEX(k,j)]<pgc->pdfCosts[INDEX(i,j)])
					{
						pgc->pdfCosts[INDEX(i,j)]=pgc->pdfCosts[INDEX(i,k)]+pgc->pdfCosts[INDEX(k,j)];
						pgc->pnNext[INDEX(i,j)]=pgc->pnNext[INDEX(i,k)];	// new best next
					}
				}
			}
		}

		// debug

//		for(i=0;i<nCount;i++)
//		{
//			for(j=0;j<nCount;j++)
//				Message("%.2f ",pgc->pdfCosts[INDEX(i,j)]);
//			Message("\n");
//		}
//		Message("\n");
//
//		for(i=0;i<nCount;i++)
//		{
//			for(j=0;j<nCount;j++)
//				Message("%d ",pgc->pnNext[INDEX(i,j)]);
//			Message("\n");
//		}
//		Message("\n");
	}
	else
		pgc=(APSPCONTEXTP)*ppvContext;

	// enumerate the results	

	for(i=pgc->i;i<pgc->nCount;i++)
	{
		for(j=pgc->j;j<pgc->nCount;j++)
		{
			// return vertex pair, cost and next vertex
	
			SetVarX(pgc->pcVarVertex1,pgc->apbtVertices[i]->pcKey,pbBindings);
			SetVarX(pgc->pcVarVertex2,pgc->apbtVertices[j]->pcKey,pbBindings);
			pcCost=MakeFloatForm(pgc->pdfCosts[INDEX(i,j)]);
			SetVarX(pgc->pcVarCost,pcCost,pbBindings);
			SetVarX(pgc->pcVarNext,pgc->apbtVertices[pgc->pnNext[INDEX(i,j)]]->pcKey,pbBindings);
			pgc->j=j+1;
			return TRUE;
		}
		pgc->i=i+1;
		pgc->j=0;
	}

	return FALSE;						// no more vertices to enumerate
}

/* MarkAllPairsShortestPath ----------------------------------------------------

Description:
	Mark the data structures associated with an all-pairs-shortest-paths context block.
*/

static void MarkAllPairsShortestPath
(
	APSPCONTEXTP pgc						// pointer to context block 
)
{
	ZoneMark(pgc->apbtVertices);
	ZoneMark(pgc->pdfCosts);
	ZoneMark(pgc->pnNext);
}
