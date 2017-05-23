/*
To retire:  ComputeTerm,  -- this is a real efficiency dog.

*/

/* compute.c -- Support for defined functions

Copyright C, 1997 - 99  F. Bacchus
*/

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "compute.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "makeform.h"
#include "plan.h"
#include "queue.h"
#include "search.h"
#include "tl_tab.h"
#include "tlparse.h"
#include "util.h"
#include "var.h"
#include "world.h"
#include "adl.h"
#include "stdlib.h"

/* global data */

//int nAELength;							/* array element length */

/* Local function prototypes */

/* ----------------------------------------------------------------------------- */

/* ComputeTerms

Description:
	Compute a sequence of terms.
Scheme:

(define (eval-terms terms world/action bindings)
  (map (lambda (term) (eval-term term world/action bindings))
	   terms))

(define (eval-term term world/action bindings)
  (cond
   ((var? term) (lookup-var term bindings))
   ((atom? term) term)
   ((function-term? term)
	(eval-function (get-operator term)
		   (eval-terms (get-args term) world/action bindings)
		   world/action))
   (else (error "Illegal term in formula ~S" term))))
*/

DECLSPEC CELLP ComputeTerms
(
	CELLP pcTerms,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcStart,pcEnd;				/* term listhead */
	CELLP pc1,pc;

	ENTER("ComputeTerms",TRUE);
	pcStart=NULL;						/* initialize listhead */
	pcEnd=(CELLP)&pcStart;

	for(pc1=pcTerms;pc1;pc1=pc1->pcNext)
	{
		pc=ComputeTerm(pc1,plpLinearPlan,pbBindings);
		if(pc)
			pcEnd=pcEnd->pcNext=CopyCell(pc);
		else
		{
			ErrorMessage("Undefined or mis-defined term:\n");
			PrintFormula(stderr,pc1,0);
			PrintFormula(pfTraceStream,pc1,0);
			return 0;
		}
	}
	EXIT("ComputeTerms");
	return pcStart;
}

/*FAHIEM Dec 27th 2000. A New version of Compute Terms. This version must be passed
  an array in which to store the values it computes. It needs to link the array.*/

DECLSPEC BOOL ComputeTermsNoCopy
(
	CELLP pcTerms,
	CELLP pcNewVals,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc1,pc;
	
	ENTER("ComputeTermsNoCopy",TRUE);
	for(pc1=pcTerms;pc1;pc1=pc1->pcNext)
	{
		pc=ComputeTerm(pc1,plpLinearPlan,pbBindings);
		if(pc)
		{
			pcNewVals->pfForm=pc->pfForm;
			pcNewVals=pcNewVals->pcNext=pcNewVals+1;
		}
		else
		{
			ErrorMessage("Undefined or mis-defined term:\n");
			PrintFormula(stderr,pc1,0);
			PrintFormula(pfTraceStream,pc1,0);
			return FALSE;
		}
	}
	--pcNewVals;
	pcNewVals->pcNext=NULL;
	EXIT("ComputeTermsNoCopy");
	return TRUE;
}

/* ComputeTerm

Description:
	Compute a single term.
Note:
	ComputeTerm no longer copies the cell of the returned value.
	If the caller needs a copy, it must call CopyCell itself.
*/

DECLSPEC CELLP ComputeTerm
(
	CELLP pcTerm,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	char ac[40];

	ENTER("ComputeTerm",TRUE);
	if(!pcTerm)							/* handle a vacuous term with grace */
		pc=NULL;
	else if(pcTerm->pfForm->paAction->pfCompute!=NoComputor)	/* if term can be computed */
		pc=(*pcTerm->pfForm->paAction->pfCompute)(pcTerm,plpLinearPlan,pbBindings);
	else if(VarQ(pcTerm))				/* if variable */
	{
#ifdef _DEBUG
		Message("compute-term: No computor for variable\n");
		PrintFormula(stderr,pcTerm,0);
#endif // _DEBUG
		pc=LookupVar(pcTerm,pbBindings);
	}
	else if(!pcTerm->pfForm->pcArgs) 	/* if literal formula */
	{
#ifdef _DEBUG
		Message("compute-term: No computor for literal\n");
		PrintFormula(stderr,pcTerm,0);
#endif // _DEBUG
		pc=pcTerm;
	}
	else
	{
		ErrorMessage("Invalid term \"%s\" in formula\n",GetName(pcTerm->pfForm,ac));
		pc=NULL;
	}
	EXIT("ComputeTerm");
	return pc;
}

/* ComputeLiteral

Description:
	Return a copy of an atomic literal.
*/

/*
xxx add routines to allocate and deallocate cell/term pairs, separate
from the usual memory allocation routines.
look into btrees and print etc, to see if we can special case, to avoid copying
returned litvals.
*/

CELLP ComputeLiteral
(
	CELLP pcLiteral,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeLiteral",TRUE);
	if(!pcLiteral)						/* handle a vacuous term with grace */
	{
		EXIT("ComputeLiteral");
		return NULL;
	}
	if(VarQ(pcLiteral))					/* debug */
//		pc=CopyCell(LookupVar(pcLiteral,pbBindings));
		pc=LookupVar(pcLiteral,pbBindings);
	else	
//		pc=CopyCell(pcLiteral);
		pc=pcLiteral;
	EXIT("ComputeLiteral");
	return pc;
}

/* ComputeVariable

Description:
	Return a variable's binding.
*/

CELLP ComputeVariable
(
	CELLP pcVariable,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeVariable",TRUE);
	if(!pcVariable)						/* handle a vacuous term with grace */
	{
		EXIT("ComputeVariable");
		return NULL;
	}
//	pc=CopyCell(LookupVar(pcVariable,pbBindings));
	pc=LookupVar(pcVariable,pbBindings);
	EXIT("ComputeVariable");
	return pc;
}

/* ComputeArray

Description:
	Return the value of an array element.
*/

CELLP ComputeArray
(
	CELLP pcArray,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int nIndex;							/* calculated linear index */
	ARRAYINFOP pai;

	ENTER("ComputeArray",TRUE);
	if(!pcArray)						/* handle a vacuous term with grace */
	{
		EXIT("ComputeArray");
		return NULL;
	}
	pc=LookupVar(pcArray,pbBindings);	/* point to array in bindings list */
	if(ArrayIndex(pcArray,pc,plpLinearPlan,pbBindings,&nIndex))
	{
		pai=pc->pfForm->uValue.paiArrayInfo;
		pc=(CELLP)MemAlloc(sizeof(CELL));
		pc->pfForm=pai->ppcArray[nIndex]->pfForm;
		EXIT("ComputeArray");
		return pc;
	}
	EXIT("ComputeArray");
	return NULL;
}

/* ArrayIndex

Description:
	Calculate the linear index of an array element reference.
*/

BOOL ArrayIndex
(
	CELLP pcArray,						/* array reference */
	CELLP pcBinding,					/* array binding */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	int *nOffset						/* returned index */
)
{
	CELLP pc;
	int n;								/* number of indices */
	int i;								/* loop index */
	int nIndex;							/* array index */
	int nOff;							/* calculated linear index */
	ARRAYINFOP pai;
	char ac[40];						/* string buffer */

	ENTER("ArrayIndex",TRUE);
	if(pcBinding->pfForm->nType!=ATOM_ARRAYINFOP)
		ErrorMessage("Invalid array reference, %s is not bound to an array\n",pcArray);
	pai=pcBinding->pfForm->uValue.paiArrayInfo;
	if(!pai)
	{
		ErrorMessage("Array reference to non-array %s\n",GetName(pcArray->pfForm,ac));
		EXIT("ArrayIndex");
		return FALSE;
	}
	n=pai->nDimension;
	i=0;								/* count up dimensions */
	nOff=0;
	for(pc=pcArray->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if(i>=n)
		{
			ErrorMessage("Too many indices for array %s\n",GetName(pc->pfForm,ac));
			EXIT("ArrayIndex");
			return FALSE;
		}
		if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nIndex))
		{
			ErrorMessage("Invalid index %d for array %s\n",i,GetName(pc->pfForm,ac));
			EXIT("ArrayIndex");
			return FALSE;
		}
		if(nIndex<0||nIndex>=pai->pnLimits[i])
		{
			ErrorMessage("Index %d out of range for array %s\n",i,GetName(pc->pfForm,ac));
			EXIT("ArrayIndex");
			return FALSE;
		}
		nOff+=pai->pnMultipliers[i]*nIndex;
		i++;
	}
	if(i!=n)
	{
		ErrorMessage("Too few indices for array %s\n",GetName(pcArray->pfForm,ac));
		EXIT("ArrayIndex");
		return FALSE;
	}
	*nOffset=nOff;
	EXIT("ArrayIndex");
	return TRUE;
}

/* ComputeDescMacro

Description:
	Compute a described macro.
	We simply substitute the value for the macro.
*/

CELLP ComputeDescMacro
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	SYMBOLINFOP psiSymbolInfo;
	CELLP pc;
	
	ENTER("ComputeDescMacro",TRUE);
	psiSymbolInfo=pcFormula->pfForm->uValue.psiSymbolInfo;
	pc=CopyCellList(psiSymbolInfo->pcFormula);
	EXIT("ComputeDescMacro");
	return pc;
}

/* ComputeDefFunction

Description:
	Compute a defined function.
	We "push a context for local variables" and then evaluate the formula.
*/

CELLP ComputeDefFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	BINDINGP pb;
	CELLP pc,pcFunction;
	CELLP pcTerms;
	FORMULAP pf;
	SYMBOLINFOP psiSymbolInfo;

	ENTER("ComputeDefFunction",TRUE);
	psiSymbolInfo=pcFormula->pfForm->uValue.psiSymbolInfo;

	/* declare function name in bindings list for returned value */
	
	pcFunction=(CELLP)MemAlloc(sizeof(CELL));
	pcFunction->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
//	pf->psName=IdentAlloc(psiSymbolInfo->psName);
	pf->psName=psiSymbolInfo->psName;
	pb=ExtendBindings(pcFunction,pcFunction,pbBindings);

	/* bind arguments to formal parameters */

	if(pcFormula->pfForm->pcArgs)
	{
		pcTerms=ComputeTerms(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
		if(!pcTerms)
			TermError("compute-def-function",pcFormula,pbBindings);	
		pb=ExtendBindings(GetDefParameters(psiSymbolInfo),pcTerms,pb);
	}

	/* add local variables to the bindings list */
	
	pc=GetDefLocals(psiSymbolInfo);
	if(pc)
	{
		pc=ExpandArrays(pc,plpLinearPlan,pb);
		pb=ExtendBindings(pc,pc,pb);
	}

	/* evaluate the formula (for side effects) */

	pc=GetDefFormula(psiSymbolInfo);
	b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pb);
	if(!b)
		ErrorMessage("Function %s terminated abnormally\n",psiSymbolInfo->psName);

	/* extract the returned value */	
	
	pc=LookupVar(pcFunction,pb);
	if(FormulaEqQ(pc,pcFunction))
	{
		ErrorMessage("No return value for function %s\n",psiSymbolInfo->psName);
		longjmp(*pjbCurrentJump,1);		/* tear down stack */
	}
	EXIT("ComputeDefFunction");
	return pc;
}

/* ComputeDefMacro

Description:
	Compute a defined macro.
	We "push a context for local variables" and then evaluate the formula.
*/

CELLP ComputeDefMacro
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	BINDINGP pb;
	CELLP pc,pcMacro;
	CELLP pcTerms;
	FORMULAP pf;
	SYMBOLINFOP psiSymbolInfo;

	ENTER("ComputeDefMacro",TRUE);
	psiSymbolInfo=pcFormula->pfForm->uValue.psiSymbolInfo;

	/* declare macro name in bindings list for returned value */
	
	pcMacro=(CELLP)MemAlloc(sizeof(CELL));
	pcMacro->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
//	pf->psName=IdentAlloc(psiSymbolInfo->psName);
	pf->psName=psiSymbolInfo->psName;
	pb=ExtendBindings(pcMacro,pcMacro,pbGlobalVariables);

	/* bind arguments to formal parameters */

	if(pcFormula->pfForm->pcArgs)
	{
		pcTerms=ComputeTerms(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
		if(!pcTerms)
			TermError("compute-def-macro",pcFormula,pbBindings);	
		pb=ExtendBindings(GetDefParameters(psiSymbolInfo),pcTerms,pb);
	}

	/* add local variables to the bindings list */
	
	pc=GetDefLocals(psiSymbolInfo);
	if(pc)
	{
		pc=ExpandArrays(pc,plpLinearPlan,pb);
		pb=ExtendBindings(pc,pc,pb);
	}

	/* evaluate the formula (for side effects) */

	pc=GetDefFormula(psiSymbolInfo);
	b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pb);
	if(!b)
		ErrorMessage("Macro %s terminated abnormally\n",psiSymbolInfo->psName);

	/* extract the returned value */	
	
	pc=LookupVar(pcMacro,pb);
	if(FormulaEqQ(pc,pcMacro))
	{
		ErrorMessage("No return value for macro %s\n",psiSymbolInfo->psName);
		longjmp(*pjbCurrentJump,1);		/* tear down stack */
	}
	EXIT("ComputeDefMacro");
	return pc;
}

/* ExpandArrays

Description:
	Locate any arrays in a local variable list, and expand them in place.
*/

CELLP ExpandArrays
(
	CELLP pcVars,						/* local variable list */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcStart,pcEnd;				/* formula listhead */
	CELLP pcZero;

	/* if no arrays, return original list */

	for(pc=pcVars;pc;pc=pc->pcNext)
		if(pc->pfForm->pcArgs)			/* if array */
			break;
	if(!pc)
		return pcVars;

	/* copy list, expanding arrays */

	pcZero=MakeIntegerForm(0);
	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;
	for(pc=pcVars;pc;pc=pc->pcNext)
	{
		if(pc->pfForm->pcArgs)			/* if array */
			pcEnd=pcEnd->pcNext=MakeArray(pc,pcZero,plpLinearPlan,pbBindings);
		else
			pcEnd=pcEnd->pcNext=CopyCell(pc);
	}
	return pcStart;
}

/* MakeArray

Description:
	Create an array and initialize its elements.
*/

CELLP MakeArray
(
	CELLP pcFormula,					/* formula to convert */
	CELLP pcValue,						/* atomic initialization value */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int n;								/* dimension count */
	int i;								/* array index */
	int nMultiplier;					/* running dimension product */
	CELLP pc;
	FORMULAP pf;
	ARRAYINFOP pai;
	CELLP *ppc;							/* array pointer */
	int nPosition;

	ENTER("MakeArray",FALSE);
	pai=(ARRAYINFOP)MemAlloc(sizeof(ARRAYINFO));
	pai->psName=IdentAllocAndPos(pcFormula->pfForm->psName,&nPosition);
	pai->nHashPos=nPosition;
	
	n=0;								/* calculate array dimension */
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
		n++;
	pai->nDimension=n;
	pai->pnLimits=(int *)MemAlloc(n*sizeof(int));
	pai->pnMultipliers=(int *)MemAlloc(n*sizeof(int));
	i=0;								/* fill in array limits */
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
		FormulaToInteger(pc,plpLinearPlan,pbBindings,pai->pnLimits+i++);
	nMultiplier=1;						/* fill in offset multipliers */
	for(i=n-1;i>=0;i--)
	{
		pai->pnMultipliers[i]=nMultiplier;
		nMultiplier*=pai->pnLimits[i];
	}
	pai->nCount=nMultiplier;

	/* allocate and fill array */

	ppc=pai->ppcArray=(CELLP *)MemAlloc(nMultiplier*sizeof(CELLP));
	for(i=0;i<nMultiplier;i++)
		*ppc++=pcValue;

	/* create the array formula */

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_ARRAYINFOP;
	pf->psName=IdentName(pcFormula->pfForm);
	pf->uValue.paiArrayInfo=pai;
	
	return pc;
}

/* ComputeGoal

Description:
	Compute a function in the goal world.
*/

CELLP ComputeGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGoal",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	pc=(*pc->pfForm->paAction->pfCompute)(pc,GetGoalWorld(TRUE),pbBindings);
	EXIT("ComputeGoal");
	return pc;
}

/* ComputeCurrent

Description:
	Compute a function in the current world.
*/

CELLP ComputeCurrent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeCurrent",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	pc=(*pc->pfForm->paAction->pfCompute)(pc,plpCurrentPlan,pbBindings);
	EXIT("ComputeCurrent");
	return pc;
}

/* ComputePrevious

Description:
	Compute a function in the previous world.
*/

CELLP ComputePrevious
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp;
	CELLP pc;

	ENTER("ComputePrevious",TRUE);

	/* if the current plan is not null, get the previous plan */
	
	plp=plpLinearPlan;
	if(plp)
		plp=plp->plpParent;

	/* if the previous plan exists, compute the passed formula, 
	otherwise return FALSE */
	
	if(plp)
	{
		pc=pcFormula->pfForm->pcArgs;
		pc=(*pc->pfForm->paAction->pfCompute)(pc,plp,pbBindings);
	}
	else
		pc=MakeIntegerForm(0);
	EXIT("ComputePrevious");
	return pc;
}

/* ComputeAStar

Description:
	Compute an a* heuristic cost function.
*/

CELLP ComputeAStar
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	double dfValue;

	ENTER("ComputeAStar",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(!pc)
	{
		pc=MakeFloatForm(plpLinearPlan->dfCost);
		EXIT("ComputeAStar");
		return pc;
	}
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfValue))
	{
		ErrorMessage("compute-a*:  Formula must evaluate to a number.\n");
		pc=MakeFloatForm(plpLinearPlan->dfCost);
		EXIT("ComputeAStar");
		return pc;
	}
	pc=MakeFloatForm(dfValue+plpLinearPlan->dfCost);
	EXIT("ComputeAStar");
	return pc;
}




/* ComputeOptimalCost

Description:
	Compute an optimal-cost heuristic cost function.
*/

CELLP ComputeOptimalCost
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeOptimalCost",TRUE);
	pc=MakeFloatForm(plpLinearPlan->dfCost);
	EXIT("ComputeOptimalCost");
	return pc;
}

/* ComputeBestAction

Description:
	Compute a best-action priority function.
*/

CELLP ComputeBestAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeBestAction",TRUE);
	pc=MakeFloatForm(LinearPlanActionPriority(plpLinearPlan));
	EXIT("ComputeBestAction");
	return pc;
}

/* ComputePlanLength

Description:
	Compute the plan-length function.
	Plan-length is the current depth of search.
*/

CELLP ComputePlanLength
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	/* check user input */

	ENTER("ComputePlanLength",FALSE);
	pc=MakeIntegerForm(plpLinearPlan->nLength);
	EXIT("ComputePlanLength");
	return pc;
}

/* ComputePlanCost

Description:
	Compute the plan-cost function.
	Plan-cost is the sum of costs encountered while achieving the current state.
*/

CELLP ComputePlanCost
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputePlanCost",FALSE);
	pc=MakeFloatForm(plpLinearPlan->dfCost);
	EXIT("ComputePlanCost");
	return pc;
}

/* ComputePlanDuration

Description:
	Compute the plan-duration function.
	Plan-duration is the sum of operation times required to achieve the current state.
*/

CELLP ComputePlanDuration
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputePlanDuration",FALSE);
	pc=MakeFloatForm(plpLinearPlan->dfTime);
	EXIT("ComputePlanDuration");
	return pc;
}

/* ComputeActionName

Description:
	Compute the action-name function.
*/

CELLP ComputeActionName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeActionName",FALSE);
	pc=CopyCell(plpLinearPlan->pcActionName);
	EXIT("ComputeActionName");
	return pc;
}

/* ComputeActionCost

Description:
	Compute the action-cost function.
*/

CELLP ComputeActionCost
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeActionCost",FALSE);
	pc=MakeFloatForm(plpLinearPlan->dfActionCost);
	EXIT("ComputeActionCost");
	return pc;
}

/* ComputeActionDuration

Description:
	Compute the action-duration function.
*/

CELLP ComputeActionDuration
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeActionDuration",FALSE);
	pc=MakeFloatForm(plpLinearPlan->dfActionDuration);
	EXIT("ComputeActionDuration");
	return pc;
}

/* ComputeActionPriority

Description:
	Compute the action-priority function.
*/

CELLP ComputeActionPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeActionPriority",FALSE);
	pc=MakeFloatForm(plpLinearPlan->dfActionPriority);
	EXIT("ComputeActionPriority");
	return pc;
}

/* ComputeWorldNumber

Description:
	Compute the world-number function.
	World-number is the generation index of the current world.
*/

CELLP ComputeWorldNumber
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldNumber",FALSE);
	pc=MakeIntegerForm(plpLinearPlan->nWorldNumber);
	EXIT("ComputeWorldNumber");
	return pc;
}


CELLP ComputeWorldCounter
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldCounter",FALSE);
	pc=MakeIntegerForm(nWorldNumber);
	EXIT("ComputeWorldCounter");
	return pc;
}

CELLP ComputeClosedlistLength
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeOpenlistLength",FALSE);
	pc=MakeIntegerForm(PlanCount(plpClosed));
	EXIT("ComputeOpenlistLength");
	return pc;
}



///* ComputeWorldHeuristicRank
//
//Description:
//	Compute the world-number function.
//	World-number is the generation index of the current world.
//*/
//
//CELLP ComputeWorldHeuristicRank
//(
//	CELLP pcFormula,
//	LINEARPLANP plpLinearPlan,
//	BINDINGP pbBindings
//)
//{
//	CELLP pc;
//
//	ENTER("ComputeWorldHeuristicRank",FALSE);
//	if(pcHeuristic)
//		pc=MakeFloatForm(plpLinearPlan->dfHeuristic);
//	else
//		pc=MakeIntegerForm(plpLinearPlan->nWorldNumber);
//	EXIT("ComputeWorldHeuristicRank");
//	return pc;
//}

/* ComputeGetPlanName

Description:
	Return the plan name string.
*/

CELLP ComputeGetPlanName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetPlanName",FALSE);
	pc=MakeStringForm(psPlanName);
	EXIT("ComputeGetPlanName");
	return pc;
}

/* ComputePlanStatus

Description:
	Return the plan-status string.
*/

CELLP ComputePlanStatus
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputePlanStatus",FALSE);
	pc=MakeStringForm(srSearchResult.psPlanStatus);
	EXIT("ComputePlanStatus");
	return pc;
}

/* ComputeWorldsGenerated

Description:
	Return the number of worlds generated.
*/

CELLP ComputeWorldsGenerated
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldsGenerated",FALSE);
	pc=MakeIntegerForm(srSearchResult.nWorldsGenerated);
	EXIT("ComputeWorldsGenerated");
	return pc;
}

/* ComputeWorldsSearched

Description:
	Return the number of worlds searched.
*/

CELLP ComputeWorldsSearched
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldsSearched",FALSE);
	pc=MakeIntegerForm(srSearchResult.nWorldsSearched);
	EXIT("ComputeWorldsSearched");
	return pc;
}

/* ComputeWorldsPruned

Description:
	Return the number of worlds pruned. 
*/

CELLP ComputeWorldsPruned
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldsPruned",FALSE);
	pc=MakeIntegerForm(srSearchResult.nWorldsPruned);
	EXIT("ComputeWorldsPruned");
	return pc;
}

/* ComputeWorldsDiscarded

Description:
	Return the number of worlds discarded.
*/

CELLP ComputeWorldsDiscarded
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldsDiscarded",FALSE);
	pc=MakeIntegerForm(srSearchResult.nWorldsDiscarded);
	EXIT("ComputeWorldsDiscarded");
	return pc;
}

/* ComputeWorldsUnexamined

Description:
	Return the number of worlds unexamined.
*/

CELLP ComputeWorldsUnexamined
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeWorldsUnexamined",FALSE);
	pc=MakeIntegerForm(srSearchResult.nWorldsUnexamined);
	EXIT("ComputeWorldsUnexamined");
	return pc;
}

/* ComputePlanCPUTime

Description:
	Return the CPU time required for the last search.
*/

CELLP ComputePlanCPUTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputePlanCPUTime",FALSE);
	pc=MakeFloatForm(srSearchResult.dfPlanCPUTime);
	EXIT("ComputePlanCPUTime");
	return pc;
}

/* ComputeGetCPUTime

Description:
	Return the CPU time (since this program started).
*/

CELLP ComputeGetCPUTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetCPUTime",FALSE);
	pc=MakeFloatForm(GetInternalRunTime());
	EXIT("ComputeGetCPUTime");
	return pc;
}

/* ComputeGetBestRelaxedMetric

Description:
	Return the best relaxed metric component in a plan
*/

CELLP ComputeGetBestRelaxedMetric
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetBestRelaxedMetric",TRUE);

	if (!plpLinearPlan) {
	    ErrorMessage("compute-get-best-relaxed-metric:  There is no plan!\n");
	    EXIT("ComputeGetBestRelaxedMetric");
	    return NULL;
	}

	pc=MakeFloatForm(plpLinearPlan->dfBestRelMetric);
	EXIT("ComputeGetBestRelaxedMetric");
	return pc;
}


/* ComputeGetNumberWorldFixedPoints

Description:
	Return the number of world (i.e. not including unrelaxed fluents) fixed points. 
        This value is defined only by some of the heuristics.
*/


CELLP ComputeGetNumberWorldFixedPoints
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetNumberWorldFixedPoints",TRUE);

	if (!plpLinearPlan) {
	    ErrorMessage("compute-get-no-world-fixed-points:  There is no plan!\n");
	    EXIT("ComputeGetBestRelaxedMetric");
	    return NULL;
	}

	pc=MakeIntegerForm(nFixPoints);
	EXIT("ComputeGetNumberWorldFixedPoints");
	return pc;
}


/* ComputeGetNumberWorldFixedPoints

Description:
	Return the number of real (i.e. including unrelaxed fluents) fixed points. 
        This value is defined only by some of the heuristics.
*/


CELLP ComputeGetNumberRealFixedPoints
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetNumberWorldFixedPoints",TRUE);

	if (!plpLinearPlan) {
	    ErrorMessage("compute-get-no-real-fixed-points:  There is no plan!\n");
	    EXIT("ComputeGetBestRelaxedMetric");
	    return NULL;
	}

	pc=MakeIntegerForm(nRealFixPoints);
	EXIT("ComputeGetNumberWorldFixedPoints");
	return pc;
}


/* ComputeGetBestPreferenceValue

Description:
	Return the best relaxed metric component in a plan
*/

CELLP ComputeGetBestPreferenceValue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetBestPreferenceValue",TRUE);

	if (!plpLinearPlan) {
	    ErrorMessage("compute-get-best-preference-value:  There is no plan!\n");
	    EXIT("ComputeGetBestPreferenceValue");
	    return NULL;
	}

	pc=MakeFloatForm(plpLinearPlan->dfBestPreferenceValue);
	EXIT("ComputeGetBestPreferenceValue");
	return pc;
}



/* ComputeGetSearchDepthLimit

Description:
	Return the current search depth limit. 
*/

CELLP ComputeGetSearchDepthLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetSearchDepthLimit",FALSE);
	pc=MakeIntegerForm(nSearchDepthLimit);
	EXIT("ComputeGetSearchDepthLimit");
	return pc;
}

/* ComputeSearchMaxDepth

Description:
	Return the maximum search depth.
*/

CELLP ComputeSearchMaxDepth
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeSearchMaxDepth",FALSE);
	pc=MakeIntegerForm(srSearchResult.nSearchMaxDepth);
	EXIT("ComputeSearchMaxDepth");
	return pc;
}

/* ComputeGetSearchHeuristicLimit

Description:
	Return the search heuristic limit.
*/

CELLP ComputeGetSearchHeuristicLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeGetSearchHeuristicLimit",FALSE);
	pc=MakeFloatForm(dfSearchHeuristicLimit);
	EXIT("ComputeGetSearchHeuristicLimit");
	return pc;
}

/* ComputeSearchMaxHeuristic

Description:
	Return the maximum search heuristic.
*/

CELLP ComputeSearchMaxHeuristic
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("ComputeSearchMaxHeuristic",FALSE);
	pc=MakeFloatForm(srSearchResult.dfSearchMaxHeuristic);
	EXIT("ComputeSearchMaxHeuristic");
	return pc;
}

/* ComputeOpenFile

Description:
	Open a file
*/

CELLP ComputeOpenFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcName;						/* file name */
	CELLP pcStatus;						/* open status */
	CELLP pc;
	int nFile;							/* file index */
	char *psStatus;						/* status string */
	char ac[40];						/* string buffer */

	pc=pcFormula->pfForm->pcArgs;
	pcName=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcName)
		TermError("open-file",pcFormula,pbBindings);
	pc=pc->pcNext;
	pcStatus=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcName)
		TermError("open-file",pcFormula,pbBindings);

	psStatus=IdentName(pcStatus->pfForm);
	if(StringEqQ(psStatus,apsStringTab[STRING_WRITE]))
		psStatus="w";
	else if(StringEqQ(psStatus,apsStringTab[STRING_APPEND]))
		psStatus="a";
	else if(StringEqQ(psStatus,apsStringTab[STRING_READ]))
		psStatus="r";
	else
	{
		ErrorMessage("open-file:  Unsupported access status: %s\n",psStatus);
		return NULL;	}

	for(nFile=MINFILE+1;nFile<MAXFILE;nFile++)
	{
		if(!aftFileTable[nFile].pfFile)
		{
			aftFileTable[nFile].pfFile=fopen(GetName(pcName->pfForm,ac),psStatus);
			if(!aftFileTable[nFile].pfFile)
			{
				ErrorMessage("open-file:  Failed to open file %s for %s access\n",
					GetName(pcName->pfForm,ac),GetName(pcStatus->pfForm,ac));
				return NULL;
			}
			aftFileTable[nFile].cStatus=*psStatus;
			aftFileTable[nFile].bStdio=FALSE;
			return MakeIntegerForm(nFile);
		}
	}
	ErrorMessage("open-file:  No more file handles available.\n");
	return NULL;
}


/* ComputePrintString

Description: 
        Returns a string given a format specification and a set of variables
	(print-string <format> <args...>)
*/


CELLP ComputePrintString
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
  CELLP pc;
  	char acBuffer[512];					/* output buffer */
	CELLP pcStream;
	CELLP pcFormat;
	CELLP pcArgs;
	char *psFormat;						/* format string */
	
	ENTER("EvalPrintString",FALSE);

	/* check arguments */
	
	pcStream=pcFormula->pfForm->pcArgs;
	if(!pcStream)
	{
		ErrorMessage("print-string:  No arguments specified\n");
		return NULL;
	}
	pcFormat=pcStream;
	if(!pcFormat)
	{
		ErrorMessage("print-string:  No format specified\n");
		return NULL;
	}
	if(!FormulaToString(pcFormat,plpLinearPlan,pbBindings,&psFormat))
		return NULL;
	if(!psFormat||!strlen(psFormat))
	{
		ErrorMessage("print:  No format specified\n");
		return NULL;
	}

	pcArgs=pcFormat->pcNext;
	FormatText(acBuffer,psFormat,pcArgs,plpLinearPlan,pbBindings);
	if(strlen(acBuffer)>sizeof(acBuffer))
	{
		ErrorMessage("print:  Output overran internal buffer (%d bytes), exiting\n",
			sizeof(acBuffer));
		exit(1);
	}
	
	pc=MakeStringForm(acBuffer);
	EXIT("EvalPrintString");

	return pc;
}



/* ComputeMakeLiteral

Description:
	Generate a literal name from a format specification and arguments.
*/

CELLP ComputeMakeLiteral
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	LITVAL lv;
	CELLP pc;
	char acBuffer[256];
	char *psFormat;

	ENTER("ComputeMakeLiteral",FALSE);

	/* check arguments */
	
	if(!pcFormula->pfForm->pcArgs)
	{
		ErrorMessage("compute-make-literal:  No arguments specified\n");
		EXIT("ComputeMakeLiteral");
		return NULL;
	}
	FormulaToString(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&psFormat);
	if(!psFormat||!strlen(psFormat))
	{
		ErrorMessage("compute-make-literal:  No format specified\n");
		EXIT("ComputeMakeLiteral");
		return NULL;
	}
				
	FormatText(acBuffer,psFormat,
		pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings);

	lv.psString=acBuffer;
	pc=MakeLiteralForm(ATOM_IDENT,lv);
	EXIT("ComputeMakeLiteral");
	return pc;
}

