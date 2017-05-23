/* makeform.c

Copyright C, 1998 - 2001 Fahiem Bacchus

Description:
	Make routines, to create formulas from other formulas.
	These are internal (fast) routines that do no error checking.
*/


/*FB Jan2001. 
  1. Optimize away IdentAlloc.
  2. Adopt a general level of hygiene for makeformulas.  In
     particular, we don't allow any makeform to alter its
     arguments.
*/

#include <stdio.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "domain.h"
#include "formula.h"
#include "makeform.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"

/* local function prototypes */

/* global data */

CELLP pcTrue;
CELLP pcFalse;

/* Non-Atomic Formula Constructors --------------------------------------------- */

/* MakeTrueForm

Description:
	Make a TRUE formula
Scheme:
*/

void InitTrueForm(void)
{
	FORMULAP pf;

	ENTER("InitTrueForm",FALSE);
	pcTrue=(CELLP)MemAlloc(sizeof(CELL));
	pcTrue->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aTrueAction;
	pf->psName=pf->paAction->psName;
	EXIT("InitTrueForm");
}

CELLP MakeTrueForm(void)
{
	CELLP pc;

	ENTER("MakeTrueForm",FALSE);
	pc=pcTrue;
	EXIT("MakeTrueForm");
	return pc;
}

/* MakeFalseForm

Description:
	Make a FALSE formula
Scheme:
*/

void InitFalseForm(void)
{
	FORMULAP pf;

	ENTER("InitFalseForm",FALSE);
	pcFalse=(CELLP)MemAlloc(sizeof(CELL));
	pcFalse->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aFalseAction;
	pf->psName=pf->paAction->psName;
	EXIT("InitFalseForm");
}

CELLP MakeFalseForm(void)
{
	CELLP pc;

	ENTER("MakeFalseForm",FALSE);
	pc=pcFalse;
	EXIT("MakeFalseForm");
	return pc;
}

/* MakeAndForm

Description:
	Make an AND formula
Scheme:

(define (make-and-form args)
  (cons 'AND args))
*/

DECLSPEC CELLP MakeAndForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeAndForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aAndAction;
	pf->psName=pf->paAction->psName;
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("MakeAndForm");
	return pc;
}

/* MakeNotForm

Description:
	Make a NOT formula.
Scheme:

(define (make-not-form arg)
  (list 'NOT arg))
*/

CELLP MakeNotForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeNotForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aNotAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArgs);
	}
	else
	{
		pf->pcArgs=pcArgs;
		pcArgs->pcNext=NULL;
	}
	EXIT("MakeNotForm");
	return pc;
}

/* MakeOrForm

Description:
	Make an OR formula.
Scheme:

(define (make-or-form args)
  (cons 'OR args))
*/

CELLP MakeOrForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeOrForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aOrAction;
	pf->psName=pf->paAction->psName;
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("MakeOrForm");
	return pc;
}

/* MakeXorForm

Description:
	Make an XOR formula.
Scheme:

(define (make-xor-form args)
  (cons 'XOR args))
*/

CELLP MakeXorForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeXorForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aXorAction;
	pf->psName=pf->paAction->psName;
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("MakeXorForm");
	return pc;
}

/* MakeImpliesForm

Description:
	Make an IMPLIES formula.
Scheme:

(define (make-implies-form arg1 arg2)
  (cons 'IMPLIES (list arg1 arg2)))
*/

CELLP MakeImpliesForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeImpliesForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aImpliesAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArg1);
		pf->pcArgs->pcNext=CopyCell(pcArg2);
	}
	else
	{
	  pf->pcArgs=pcArg1;
	  pcArg1->pcNext=pcArg2;
	  pcArg2->pcNext=NULL;
	}
	EXIT("MakeImpliesForm");
	return pc;
}

/* MakeIfThenElseForm

Description:
	Make an IFTHENELSE formula.
Scheme:

(define (make-if-then-else-form arg1 arg2 arg3)
  (cons 'IF-THEN-ELSE (list arg1 arg2 arg3)))
*/

CELLP MakeIfThenElseForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,						/* antecedent */
	CELLP pcArg2,						/* positive consequence */
	CELLP pcArg3						/* negative consequence */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeIfThenElseForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aIfThenElseAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArg1);
		pf->pcArgs->pcNext=CopyCell(pcArg2);
		pf->pcArgs->pcNext->pcNext=CopyCell(pcArg3);
	}
	else
	{
	  pf->pcArgs=pcArg1;
	  pcArg1->pcNext=pcArg2;
	  pcArg2->pcNext=pcArg3;
	  pcArg3->pcNext=NULL;
	}
	EXIT("MakeIfThenElseForm");
	return pc;
}

/* MakeInTheSetForm

Description:
	Make an InTheSet formula.
*/

CELLP MakeInTheSetForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVars,						/* variable list */
	CELLP pcVals						/* value list */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeInTheSetForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aInTheSetAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcVars=CopyCell(pcVars);
		pf->pcArgs=CopyCell(pcVals);
	}
	else
	{
		pf->pcVars=pcVars;
		pf->pcArgs=pcVals;
	}
	EXIT("MakeInTheSetForm");
	return pc;
}

/* MakeForAllForm

Description:
	Make an FORALL formula.
*/

CELLP MakeForAllForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit,						/* generator formula (such-that clause) */
	CELLP pcFormula
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeForAllForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aForAllAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcVars=CopyCellList(pcVariables);
		pf->pcGenLit=CopyCell(pcGenLit);
		pf->pcArgs=CopyCell(pcFormula);
	}
	else
	{
		pf->pcVars=pcVariables;
		pf->pcGenLit=pcGenLit;
		pcGenLit->pcNext=NULL;
		pf->pcArgs=pcFormula;
		if(pcFormula)
			pcFormula->pcNext=NULL;
	}
	EXIT("MakeForAllForm");
	return pc;
}

/* MakeExistsForm

Description:
	Make an Exists formula.
*/

CELLP MakeExistsForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit,						/* generator formula (such-that clause) */
	CELLP pcFormula
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeExistsForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aExistsAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcVars=CopyCellList(pcVariables);
		pf->pcGenLit=CopyCell(pcGenLit);
		pf->pcArgs=CopyCell(pcFormula);
	}
	else
	{
		pf->pcVars=pcVariables;
		pf->pcGenLit=pcGenLit;
		pcGenLit->pcNext=NULL;
		pf->pcArgs=pcFormula;
		if(pcFormula)
			pcFormula->pcNext=NULL;
	}
	EXIT("MakeExistsForm");
	return pc;
}

/* MakeExistsXForm

Description:
	Make an ExistsX formula.
*/

CELLP MakeExistsXForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit,						/* generator formula (such-that clause) */
	CELLP pcFormula
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeExistsXForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aExistsXAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcVars=CopyCellList(pcVariables);
		pf->pcGenLit=CopyCell(pcGenLit);
		pf->pcArgs=CopyCell(pcFormula);
	}
	else
	{
		pf->pcVars=pcVariables;
		pf->pcGenLit=pcGenLit;
		pcGenLit->pcNext=NULL;
		pf->pcArgs=pcFormula;
		if(pcFormula)
			pcFormula->pcNext=NULL;
	}
	EXIT("MakeExistsXForm");
	return pc;
}

/* MakeBindingForm

Description:
	Make a bindings formula.
	Note that both the pfVals and pfVars inputs may be lists.
Scheme:

(define (make-binding-form vars vals formula)
  (cons 'BINDING (list vars vals formula)))
*/

CELLP MakeBindingForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVars,
	CELLP pcVals,
	CELLP pcFormula
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeBindingForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aBindingAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcVars=CopyCellList(pcVars);
		pf->pcGenLit=CopyCellList(pcVals);	/* store values in genlit location */
		pf->pcArgs=CopyCell(pcFormula);
	}
	else
	{
		pf->pcVars=pcVars;
		pf->pcGenLit=pcVals;			/* store values in genlit location */
		pf->pcArgs=pcFormula;
		pcFormula->pcNext=NULL;
	}
#ifdef _DEBUG
	{
		int i,j;
		CELLP pc1;

		i=0;
		for(pc1=pcVars;pc1;pc1=pc1->pcNext)
			i++;
		j=0;
		for(pc1=pcVals;pc1;pc1=pc1->pcNext)
			j++;
		if(i!=j)
			ErrorMessage("MakeBindingForm:  Variable and value lists are different lengths %d %d\n",
				i,j);
	}
#endif // _DEBUG
	EXIT("MakeBindingForm");
	return pc;
}

/* MakeDeltaForm

Description:
	Make a delta (time shift) formula.
Scheme:

(define (make-delta-form formula)
  (cons 'DELTA formula))
*/

CELLP MakeDeltaForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeDeltaForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aDeltaAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArgs);
	}
	else
	{
		pf->pcArgs=pcArgs;
		pcArgs->pcNext=NULL;
	}
	EXIT("MakeDeltaForm");
	return pc;
}

/* MakeIntegerForm

Description:
	Make an integer formula.
*/

DECLSPEC CELLP MakeIntegerForm
(
	int nValue
)
{
	CELLP pc;
	FORMULAP pf;
#ifdef _DEBUG
	char acBuffer[12];
	int nPosition;
#endif // _DEBUG
	ENTER("MakeIntegerForm",FALSE);

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_INTEGER;
	pf->paAction=&aIntegerAction;
#ifdef _DEBUG
	sprintf(acBuffer,"%d",nValue);
	pf->psName=IdentAllocAndPos(acBuffer,&nPosition);
	pf->nHashPos=nPosition;
#endif // _DEBUG
	pf->uValue.nInteger=nValue;
	EXIT("MakeIntegerForm");
	return pc;
}

/* MakeFloatForm

Description:
	Make a floating point formula.
*/

DECLSPEC CELLP MakeFloatForm
(
	double dfValue
)
{
#ifdef _DEBUG
	char acBuffer[40];
	char *ps;
#endif // _DEBUG
	CELLP pc;
	FORMULAP pf;
	int nPosition;

	ENTER("MakeFloatForm",FALSE);

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_FLOAT;
	pf->paAction=&aFloatAction;
#ifdef _DEBUG
	sprintf(acBuffer,"%f",dfValue);
	for(ps=acBuffer+strlen(acBuffer);ps>acBuffer&&*ps=='0';--ps);
		ps[1]=0;						/* delete trailing zeros */
		pf->psName=IdentAllocAndPos(acBuffer,&nPosition);
		pf->nHashPos=nPosition;
#endif // _DEBUG
	pf->uValue.dfFloat=dfValue;
	EXIT("MakeFloatForm");
	return pc;
}

/* MakeStringForm

Description:
	Make a string formula.
*/

DECLSPEC CELLP MakeStringForm
(
	char *psValue						/* string value */
)
{
	CELLP pc;
	FORMULAP pf;
	int nPosition;

	ENTER("MakeStringForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_STRING;
	pf->paAction=&aStringAction;
	pf->psName=StrAllocAndPos(psValue,&nPosition);
	pf->nHashPos=nPosition;
	pf->uValue.psString=pf->psName;
	EXIT("MakeStringForm");
	return pc;
}

/* MakeLiteralForm

Description:
	Make a literal formula.
*/

DECLSPEC CELLP MakeLiteralForm
(
	int nType,							/* atomic type */
	LITVAL lValue						/* literal value */
)
{
	CELLP pc;
	FORMULAP pf;
	int nPosition;
#ifdef _DEBUG
//	char ac[40];
//	char *ps;
#endif // _DEBUG

	ENTER("MakeLiteralForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=nType;
	pf->paAction=&aLiteralAction;
	pf->uValue=lValue;
	switch(nType)
	{
		case ATOM_INTEGER:
#ifdef _DEBUG
//			sprintf(ac,"%d",lValue.nInteger);
//			pf->psName=IdentAlloc(ac);
#endif // _DEBUG
			break;
		case ATOM_FLOAT:
#ifdef _DEBUG
//			sprintf(ac,"%f",lValue.dfFloat);
//			for(ps=ac+strlen(ac);ps>ac&&*ps=='0';--ps);
//			ps[1]=0;					/* delete trailing zeros */
//			pf->psName=IdentAlloc(ac);
#endif // _DEBUG
			break;
		case ATOM_IDENT:
		  pf->psName=IdentAllocAndPos(lValue.psString,&nPosition);
		  pf->nHashPos=nPosition;
			break;
		case ATOM_STRING:
		  pf->psName=StrAllocAndPos(lValue.psString,&nPosition);
		  pf->nHashPos=nPosition;
			break;
		default:
			ErrorMessage("MakeLiteralForm:  Unsupported atomic type: %d\n",nType);
	}
	EXIT("MakeLiteralForm");
	return pc;
}

/* MakeAddForm

Description:
	Make an ADD formula
*/

DECLSPEC CELLP MakeAddForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeAddForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aAddAction;
	pf->psName=pf->paAction->psName;
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("MakeAddForm");
	return pc;
}

/* MakeSymbolInfoForm

Description:
	Make a formula for an object registered in the symbol info table.
Notes:
	Due to macro substitution, this routine may return a sequence of formulas!
*/

DECLSPEC CELLP MakeSymbolInfoForm
(
	BOOL bCopy,							/* copy arguments */
	LISTP plName,						/* name */
	CELLP pcArgs						/* list of arguments */
)
{
	CELLP pc,pc1;
	FORMULAP pf;
	int nLength;						/* number of arguments */
	SYMBOLINFOP psiSymbolInfo;
	int nPosition;

	ENTER("MakeSymbolInfoForm",TRUE);

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=plName->nType;
	pf->uValue=plName->uValue;
	pf->psName=IdentAllocAndPos(plName->psName,&nPosition);
	pf->nHashPos=nPosition;

	psiSymbolInfo=GetSymbolInfoPtr(pf->psName);
	if(!psiSymbolInfo)
		ErrorMessage("MakeSymbolInfoForm:  Unknown symbol %s\n",plName->psName);
	else
	{
		if(psiSymbolInfo->nArity>=0)
		{
			nLength=0;					/* check arity */
			for(pc1=pcArgs;pc1;pc1=pc1->pcNext)
				nLength++;
			if(psiSymbolInfo->nArity!=nLength)
			{
				ErrorMessage("%s called with %d argument%s, requires %d\n",
					pf->psName,nLength,nLength==1?"":"s",psiSymbolInfo->nArity);
				EXIT("MakeSymbolInfoForm");
				return NULL;
			}
		}	
		pf->paAction=psiSymbolInfo->paAction;
	}
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;

	/* handle macros */
	
	if(psiSymbolInfo->nSymbolType==SYMBOL_MACRO)
		pc=(*pc->pfForm->paAction->pfCompute)(pc,NULL,NULL);
	
	EXIT("MakeSymbolInfoForm");
	return pc;
}

/* MakeSearchGlobalInitializationForm

Description:
	Make an search-global-initialization formula.
	This is basically an internal version of an assignment.
*/

CELLP MakeSearchGlobalInitializationForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeSearchGlobalInitializationForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aSearchGlobalInitializationAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArg1);
		pf->pcArgs->pcNext=CopyCell(pcArg2);
	}
	else
	{
		pf->pcArgs=pcArg1;
		pcArg1->pcNext=pcArg2;
		pcArg2->pcNext=NULL;
	}
	EXIT("MakeSearchGlobalInitializationForm");
	return pc;
}

/* MakeOptimalCostForm

Description:
	Make an optimal-cost formula.
*/

CELLP MakeOptimalCostForm(void)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeOptimalCostForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aOptimalCostAction;
	pf->psName=pf->paAction->psName;
	EXIT("MakeOptimalCostForm");
	return pc;
}

/* MakeBestActionForm

Description:
	Make a best-action formula.
*/

CELLP MakeBestActionForm(void)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeBestActionForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aBestActionAction;
	pf->psName=pf->paAction->psName;
	EXIT("MakeBestActionForm");
	return pc;
}

/* routines called from user libraries -------------------------------------- */

/* MakeEqForm

Description:
	Make an = formula.
*/

DECLSPEC CELLP MakeEqForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeEqForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aEqAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArg1);
		pf->pcArgs->pcNext=CopyCell(pcArg2);
	}
	else
	{
	  pf->pcArgs=pcArg1;
	  pcArg1->pcNext=pcArg2;
	  pcArg2->pcNext=NULL;
	}
	EXIT("MakeEqForm");
	return pc;
}

/* MakeGlobalDelayedActionForm

Description:
	Make an GlobalDelayedAction formula.
*/

DECLSPEC CELLP MakeGlobalDelayedActionForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,						/* delay */
	CELLP pcArg2,						/* tag formula */
	CELLP pcArg3						/* action formula */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeGlobalDelayedActionForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aGlobalDelayedActionAction;
	pf->psName=pf->paAction->psName;
	if(bCopy)
	{
		pf->pcArgs=CopyCell(pcArg1);
		pf->pcArgs->pcNext=CopyCell(pcArg2);
		pf->pcArgs->pcNext->pcNext=CopyCell(pcArg3);
	}
	else
	{
	  pf->pcArgs=pcArg1;
	  pcArg1->pcNext=pcArg2;
	  pcArg2->pcNext=pcArg3;
	  pcArg3->pcNext=NULL;
	}
	EXIT("MakeGlobalDelayedActionForm");
	return pc;
}

/* MakeSetInitializationSequenceForm

Description:
	Make a set-initialization-sequence formula
*/

DECLSPEC CELLP MakeSetInitializationSequenceForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeSetInitializationSequenceForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aSetInitializationSequenceAction;
	pf->psName=pf->paAction->psName;
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("MakeSetInitializationSequenceForm");
	return pc;
}

/* MakeSetInitialWorldForm

Description:
	Make a set-initial-world formula
*/

DECLSPEC CELLP MakeSetInitialWorldForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeSetInitialWorldForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aSetInitialWorldAction;
	pf->psName=pf->paAction->psName;
	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
	EXIT("MakeSetInitialWorldForm");
	return pc;
}

/* MakePrintDeltaTimeForm

Description:
	Make a print-delta-time formula
*/

//DECLSPEC CELLP MakePrintDeltaTimeForm
//(
//	BOOL bCopy,							/* copy arguments */
//	CELLP pcArgs
//)
//{
//	CELLP pc;
//	FORMULAP pf;
//
//	ENTER("MakePrintDeltaTimeForm",FALSE);
//	pc=(CELLP)MemAlloc(sizeof(CELL));
//	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
//	pf->nType=ATOM_IDENT;
//	pf->paAction=&aPrintDeltaTimeAction;
//	pf->psName=pf->paAction->psName;
//	pf->pcArgs=bCopy?CopyCellList(pcArgs):pcArgs;
//	EXIT("MakePrintDeltaTimeForm");
//	return pc;
//}
