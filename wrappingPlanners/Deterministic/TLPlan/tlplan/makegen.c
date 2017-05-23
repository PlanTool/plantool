/* makegen.c -- Generator Routines

Copyright C, 1997, 2001, Fahiem Bacchus

*/

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */


#include "tlplan.h"
#include "btree.h"
#include "hbtree.h"
#include "compute.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "makeform.h"
#include "makegen.h"
#include "tl_tab.h"
#include "tlparse.h"
#include "util.h"
#include "var.h"
#include "world.h"
#include "zone.h"
#include "adl.h"

typedef struct DefinedContext
{
	BINDINGP pbBindings;			/* saved bindings list */
	BINDINGP pbTranslation;			/* bindings translation list */
}DEFINEDCONTEXT, *DEFINEDCONTEXTP;

typedef struct PermuteContext
{
	CELLP pcArgs;					/* evaluated terms */
	CELLP pcVals;					/* permuted list */
}PERMUTECONTEXT, *PERMUTECONTEXTP;

static BOOL FindNext
(
	PREDCONTEXTP ppc,
	BINDINGP pbBindings
);
static BOOL FindNext_H
(
	PREDCONTEXTP pgc,					/* generator context */
	BINDINGP pbBindings
);
static BOOL TestTuple
(
	CELLP pcTuple,
	CELLP pcArgs
);
static void SetBindings
(
	CELLP pcTuple,
	CELLP pcArgs,
	BINDINGP pbBindings
);

/* GenerateDescPredicate -------------------------------------------------------

Description:
	Generate a described predicate.
Scheme:

(define (make-desc-pred-generator gen-lit variables world/action bindings)
	(let* ((index (get-symbol-index (get-operator gen-lit)))
			(args (eval-terms (get-args gen-lit) world/action bindings))
			(tuple '())
			(generator (desc-predicate-generator index world/action)))
		(letrec ((find-next
					(lambda ()
						(set! tuple (generator))
						(if tuple
							(if (test-tuple)
								(begin (set-bindings) #t)
								(find-next))
							#f)))
				(test-tuple
					(lambda ()
						(every2 (lambda (val arg) (if (var? arg) #t (eqv? val arg)))
							tuple
							args)))
				(set-bindings
					(lambda ()
						(for-each
							(lambda (val arg)
								(if (var? arg) (set-var! arg val bindings)))
							tuple args))))
			(if generator
				(lambda ()
					(find-next))
				#f))))


BOOL (*GenerateDescPredicate)(CELLP,void,LINEARPLANP

*/ 
/*
 FAHIEM DEC 2000. Modified to detect if pcArgs contains a bound variable
 in its first argument place. In which case we call a special version of
 BTreeGenerator. Note that this is certainly not the most efficient way of
 doing this, as we test pcArgs every time we call generatedescpredicate.
*/


BOOL GenerateDescPredicate
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
) 
{
  
  if (bVerifyingPreservedProperty || bMutexMode)
    return GenerateDescPredicate_H(pcGenLit,ppvContext,pcVars,plpLinearPlan,pbBindings);
  if (bUseHWorld) 
    return GenerateDescPredicate_H(pcGenLit,ppvContext,pcVars,plpLinearPlan,pbBindings);
  else if (bUseHWorldFF)
    return GenerateDescPredicate_H(pcGenLit,ppvContext,pcVars,plpLinearPlan,pbBindings);
  else
    return GenerateDescPredicate_STD(pcGenLit,ppvContext,pcVars,plpLinearPlan,pbBindings);
}


BOOL GenerateDescPredicate_STD
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("GenerateDescPredicate",FALSE);
	if(!*ppvContext)
	{
		PREDCONTEXTP pgc;
		BTREEP pbtArgTree;
		int nIndex;
		
		pgc=(PREDCONTEXTP)MemAlloc(sizeof(PREDCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		
		nIndex=pcGenLit->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
		if(pcGenLit->pfForm->pcArgs)
		{
			pgc->pcArgs=ComputeTerms(pcGenLit->pfForm->pcArgs,
				plpLinearPlan,pbBindings);
			if(!pgc->pcArgs)
				TermError("generate-desc-predicate",pcGenLit,pbBindings);
		}
		pbtArgTree=GetSymbolArgs(LinearPlanWorld(plpLinearPlan),nIndex);
		if(VarQ(pgc->pcArgs))
			BTreeGenerator(pbtArgTree,&pgc->pvContext);	/* initialize generator */
		//FB
		else
			BTreeGeneratorWithBoundVars(pbtArgTree,&pgc->pvContext,pgc->pcArgs);
	}
	if(!FindNext((PREDCONTEXTP)*ppvContext,pbBindings))
	{
		EXIT("GenerateDescPredicate");
		return FALSE;
	}
	
	EXIT("GenerateDescPredicate");
	return TRUE;
}



BOOL GenerateDescPredicate_H
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("GenerateDescPredicate",FALSE);
	if(!*ppvContext)
	{
		PREDCONTEXTP pgc;
		HBTREEP pbtArgTree;
		int nIndex;
		
		pgc=(PREDCONTEXTP)MemAlloc(sizeof(PREDCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		
		nIndex=pcGenLit->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
		if(pcGenLit->pfForm->pcArgs)
		{
			pgc->pcArgs=ComputeTerms(pcGenLit->pfForm->pcArgs,
				plpLinearPlan,pbBindings);
			if(!pgc->pcArgs)
				TermError("generate-desc-predicate",pcGenLit,pbBindings);
		}
		pbtArgTree=GetSymbolArgs(LinearPlanHWorld(plpLinearPlan),nIndex);
		if(VarQ(pgc->pcArgs))
			HBTreeGenerator(pbtArgTree,&pgc->pvContext);	/* initialize generator */
		//FB
		else
			HBTreeGeneratorWithBoundVars(pbtArgTree,&pgc->pvContext,pgc->pcArgs);
	}
	if(!FindNext_H((PREDCONTEXTP)*ppvContext,pbBindings))
	{
		EXIT("GenerateDescPredicate_H");
		return FALSE;
	}
	
	EXIT("GenerateDescPredicate_H");
	return TRUE;
}


/* FindNext_H: Same as FindNext, but for h-worlds.*/

static BOOL FindNext_H
(
	PREDCONTEXTP pgc,					/* generator context */
	BINDINGP pbBindings
)
{
	CELLP pcTuple;
	
	ENTER("FindNext_H",TRUE);

	if(VarQ(pgc->pcArgs))
	{
		for(pcTuple=HBTreeGenerator(NULL,&pgc->pvContext);pcTuple;
			pcTuple=HBTreeGenerator(NULL,&pgc->pvContext))
		{
			if(TestTuple(pcTuple,pgc->pcArgs))
			{
				SetBindings(pcTuple,pgc->pcArgs,pbBindings);
				EXIT("FindNext");
				return TRUE;
			}
		}
	}
	else
	{
		//FB
		for(pcTuple=HBTreeGeneratorWithBoundVars(NULL,&pgc->pvContext,pgc->pcArgs);pcTuple;
			pcTuple=HBTreeGeneratorWithBoundVars(NULL,&pgc->pvContext,pgc->pcArgs))
		{
			if(TestTuple(pcTuple,pgc->pcArgs))
			{
				SetBindings(pcTuple,pgc->pcArgs,pbBindings);
				EXIT("FindNext");
				return TRUE;
			}
		}
	}
	EXIT("FindNext_H");
	return FALSE; 						/* no more tuples */
}



/* FindNext

Description:
	Find the next tuple that satisfies the quantifier's arguments, and
	bind its value to pbBindings.
*/

/*Fahiem Dec 2000. Modified to detect if pcArgs contains a bound variable
  in its first argument place. In which case we call a special version of
  BTreeGenerator.
*/

static BOOL FindNext
(
	PREDCONTEXTP pgc,					/* generator context */
	BINDINGP pbBindings
)
{
	CELLP pcTuple;
	
	ENTER("FindNext",TRUE);

	if(VarQ(pgc->pcArgs))
	{
		for(pcTuple=BTreeGenerator(NULL,&pgc->pvContext);pcTuple;
			pcTuple=BTreeGenerator(NULL,&pgc->pvContext))
		{
			if(TestTuple(pcTuple,pgc->pcArgs))
			{
				SetBindings(pcTuple,pgc->pcArgs,pbBindings);
				EXIT("FindNext");
				return TRUE;
			}
		}
	}
	else
	{
		//FB
		for(pcTuple=BTreeGeneratorWithBoundVars(NULL,&pgc->pvContext,pgc->pcArgs);pcTuple;
			pcTuple=BTreeGeneratorWithBoundVars(NULL,&pgc->pvContext,pgc->pcArgs))
		{
			if(TestTuple(pcTuple,pgc->pcArgs))
			{
				SetBindings(pcTuple,pgc->pcArgs,pbBindings);
				EXIT("FindNext");
				return TRUE;
			}
		}
	}
	EXIT("FindNext");
	return FALSE; 						/* no more tuples */
}

/* TestTuple

Description:
	Compare a predicate tuple with a quantifier's arguments.
	They match if either the argument is a variable, or the values are equal.
*/

BOOL TestTuple
(
	CELLP pcTuple,
	CELLP pcArgs
)
{
	CELLP pc1,pc2;
	
	ENTER("TestTuple",FALSE);
	for(pc1=pcTuple,pc2=pcArgs;pc1&&pc2;pc1=pc1->pcNext,pc2=pc2->pcNext)
	{
		if(!VarQ(pc2)&&!FormulaEqQ(pc1,pc2))
		{
			EXIT("TestTuple");
			return FALSE;
		}
	}
	EXIT("TestTuple");
	return TRUE;
}

/* GenerateDefGenerator

Description:
	Generate a defined generator.
	On our first run, we create a bindings list for parameters and local variables.
	We save this as our context so it can be used multiple times.  
	Each time we are called, we bind our initialize flag to the generator name,
	then evaluate the defined generator's formula.  
	We terminate when the generator returns false.
*/

BOOL GenerateDefGenerator
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	BINDINGP pb;
	CELLP pc,pc1,pc2,pcGenerator;
	CELLP pcTerms;
	FORMULAP pf;
	SYMBOLINFOP psiSymbolInfo;
	DEFINEDCONTEXTP pgc;				/* generator context */
	char acBuffer[128];
	int nPosition;

	ENTER("GenerateDefGenerator",TRUE);
	
	if(!*ppvContext)					/* if this is our first time */
	{
		pgc=(DEFINEDCONTEXTP)MemAlloc(sizeof(DEFINEDCONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;
		
		psiSymbolInfo=pcGenLit->pfForm->uValue.psiSymbolInfo;
		
		/* declare generator name in bindings list for returned value */
		
		pcGenerator=(CELLP)MemAlloc(sizeof(CELL));
		pcGenerator->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
		pf->nType=ATOM_IDENT;
		sprintf(acBuffer,"?%s",psiSymbolInfo->psName);
		pf->psName=IdentAllocAndPos(acBuffer,&nPosition);
		pf->nHashPos=nPosition;
		pc=MakeIntegerForm(0);
		pb=ExtendBindings(pcGenerator,pc,pbBindings);
		
		/* bind arguments to formal parameters */
		
		if(pcGenLit->pfForm->pcArgs)
		{
			pcTerms=ComputeTerms(pcGenLit->pfForm->pcArgs,plpLinearPlan,pbBindings);
			if(!pcTerms)
				TermError("generate-def-generator",pcGenLit,pbBindings);	
			pb=ExtendBindings(GetDefParameters(psiSymbolInfo),pcTerms,pb);
		}
		
		/* add local variables to the bindings list */
		
		pc=GetDefLocals(psiSymbolInfo);
		if(pc)
		{
			pc=ExpandArrays(pc,plpLinearPlan,pb);
			pb=ExtendBindings(pc,pc,pb);
		}
		
		pgc->pbBindings=pb;				/* save our bindings list */
		
		/* create the translation list */
		
		pgc->pbTranslation=NULL;
		for(pc=pcVars;pc;pc=pc->pcNext)
		{
			for(pc1=GetDefParameters(psiSymbolInfo),pc2=pcGenLit->pfForm->pcArgs;pc2;pc1=pc1->pcNext,pc2=pc2->pcNext)
			{
				if(FormulaEqQ(pc,pc2))
				{
					pgc->pbTranslation=ExtendBindings(CopyCell(pc1),CopyCell(pc2),pgc->pbTranslation);
					break;
				}
			}		
		}
	}
	else
	{
		pgc=(DEFINEDCONTEXTP)*ppvContext;
		psiSymbolInfo=pcGenLit->pfForm->uValue.psiSymbolInfo;
		pb=pgc->pbBindings;
		pcGenerator=(CELLP)MemAlloc(sizeof(CELL));
		pcGenerator->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
		pf->nType=ATOM_IDENT;
		sprintf(acBuffer,"?%s",psiSymbolInfo->psName);
		pf->psName=IdentAllocAndPos(acBuffer,&nPosition);
		pf->nHashPos=nPosition;
		pc=MakeIntegerForm(1);
		SetVarX(pcGenerator,pc,pb);
	}
	
	/* evaluate the formula (for side effects) */
	
	pc=GetDefFormula(psiSymbolInfo);
	b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pb);
	if(!b)
	{
		EXIT("GenerateDefGenerator");
		return FALSE;
	}
	
	/* return the desired values to the quantifier */
	
	for(pb=pgc->pbTranslation;pb;pb=pb->pbNext)
	{
		pc=LookupVar(pb->pcVar,pgc->pbBindings);
		SetVarX(pb->pcVal,pc,pbBindings);
	}
	
	EXIT("GenerateDefGenerator");
	return TRUE;
}

/* SetBindings

Description:
	For each variable named in pfArgs, use the corresponding value in pfTuple
	to set the variable's binding in pbBindings.
Note:
	The passed values in pfTuple become the property of SetBindings.
*/

static void SetBindings
(
 CELLP pcTuple,
 CELLP pcArgs,
 BINDINGP pbBindings
 )
{
  CELLP pc1,pc2;

  ENTER("SetBindings",TRUE);
  for(pc1=pcTuple,pc2=pcArgs;pc1&&pc2;pc1=pc1->pcNext,pc2=pc2->pcNext)
    {
      if(VarQ(pc2))
	SetVarX(pc2,pc1,pbBindings);
    }
  EXIT("SetBindings");
}

/* GenerateEq ------------------------------------------------------------------

Description:
	Equate generator.
Notes:
	Our generator must be a computable expression.
	We assume that if the generator isn't a function, it must be a constant.
	If we encounter bad parameters, we don't generate anything.
*/

BOOL GenerateEq
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVal;
	CELLP pcVar;
	CELLP pcGen;

	ENTER("GenerateEq",FALSE);
	if(!*ppvContext)
	{
		/* determine which argument is the generator */

		if(VarQ(pcGenLit->pfForm->pcArgs))
		{
			pcVar=pcGenLit->pfForm->pcArgs;
			pcGen=pcVar->pcNext;
		}
		else
		{
			pcGen=pcGenLit->pfForm->pcArgs;
			pcVar=pcGen->pcNext;
		}

		/* check for bad parameters */
	
		if(!StringEqQ(IdentName(pcVar->pfForm),IdentName(pcVars->pfForm))||pcVars->pcNext)
		{
			ErrorMessage("generate-eq:	Free variable mismatch.\n");
			PrintFormulaList(stderr,pcGenLit);
			PrintFormulaList(pfTraceStream,pcGenLit);
			longjmp(*pjbCurrentJump,1);		/* tear down stack */
		}

		if(!pcGen->pfForm->paAction->pfCompute)
		{
			ErrorMessage("generate-eq:	Value cannot be computed.\n");
			PrintFormulaList(stderr,pcGenLit);
			PrintFormulaList(pfTraceStream,pcGenLit);
			longjmp(*pjbCurrentJump,1);		/* tear down stack */
		}

		*(int *)ppvContext=TRUE;		/* use context pointer as a flag */
		pcVal=ComputeTerm(pcGen,plpLinearPlan,pbBindings);
		if(!pcVal)						/* don't generate a bad expression */
		{
			if(pcGen->pfForm->paAction->pfCompute!=ComputeDescFunction)
				TermError("generate-eq:	Invalid expression.\n",pcVal,pbBindings);
			EXIT("GenerateEq");
			return FALSE;
		}
		SetVarX(pcVars,pcVal,pbBindings);
		EXIT("GenerateEq");
		return TRUE;
	}
	EXIT("GenerateEq");
	return FALSE;
}

/* GenerateFunction --------------------------------------------------------

Description:
	A generator that returns a function value (at most once).
Scheme:

(define (make-fn-value-generator gen-lit variables world/action bindings)
	(let* ((value (eval-term (get-fn-lit-gen gen-lit) world/action bindings))
			(first-time #t))
		(lambda ()
			(if first-time
				(begin (set-var! (first variables) value bindings)
					(set! first-time #f)
					#t)
				#f))))
*/

//BOOL GenerateFunction
//(
//	CELLP pcGenLit,
//	void **ppvContext,
//	CELLP pcVars,
//	LINEARPLANP plpLinearPlan,
//	BINDINGP pbBindings
//)
//{
//	CELLP pcVal;
//
//	ENTER("GenerateFunction",FALSE);
//	if(!*ppvContext)
//	{
//		*(int *)ppvContext=TRUE;		/* use context pointer as a flag */
//		pcVal=ComputeTerm(pcGenLit,plpLinearPlan,pbBindings);
//		if(!pcVal)						/* don't generate a nonexistant value */
//			return FALSE;
//		SetVarX(pcVars,pcVal,pbBindings);
//		EXIT("GenerateFunction");
//		return TRUE;
//	}
//	EXIT("GenerateFunction");
//	return FALSE;
//}

/* Modalities ---------------------------------------------------------------- */

/* GenerateGoal

Description:
	Goal modality for generators.
*/

BOOL GenerateGoal
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	
	ENTER("GenerateGoal",FALSE);
	b=(*pcGenLit->pfForm->pcArgs->pfForm->paAction->pfGenerator)
		(pcGenLit->pfForm->pcArgs,ppvContext,pcVars,GetGoalWorld(TRUE),pbBindings);
	EXIT("GenerateGoal");
	return b;
}

/* GenerateCurrent

Description:
	Current modality for generators.
*/

BOOL GenerateCurrent
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("GenerateCurrent",FALSE);
	b=(*pcGenLit->pfForm->pcArgs->pfForm->paAction->pfGenerator)
		(pcGenLit->pfForm->pcArgs,ppvContext,pcVars,plpCurrentPlan,pbBindings);
	EXIT("GenerateCurrent");
	return b;
}

/* GeneratePrevious

Description:
	Previous modality for generators.
*/

BOOL GeneratePrevious
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp;
	BOOL b;

	ENTER("GeneratePrevious",FALSE);

	/* if the current plan is not null, get the previous plan */
	
	plp=plpLinearPlan;
	if(plp)
		plp=plp->plpParent;

	/* if the previous plan exists, generate the passed formula, 
		otherwise return FALSE */
	
	if(plp)
		b=(*pcGenLit->pfForm->pcArgs->pfForm->paAction->pfGenerator)
			(pcGenLit->pfForm->pcArgs,ppvContext,pcVars,plp,pbBindings);
	else
		b=FALSE;
	EXIT("GeneratePrevious");
	return b;
}

/* GeneratePermute

Description:
	Permutation modality for generators.
Notes:
	For now, we only support described predicate generators.
*/

BOOL GeneratePermute
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	PERMUTECONTEXTP pgc;				/* generator context */
	CELLP pc; 							/* formula pointer */
	
	ENTER("PermuteGenerator",FALSE);
	
	if(!*ppvContext)
	{
		SYMBOLINFOP psiSymbolInfo;		/* symbol information pointer */
		CELLP pcGen;					/* pointer to generator */
		CELLP *ppc1;					/* array of formula pointers */
		void *pvContext;				/* context for BTreeGenerator */
		CELLP pcStart,pcEnd;			/* enumerated predicate list */
		CELLP pcTuple;					/* returned btree keys (we don't own these!) */
		BTREEP pbtArgTree;				/* btree pointer */
		int nCount;						/* number of predicates */
		int nIndex;						/* symbol info index */
		int i;							/* loop index */
		
		pcGen=pcGenLit->pfForm->pcArgs;
		if(!pcGen)
		{
			ErrorMessage("permute-generator:  Generator argument missing\n");
			EXIT("PermuteGenerator");
			return FALSE;
		}
		if(pcGen->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("permute-generator:  Unsupported generator type\n");
			EXIT("PermuteGenerator");
			return FALSE;
		}
		psiSymbolInfo=pcGen->pfForm->uValue.psiSymbolInfo;
		if(!PredicateQ(psiSymbolInfo)&&!DescribedQ(psiSymbolInfo))
		{
			ErrorMessage("permute-generator:  Unsupported generator type\n");
			EXIT("PermuteGenerator");
			return FALSE;
		}
		
		pgc=(PERMUTECONTEXTP)MemAlloc(sizeof(PERMUTECONTEXT));
		*ppvContext=(void *)pgc;
		pbBindings->pgcContext=(GCONTEXTP)pgc;	
		nIndex=psiSymbolInfo-asiWorldSymbols;
		if(pcGen->pfForm->pcArgs)
		{
			pgc->pcArgs=ComputeTerms(pcGen->pfForm->pcArgs,plpLinearPlan,pbBindings);
			if(!pgc->pcArgs)
				TermError("permute-generator",pcGen,pbBindings);
		}
		pbtArgTree=GetSymbolArgs(LinearPlanWorld(plpLinearPlan),nIndex);
		pvContext=0;
		BTreeGenerator(pbtArgTree,&pvContext);	/* initialize generator */
		
		/* enumerate the described predicate to create a list */
		
		pcStart=NULL;
		pcEnd=(CELLP)&pcStart;
		nCount=0;
		for(pcTuple=BTreeGenerator(NULL,&pvContext);pcTuple;
		pcTuple=BTreeGenerator(NULL,&pvContext))
		{
			if(TestTuple(pcTuple,pgc->pcArgs))
			{
				pcEnd=pcEnd->pcNext=(CELLP)MemAlloc(sizeof(CELL));
				pcEnd->pfForm=(FORMULAP)MemAlloc(sizeof(FORMULA));	// dummy formula
				pcEnd->pfForm->pcArgs=pcTuple;
				nCount++;
			}
		}
		
		/* permute the list */
		
		ppc1=(CELLP *)MemAlloc(sizeof(CELLP)*nCount);
		pc=pcStart;
		for(i=0;i<nCount;i++)
		{
			ppc1[i]=pc;
			pc=pc->pcNext;
		}
		
		pgc->pcVals=NULL;
		pcEnd=(CELLP)&pgc->pcVals;
		while(nCount)
		{
			i=rand()%nCount;
			pcEnd=pcEnd->pcNext=ppc1[i];
			ppc1[i]=ppc1[--nCount];
		}
		pcEnd->pcNext=NULL;
	}
	else
		pgc=(PERMUTECONTEXTP)*ppvContext;

	if(!pgc->pcVals)
	{
		EXIT("PermuteGenerator");
		return FALSE;
	}
	pc=pgc->pcVals;
	pgc->pcVals=pc->pcNext;
	SetBindings(pc->pfForm->pcArgs,pgc->pcArgs,pbBindings);
	EXIT("PermuteGenerator");
	return TRUE;
}
