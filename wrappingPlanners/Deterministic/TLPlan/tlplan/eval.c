/* eval.c

Copyright C, 1996 - 2001, F. Bacchus

Eval

 Routines to evaluate first order formulas on worlds.
--------------------------------------------------------------------------------

 Formula Syntax.
 Predicates are specified via literal lists, e.g.,
 (Pred x1 x2)
 Functions are specified via equality literals where the function
 always comes first and the function value comes second, e.g.,
 (= (fn x1) y1)
 Quantified Formulas have the syntax, e.g.,
 (forall (?x ?y) (on ?x ?y) formula)
 Binding formulas are internal: used only by the temporal goal progressor.
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
#include "color.h"
#include "compute.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "idle.h"
#include "iface.h"
#include "interval.h"
#include "makeform.h"
#include "makegen.h"
#include "plan.h"
#include "progress.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "var.h"
#include "world.h"
#include "adl.h"
#include "hbtree.h"


/* local structures and definitions */

#define MINFILE 3
#define MAXFILE 32

/* local function prototypes */

/* global data */

DECLSPEC char *psPlanName="";			/* current problem name */
DECLSPEC BOOL bImmediateExit;			/* immediate exit flag */

FILETABLE aftFileTable[MAXFILE];		/* file handle table */

int nSearchDepthLimit;					/* search depth limit */
double dfSearchHeuristicLimit;			/* search heuristic limit */

/* local data */

/* Evaluators ------------------------------------------------------------------ */

/* EvalDefPredicate

Description:
	Evaluate a defined predicate...
Scheme:

(define (eval-def-predicate index args world/action)
  (let* ((df (get-symbol-eval-object index))
		 (bindings (extend-bindings (get-def-parameters df) args '())))
	(eval-formula (get-def-formula df) world/action bindings)))
*/

BOOL EvalDefPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;
	CELLP pcTerms;
	SYMBOLINFOP psiSymbolInfo;
	BINDINGP pb;

	ENTER("EvalDefPredicate",TRUE);
	psiSymbolInfo=pcFormula->pfForm->uValue.psiSymbolInfo;
	if(!psiSymbolInfo->pcFormula)
	{
		ErrorMessage("Eval-def-predicate:  Undefined predicate \"%s\".\n",psiSymbolInfo->psName);
		return FALSE;
	}

	/* bind arguments to formal parameters */
	
	pb=pbBindings;
	if(pcFormula->pfForm->pcArgs)
	{
		pcTerms=ComputeTerms(pcFormula->pfForm->pcArgs,plpLinearPlan,pb);
		if(!pcTerms)
			TermError("eval-def-predicate",pcFormula,pb);
		pb=ExtendBindings(GetDefParameters(psiSymbolInfo),pcTerms,pb);
	}

	/* add local variables to the bindings list */
	
	pc=GetDefLocals(psiSymbolInfo);
	if(pc)
	{
		pc=ExpandArrays(pc,plpLinearPlan,pb);
		pb=ExtendBindings(pc,pc,pb);
	}

	/* evaluate the formula */

	pc=GetDefFormula(psiSymbolInfo);
	b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pb);

	// debug (color a defined formula in the log file, each time it's evaluated)
	
//	if(!stricmp(psiSymbolInfo->psName,"ok-to-turn"))
//	{
//		CommandPrintf(pfTraceStream,"%s ",psiSymbolInfo->psName);
//		PrintFormulaList(pfTraceStream,pcTerms);
//		EColorFormula(pfTraceStream,pc,plpLinearPlan,pb,0);
//		CommandPrintf(pfTraceStream,"\n");
//	}

	EXIT("EvalDefPredicate");
	return b;
}

/* EvalTrue

Description:
Scheme:

(define (eval-true args world/action bindings)
  #t)
*/

BOOL EvalTrue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalTrue",TRUE);
	EXIT("EvalTrue");
	return TRUE;
}

/* EvalFalse

Description:
Scheme:

(define (eval-false args world/action bindings)
  #f)
*/

BOOL EvalFalse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalFalse",TRUE);
	EXIT("EvalFalse");
	return FALSE;
}

/* EvalAnd

Description:
	Evaluate an AND
Scheme:

(define (eval-and args world/action bindings)
  (every (lambda (formula) (eval-formula formula world/action bindings))
		 args))
*/

BOOL EvalAnd
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalAnd",TRUE);

	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if(!(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
		{
			EXIT("EvalAnd");
			return FALSE;
		}
	}
	EXIT("EvalAnd");
	return TRUE;
}

/* EvalAndRelaxed_H almost exactly as EvalAnd. The only difference is that it resets
   the value of dEvalTotalCost. This is done so that costs of conditional effects of are not 
   passed to other (potentially conditional) effects. */ 

BOOL EvalAndRelaxed_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	double hsp_cost=0;
	BOOL bResetCosts=FALSE;

	ENTER("EvalAnd",TRUE);
	


	if (bUnderRelaxedModifyWorld) {
	  hsp_cost = dEvalTotalCost;
	  bResetCosts=TRUE;
	}
	
	bUnderRelaxedModifyWorld=FALSE; /* we don't want nested 
					   ands to reset the cost */
					   
					
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
	  /* reset the cost each time a new conjunct is evaluated when
	     under a relaxed modify-world */

	  if (bResetCosts) dEvalTotalCost=hsp_cost;
	  
	  if(!(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings)) {
	  
	    if (bResetCosts) bUnderRelaxedModifyWorld=TRUE; /* restore the flag, just in case */
	    EXIT("EvalAnd");
	    return FALSE;
	  }
		
	}

	if (bResetCosts) bUnderRelaxedModifyWorld=TRUE; /* restore the flag, just in case */
	EXIT("EvalAnd");
	return TRUE;
}


BOOL EvalAndRelaxed_FF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	double hsp_cost=0;
	BOOL bResetCosts=FALSE;
	LISTP pcSupFacts;
	
	ENTER("EvalAndRelaxed_FF",TRUE);
	

	pcSupFacts=NULL; // keep compiler happy

	if (bUnderRelaxedModifyWorld) {
	  hsp_cost = dEvalTotalCost;
	  pcSupFacts = pcFFSupFacts;
	  bResetCosts=TRUE;
	}
	
	bUnderRelaxedModifyWorld=FALSE; /* we don't want nested 
					   ands to reset the cost */
					   
		
	//	printf("pcFFSupFacts=%p\n",pcFFSupFacts);
	
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
	  /* reset the cost each time a new conjunct is evaluated when
	     under a relaxed modify-world */

      	  if(!(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings)) {
	    
	    if (bResetCosts) {
	      dEvalTotalCost=hsp_cost;
	      pcFFSupFacts=pcSupFacts;
	    }
	    
	    if (bResetCosts) bUnderRelaxedModifyWorld=TRUE; /* restore the flag, just in case */
	    EXIT("EvalAnd");
	    return FALSE;
	  }

	  if (bResetCosts) {
	    dEvalTotalCost=hsp_cost;
	    pcFFSupFacts=pcSupFacts;
	    // printf("Resetting pcFFSupFacts back to %p\n",pcFFSupFacts);
	  }

		
	}

	if (bResetCosts) bUnderRelaxedModifyWorld=TRUE; /* restore the flag, just in case */
	EXIT("EvalAndRelaxed_FF");
	return TRUE;
}



/* EvalOr

Description:
	Evaluate an OR.
Scheme:

(define (eval-or args world/action bindings)
  (some (lambda (formula) (eval-formula formula world/action bindings))
		args))
*/

BOOL EvalOr
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("EvalOr",TRUE);
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if((*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
		{
			EXIT("EvalOr");
			return TRUE;
		}
	}
	EXIT("EvalOr");
	return FALSE;
}

/* EvalXor

Description:
	Evaluate an exclusive or.
Scheme:

(define (eval-xor args world/action bindings)
  (letrec
	  ((loop (lambda (forms found-one)
		   (cond ((null? forms) found-one)
			 ((eval-formula (first forms) world/action bindings)
			  (if found-one #f (loop (rest forms) #t)))
			 (else (loop (rest forms) found-one))))))
	(loop args #f)))
*/

BOOL EvalXor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int count;

	ENTER("EvalXor",TRUE);
	count=0;
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if((*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
		{
			if(count)
			{
				EXIT("EvalXor");
				return FALSE;
			}
			count++;
		}
	}
	EXIT("EvalXor");
	return count==1;
}

/* EvalImplies

Description:
	Evaluate an IMPLIES
Scheme:

(define (eval-implies args world/action bindings)
  (if (eval-formula (first args) world/action bindings)
	  (eval-formula (second args) world/action bindings)
	#t))
*/

BOOL EvalImplies
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalImplies",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if((*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
	{
		pc=pc->pcNext;
		b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	}
	else
		b=TRUE;
	EXIT("EvalImplies");
	return b;
}

/* EvalIfThenElse

Description:
	Evaluate an IFTHENELSE. (IFTHENELSE A B C) is equivalent to,
	(and (IMPLIES A B) (IMPLIES (not A) C).
Scheme:

(define (eval-if-then-else args world/action bindings)
   (and (IMPLIES A B) (IMPLIES (not A) C). But is nice syntactic sugar."
  (if (eval-formula (first args) world/action bindings)
	  (eval-formula (second args) world/action bindings)
	(eval-formula (third args) world/action bindings)))
*/

BOOL EvalIfThenElse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalIfThenElse",TRUE);
	pc=pcFormula->pfForm->pcArgs;

		  
	if((*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
	{
		pc=pc->pcNext;
		b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	}
	else
	{
		pc=pc->pcNext->pcNext;
		b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	}
	EXIT("EvalIfThenElse");
	return b;
}

BOOL EvalIfThenElse_RESET
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalIfThenElse",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	
	dEvalTotalCost=0; /* reseting the total cost before evaluating anything */
	  
	if((*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
	{
		pc=pc->pcNext;
		b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	}
	else
	{
		pc=pc->pcNext->pcNext;
		b=(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	}
	EXIT("EvalIfThenElse");
	return b;
}


/* EvalNot

Description:
	Evaluate a NOT.
Scheme:

(define (eval-not args world/action bindings)
  (not (eval-formula (first args) world/action bindings)))
*/

BOOL EvalNot
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalNot",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	b=!(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);
	EXIT("EvalNot");
	return b;
}


BOOL EvalNotRelaxed
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings
)
{
  ENTER("EvalNotRelaxed", TRUE);
  EXIT("EvalNotRelaxed");
  return TRUE;
}

/* EvalNotNeg 

Behaves as EvalNot, but flips a global flag. 
This flag changes the behaviour of EvalDescPredicate when 
in relaxed mode with negations */

BOOL EvalNotRelaxedNeg 
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalNotRelaxedNeg",TRUE);
	bUnderNot = 1-bUnderNot; /* flip the global flag */

	pc=pcFormula->pfForm->pcArgs;
	b=!(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings);

	bUnderNot = 1-bUnderNot; /* flip it back again */
	EXIT("EvalNotRelaxedNeg");
	return b;
}

/* EvalEq

Description:
	Evaluate an = relation.
Scheme:

(define (eval-= args world/action bindings)
  (eqv? (eval-term (first args) world/action bindings)
	(eval-term (second args) world/action bindings)))
*/

BOOL EvalEq
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc1,pc2;

	ENTER("EvalEq",TRUE);
	pc1=ComputeTerm(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pc1)
		TermError("eval-eq",pcFormula,pbBindings);
	pc2=ComputeTerm(pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings);
	if(!pc2)
		TermError("eval-eq",pcFormula,pbBindings);
	b=FormulaEqQ(pc1,pc2);
	EXIT("EvalEq");
	return b;
}

/* EvalForAll

Description:
	Evaluate a FORALL formula.
Scheme:

(define (eval-forall args world/action bindings)
	(let* ((variables (get-qf-variables args))
			(gen-lit (get-qf-generator args))
			(formula (get-qf-formula args))
			(generator '()))
		(set! bindings (extend-bindings variables variables bindings))
		(set! generator
			(make-generator gen-lit variables world/action bindings))
		(if generator
			(do ((finish? #f)
					(result '()))
				(finish? result)
				(cond
					((generator)
						(if (not (eval-formula formula world/action bindings))
							(begin (set! finish? #t)
								(set! result #f))))
					(else (set! finish? #t)
						(set! result #t))))
			#t)))
*/

BOOL EvalForAll
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;
	CELLP pcVars;
	CELLP pcGenLit;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index

	ENTER("EvalForAll",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	if(!pcArgs)
	{
		EXIT("EvalForAll");
		return FALSE;
	}
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{
		if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings))
		{
			SetBTreeSP(nBaseSP);		// restore btree stack pointer
			EXIT("EvalForAll");
			return FALSE;
		}
	}
	EXIT("EvalForAll");
	return TRUE;
}


BOOL EvalForAll_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;
	CELLP pcVars;
	CELLP pcGenLit;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index
	double hsp_cost;
	
	ENTER("EvalForAll_H",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	nBaseSP=HGetBTreeSP();


	hsp_cost=dEvalTotalCost;

	if(!pcArgs)
	{
		EXIT("EvalForAll_H");
		return FALSE;
	}
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{

	  dEvalTotalCost = hsp_cost;  /* reset the cost prior to evaluating the formula */

		if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings))
		{
			HSetBTreeSP(nBaseSP);		// restore btree stack pointer
			EXIT("EvalForAll_H");
			return FALSE;
		}
	}
	EXIT("EvalForAll_H");
	return TRUE;
}



BOOL EvalForAll_FF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;
	CELLP pcVars;
	CELLP pcGenLit;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index
	double hsp_cost;
	LISTP pcSupFacts;
	
	ENTER("EvalForAll_H",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	nBaseSP=HGetBTreeSP();


	hsp_cost=dEvalTotalCost;
	pcSupFacts=pcFFSupFacts;

	if(!pcArgs)
	{
		EXIT("EvalForAll_FF");
		return FALSE;
	}
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{

	  dEvalTotalCost = hsp_cost;  /* reset the cost prior to evaluating the formula */
	  pcFFSupFacts = pcSupFacts;  /* reset the list of facts */

		if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings))
		{
		  dEvalTotalCost = hsp_cost;  /* reset the cost prior to evaluating the formula */
		  pcFFSupFacts = pcSupFacts;  /* reset the list of facts */
		  
		  HSetBTreeSP(nBaseSP);		// restore btree stack pointer
		  EXIT("EvalForAll_FF");
		  return FALSE;
		}
	}
	EXIT("EvalForAll_H");
	return TRUE;
}



BOOL EvalForAll_HNoReset
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;
	CELLP pcVars;
	CELLP pcGenLit;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index
	double hsp_cost;
	
	ENTER("EvalForAll_H",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	nBaseSP=HGetBTreeSP();


	hsp_cost=dEvalTotalCost;

	if(!pcArgs)
	{
		EXIT("EvalForAll_H");
		return FALSE;
	}
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{

		if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings))
		{
			HSetBTreeSP(nBaseSP);		// restore btree stack pointer
			EXIT("EvalForAll_H");
			return FALSE;
		}
	}
	EXIT("EvalForAll_H");
	return TRUE;
}




BOOL EvalForAllNoGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;

	ENTER("EvalForAllNoGenerator",TRUE);
	pcArgs=pcFormula->pfForm->pcArgs;

	if(!pcArgs)
	{
		EXIT("EvalForAllNoGenerator");
		return FALSE;
	}
	
	if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings)) {
	  EXIT("EvalForAllNoGenerator");
	  return FALSE;
	}

	EXIT("EvalForAllNoGenerator");
	return TRUE;
}


BOOL EvalExistsNoGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;

	ENTER("EvalExistsNoGenerator",TRUE);
	pcArgs=pcFormula->pfForm->pcArgs;

	if(!pcArgs)
	{
		EXIT("EvalExistsNoGenerator");
		return FALSE;
	}
	
	if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings)) {
	  EXIT("EvalExistsNoGenerator");
	  return FALSE;
	}

	EXIT("EvalExistsNoGenerator");
	return TRUE;
}


BOOL EvalExistsXNoGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;

	ENTER("EvalExistsXNoGenerator",TRUE);
	pcArgs=pcFormula->pfForm->pcArgs;

	if(!pcArgs)
	{
		EXIT("EvalExistsXNoGenerator");
		return TRUE;
	}
	
	if(!(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpLinearPlan,pbBindings)) {
	  EXIT("EvalExistsXNoGenerator");
	  return FALSE;
	}

	EXIT("EvalExistsXNoGenerator");
	return TRUE;
}



/* EvalExists

Description:
	Evaluate an EXISTS formula.
Scheme:

(define (eval-exists args world/action bindings)
	(let* ((variables (get-qf-variables args))
			(gen-lit (get-qf-generator args))
			(formula (get-qf-formula args))
			(generator '()))
		(set! bindings (extend-bindings variables variables bindings))
		(set! generator (make-generator gen-lit variables world/action bindings))
		(if generator
			(do ((finish? #f)
					(result '()))
				(finish? result)
				(cond
					((generator)
						(if (or (null? formula)
								(eval-formula formula world/action bindings))
							(begin (set! finish? #t)
								(set! result #t))))
					(else (set! finish? #t)
						(set! result #f))))
			#f)))
*/

BOOL EvalExists
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pc;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index
	
	ENTER("EvalExists",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pc=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{
	
		if(!pc||(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
		{
			SetBTreeSP(nBaseSP);		// restore btree stack pointer
			EXIT("EvalExists");
			return TRUE;
		}
	}
	EXIT("EvalExists");
	return FALSE;
}

/* Same as EvalExists, but for h-worlds */

BOOL EvalExists_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pc;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index

	ENTER("EvalExists_H",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pc=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	nBaseSP=HGetBTreeSP();

	
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{

	  if(!pc||(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
	    {
			HSetBTreeSP(nBaseSP);		// restore btree stack pointer
			EXIT("EvalExists");
			//fprintf(HandleToFileStream(3),"exists %p returnung TRUE\n",pc); 
			return TRUE;
		}
	}

	EXIT("EvalExists_H");
	return FALSE;
}


/* EvalExistsX

Description:
	Evaluate an EXISTS! (exists unique) formula.
Scheme:

(define (eval-exists! args world/action bindings)
  (let* ((variables (get-qf-variables args))
		 (gen-lit   (get-qf-generator args))
		 (formula   (get-qf-formula args))
		 (generator '()))
	(set! bindings (extend-bindings variables variables bindings))
	(set! generator
	  (make-generator gen-lit variables world/action bindings))
	(if generator
		(do ((finish? #f)
			 (result '())
			 (found-one? #f))
			(finish? result)
		  (if (generator)
		  (if (or (null? formula)
			  (eval-formula formula world/action bindings))
		  (cond(found-one?
			(set! finish? #t)
			(set! result '()))
			   (else (set! found-one? #t))))
		  (cond(found-one?
			(set! finish? #t)
			(set! result #t))
		   (else (set! finish? #t)
			 (set! result '())))))
	#f)))
*/

BOOL EvalExistsX
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pc;
	void *pvContext;
	BOOL bFoundOne;
	int nBaseSP;						// saved base btree stack index

	ENTER("EvalExistsX",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pc=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	bFoundOne=FALSE;
	nBaseSP=GetBTreeSP();

	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{
		if(!pc||(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
		{
			if(bFoundOne)
			{
				SetBTreeSP(nBaseSP);		// restore btree stack pointer
				EXIT("EvalExistsX");
				return FALSE;
			}
			bFoundOne=TRUE;
		}
	}
	EXIT("EvalExistsX");
	return bFoundOne;
}


BOOL EvalExistsX_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pc;
	void *pvContext;
	BOOL bFoundOne;
	int nBaseSP;						// saved base btree stack index

	ENTER("EvalExistsX_H",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pc=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* push */
	pvContext=NULL;
	bFoundOne=FALSE;
	nBaseSP=HGetBTreeSP();

	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
	{
		if(!pc||(*pc->pfForm->paAction->pfEval)(pc,plpLinearPlan,pbBindings))
		{
			if(bFoundOne)
			{
				HSetBTreeSP(nBaseSP);		// restore btree stack pointer
				EXIT("EvalExistsX");
				return FALSE;
			}
			bFoundOne=TRUE;
		}
	}
	EXIT("EvalExistsX_H");
	return bFoundOne;
}



/* EvalGoal

Description:
	Evaluate the truth of a goal formula.
Scheme:

(define (eval-goal args world/action bindings)
  (eval-formula (first args) (get-goal-world) bindings))
*/

BOOL EvalGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalGoal",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	b=(*pc->pfForm->paAction->pfEval)(pc,GetGoalWorld(TRUE),pbBindings);
	EXIT("EvalGoal");
	return b;
}

/* EvalCurrent

Description:
	Evaluate the truth of a current formula.
*/

BOOL EvalCurrent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("EvalCurrent",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	b=(*pc->pfForm->paAction->pfEval)(pc,plpCurrentPlan,pbBindings);
	EXIT("EvalCurrent");
	return b;
}

/* EvalPrevious

Description:
	Evaluate the truth of a previous formula.
*/

BOOL EvalPrevious
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	LINEARPLANP plp;
	BOOL b;

	ENTER("EvalPrevious",TRUE);

	/* if the current plan is not null, get the previous plan */
	
	plp=plpLinearPlan;
	if(plp)
		plp=plp->plpParent;

	/* if the previous plan exists, evaluate the passed formula, 
	otherwise return FALSE */
	
	pc=pcFormula->pfForm->pcArgs;
	if(plp)
		b=(*pc->pfForm->paAction->pfEval)(pc,plp,pbBindings);
	else
		b=FALSE;
	EXIT("EvalPrevious");
	return b;
}

/* Pseudo Predicates -------------------------------------------------------- */

/* EvalAssign

Description:
	Evaluate a := pseudo predicate.
*/

BOOL EvalAssign
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVar,pcVal;

	ENTER("EvalAssign",TRUE);
	pcVar=pcFormula->pfForm->pcArgs;
	pcVal=ComputeTerm(pcVar->pcNext,plpLinearPlan,pbBindings);
	if(!pcVal)
		TermError("eval-assign",pcFormula,pbBindings);
	if(pcVar->pfForm->pcArgs)
		SetArrayX(pcVar,pcVal,plpLinearPlan,pbBindings);
	else
		SetVarX(pcVar,pcVal,pbBindings);
	EXIT("EvalAssign");
	return TRUE;
}

/* EvalAssignAppend

Description:
	Evaluate a :<< pseudo predicate.
Notes:
	Until we change an array-info structure to be an array of cells,
	we can't support the append operation reliably!
*/

BOOL EvalAssignAppend
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVar,pcVal;
	CELLP pc;

	ENTER("EvalAssignAppend",TRUE);
	pcVar=pcFormula->pfForm->pcArgs;
	if(pcVar->pfForm->pcArgs)
	{
		ErrorMessage("Assign-append is not supported on arrays\n");
		return FALSE;
	}
	for(pc=pcVar->pcNext;pc;pc=pc->pcNext)
	{
		if(pc->pfForm->nType==ATOM_SYMBOLINFOP&&
			!DescribedQ(pc->pfForm->uValue.psiSymbolInfo)&&
			FunctionQ(pc->pfForm->uValue.psiSymbolInfo))
			pcVal=ComputeTerm(pc,plpLinearPlan,pbBindings);
		else
//			pcVal=CopyCell(pc);
			pcVal=pc;
		if(!pcVal)
			TermError("eval-append",pcFormula,pbBindings);
		else
			AppendVarX(pcVar,pcVal,pbBindings);
	}
	EXIT("EvalAssignAppend");
	return TRUE;
}

/* EvalSearchGlobalInitialization

Description:
	Evaluate an search-global-initialization pseudo predicate.
	This is an internal form of eval-assign.
*/

BOOL EvalSearchGlobalInitialization
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVar,pcVal;
	ARRAYINFOP pai;
	CELLP *ppc;							/* array pointer */
	int i;

	ENTER("EvalSearchGlobalInitialization",TRUE);
	pcVar=pcFormula->pfForm->pcArgs;
	pcVal=ComputeTerm(pcVar->pcNext,plpLinearPlan,pbBindings);
	if(!pcVal)
		TermError("eval-search-global-initialization",pcFormula,pbBindings);
	if(pcVar->pfForm->nType==ATOM_ARRAYINFOP)
	{
		pcVar=LookupVar(pcVar,pbBindings);
		pai=pcVar->pfForm->uValue.paiArrayInfo;
		ppc=pai->ppcArray;
		for(i=0;i<pai->nCount;i++)
			*ppc++=pcVal;	
	}
	else
		SetVarX(pcVar,pcVal,pbBindings);
	EXIT("EvalSearchGlobalInitialization");
	return TRUE;
}

/* EvalPrint

Description:
	Evaluate a print pseudo predicate.

	(print <stream> <format> <args...>)

*/

BOOL EvalPrint
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[512];					/* output buffer */
	CELLP pcStream;
	CELLP pcFormat;
	CELLP pcArgs;
	int nFile;							/* file handle */
	FILE *pfs;							/* output file stream */
	char *psFormat;						/* format string */
	
	ENTER("EvalPrint",FALSE);

	/* check arguments */
	
	pcStream=pcFormula->pfForm->pcArgs;
	if(!pcStream)
	{
		ErrorMessage("print:  No arguments specified\n");
		return TRUE;
	}
	if(!FormulaToInteger(pcStream,plpLinearPlan,pbBindings,(int *)&nFile))
		return FALSE;
	if(nFile<0||nFile>MAXFILE-1)
	{
		ErrorMessage("print:  File handle out of range: %d\n",nFile);
		return TRUE;
	}
	pfs=aftFileTable[nFile].pfFile;
	if(!pfs)
	{
		ErrorMessage("print:  File handle not in use: %d\n",nFile);
		return TRUE;
	}
	pcFormat=pcStream->pcNext;
	if(!pcFormat)
	{
		ErrorMessage("print:  No format specified\n");
		return TRUE;
	}
	if(!FormulaToString(pcFormat,plpLinearPlan,pbBindings,&psFormat))
		return FALSE;
	if(!psFormat||!strlen(psFormat))
	{
		ErrorMessage("print:  No format specified\n");
		return TRUE;
	}

	pcArgs=pcFormat->pcNext;
	FormatText(acBuffer,psFormat,pcArgs,plpLinearPlan,pbBindings);
	if(strlen(acBuffer)>sizeof(acBuffer))
	{
		ErrorMessage("print:  Output overran internal buffer (%d bytes), exiting\n",
			sizeof(acBuffer));
		exit(1);
	}
	CommandPrintf(pfs,"%s",acBuffer);
	EXIT("EvalPrint");
	return TRUE;
}

/* EvalPlan 

Description:
	Invoke the planner on current world and goal.
Returns:
	TRUE iff a plan was created.
Scheme:

(define (plan )
	(tlplanner (get-initial-world) (get-tl-control) (get-search-strategy)
		(get-priority-fn) (get-cost-fn)))
*/

BOOL EvalPlan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int i;
	BOOL bStatus;
	jmp_buf *pjbSaved;
	int nLineNumberSaved;

	/* make sure we can go through with this */

	ENTER("EvalPlan",FALSE);
	bStatus=TRUE;						/* assume success */
	for(i=0;i<nNumberOfWorldSymbols;i++)
	{
		if(asiWorldSymbols[i].nSymbolType==SYMBOL_UNKNOWN)
		{
			ErrorMessage("plan:  Symbol %s has unknown type\n",
				asiWorldSymbols[i].psName);
			bStatus=FALSE;
		}
		if(asiWorldSymbols[i].nEvalType==EVAL_UNKNOWN)
		{
			ErrorMessage("plan:  Symbol %s is undefined\n",
				asiWorldSymbols[i].psName);
			bStatus=FALSE;
		}
	}
	if(!bStatus)
	{
		ErrorMessage("plan:  Ensure all predicates, functions and generators ",
			"are correctly defined\nbefore calling plan.\n");
		EXIT("EvalPlan");
		return FALSE;
	}
	if(nErrorCount&&!bInteractiveMode)
	{
		ErrorMessage("plan:  %d Error%s encountered.\n"
			"Please correct any problems and call clear-world-symbols before proceeding.\n",
			nErrorCount,nErrorCount==1?" was":"s were");
		EXIT("EvalPlan");
		return FALSE;
	}
	if(!plpInitialWorld)
	{
		ErrorMessage("plan:  No initial world.\n"
			"Please call set-initial-world before proceeding.\n");
		EXIT("EvalPlan");
		return FALSE;
	}

	/* call the planner */

	if(bVerbose)
		Message("Planning \"%s\"\n",psPlanName);
	nLineNumberSaved=nLineNumber;
	nLineNumber=0;						/* turn off line number messages */
	plpFinalPlan=NULL;
	pjbSaved=pjbCurrentJump;
	pjbCurrentJump=&jbTLPlannerJump;
	bStatus=FALSE;						/* assume failure */
	if(!setjmp(jbTLPlannerJump))		/* set up for quick exit from planner */
		bStatus=TLPlanner(plpInitialWorld,pcTLForm,pfSearchStrategy,
			pcHeuristicFn,pcPriorityFn,pbBindings);
	nLineNumber=nLineNumberSaved;
	pjbCurrentJump=pjbSaved;
	EXIT("EvalPlan");
	return bStatus;
}

/* EvalSetPlanName

Description:
	Evaluate a plan-name pseudo-predicate.
	Plan-name specifies a name for the current problem.
*/

BOOL EvalSetPlanName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char acBuffer[_MAX_PATH];
	char *psFormat;
	int nPosition;

	ENTER("EvalSetPlanName",FALSE);

	/* check arguments */
	
	if(!pcFormula->pfForm->pcArgs)
	{
		ErrorMessage("set-plan-name:  No arguments specified\n");
		return TRUE;
	}
	FormulaToString(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&psFormat);
	if(!psFormat||!strlen(psFormat))
	{
		ErrorMessage("set-plan-name:  No format specified\n");
		return TRUE;
	}
				
	FormatText(acBuffer,psFormat,
		pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings);
	psPlanName=StrAllocAndPos(acBuffer,&nPosition);	
	EXIT("EvalSetPlanName");
	return TRUE;
}

/* EvalResetPlanName

Description:
	Reset (clear) the plan-name.
*/

BOOL EvalResetPlanName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalResetPlanName",FALSE);
	psPlanName=NULL;
	EXIT("EvalResetPlanName");
	return TRUE;
}

/* EvalExit

Description:
	Evaluate an exit pseudo-predicate.
*/

BOOL EvalExit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("EvalExit",FALSE);

	bImmediateExit=TRUE;
	EXIT("EvalPlanName");
	return TRUE;
}

/* EvalPrintWorld

Description:
	Evaluate a print-world pseudo-predicate.
*/

BOOL EvalPrintWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nFile;

	ENTER("EvalPrintWorld",FALSE);
	if(!FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile))
	{
		EXIT("EvalPrintWorld");
		return FALSE;
	}
	PrintWorld(nFile,plpLinearPlan,pbBindings);
	EXIT("EvalPrintWorld");
	return TRUE;
}

/* EvalPrintWorldList

Description:
	Write all the described symbols to the specified stream.
*/

BOOL EvalPrintWorldList
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nFile;
	FILE *pfFile;
	BTREEP *apbtWorld;
	int i;
	CELLP pcStart;
	CELLP pc;

	ENTER("EvalPrintWorldList",TRUE);

	if(!FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile))
	{
		EXIT("EvalPrintWorldList");
		return FALSE;
	}
	pfFile=HandleToFileStream(nFile);
	if(!pfFile)
	{
		ErrorMessage("print-world-list:  Invalid file handle, %d\n",nFile);
		EXIT("EvalPrintWorldList");
		return FALSE;
	}
	
	if(plpLinearPlan)
	{
		apbtWorld=LinearPlanWorld(plpLinearPlan);
		for(i=0;i<nNumberOfDescribedSymbols;i++)
		{
//			pcStart=ArgTreeToFormulaList(GetSymbolName(i),GetSymbolArgs(apbtWorld,i));
			pcStart=ArgTreeToFormulaList(apbtWorld,i);
			for(pc=pcStart;pc;pc=pc->pcNext)
				PrintFormula(pfFile,pc,0);
		}
	}
	EXIT("EvalPrintWorldList");
	return TRUE;
}

/* Formula Predicates ---------------------------------------------------------- */

/* Here, for the sake of efficiency, instead of testing the formula's name,
we test one of the member function pointers (one that exists).  */

/* EqFormQ

Description:
	Test if formula is an = formula.
Scheme:

(define (=-formp form)
  (eq? (get-operator form) '=))
*/

BOOL EqFormQ
(
	CELLP pcFormula							/* formula */
)
{
	ENTER("EqFormQ",TRUE);
	EXIT("EqFormQ");
	return pcFormula->pfForm->paAction==&aEqAction;
}

/* PlusEqFormQ

Description:
	Test if formula is a += formula.
*/

BOOL PlusEqFormQ
(
	CELLP pcFormula							/* formula */
)
{
	ENTER("PlusEqFormQ",TRUE);
	EXIT("PlusEqFormQ");
	return pcFormula->pfForm->paAction==&aPlusEqAction;
}

/* MinusEqFormQ

Description:
	Test if formula is a -= formula.
*/

BOOL MinusEqFormQ
(
	CELLP pcFormula							/* formula */
)
{
	ENTER("MinusEqFormQ",TRUE);
	EXIT("MinusEqFormQ");
	return pcFormula->pfForm->paAction==&aMinusEqAction;
}

/* TrueFormQ

Description:
	Test if formula is a TRUE formula (predicate).
Scheme:
*/

BOOL TrueFormQ
(
	CELLP pcFormula
)
{
	ENTER("TrueFormQ",TRUE);
	EXIT("TrueFormQ");
	return pcFormula->pfForm->paAction==&aTrueAction;
}

/* FalseFormQ

Description:
	Test if formula is a FALSE formula (predicate).
Scheme:
*/

BOOL FalseFormQ
(
	CELLP pcFormula
)
{
	ENTER("FalseFormQ",TRUE);
	EXIT("FalseFormQ");
	return pcFormula->pfForm->paAction==&aFalseAction;
}

/* AndFormQ

Description:
	Test if formula is an AND formula (predicate).
Scheme:

(define (and-formp form)
  (eq? (get-operator form) 'AND))
*/

BOOL AndFormQ
(
	CELLP pcFormula							/* formula */
)
{
	ENTER("AndFormQ",TRUE);
	EXIT("AndFormQ");
//	return pcFormula->pfForm->paAction->pfProgress==ProgressAnd;
	return pcFormula->pfForm->paAction==&aAndAction;
}

/* OrFormQ

Description:
	Test if formula is an OR formula (predicate).
Scheme:

(define (or-formp form)
  (eq? (get-operator form) 'OR))
*/

BOOL OrFormQ
(
	CELLP pcFormula
)
{
	ENTER("OrFormQ",TRUE);
	EXIT("OrFormQ");
	return pcFormula->pfForm->paAction==&aOrAction;
}

/* XorFormQ

Description:
	Test if formula is an XOR formula (predicate).
Scheme:

(define (xor-formp form)
  (eq? (get-operator form) 'XOR))
*/

BOOL XorFormQ
(
	CELLP pcFormula
)
{
	ENTER("XorFormQ",TRUE);
	EXIT("XorFormQ");
	return pcFormula->pfForm->paAction==&aXorAction;
}

/* IfThenElseFormQ

Description:
	Test if formula is an IfThenElse formula (predicate).
Scheme:

(define (if-then-else-formp form)
  (eq? (get-operator form) 'IF-THEN-ELSE))
*/

//BOOL IfThenElseFormQ
//(
//	CELLP pcFormula
//)
//{
//	ENTER("IfThenElseFormQ",TRUE);
//	EXIT("IfThenElseFormQ");
//	return pcFormula->pfForm->paAction==&aIfThenElseAction;
//}

/* NotFormQ

Description:
	Test if formula is a NOT formula (predicate).
Scheme:

(define (not-formp form)
  (eq? (get-operator form) 'NOT))
*/

BOOL NotFormQ
(
	CELLP pcFormula
)
{
	ENTER("NotFormQ",TRUE);
	EXIT("NotFormQ");
	return pcFormula->pfForm->paAction==&aNotAction;
}

/* ImpliesFormQ

Description:
	Test if formula is a IMPLIES formula (predicate).
Scheme:

(define (implies-formp form)
  (eq? (get-operator form) 'IMPLIES))
*/

BOOL ImpliesFormQ
(
	CELLP pcFormula
)
{
	ENTER("ImpliesFormQ",TRUE);
	EXIT("ImpliesFormQ");
	return pcFormula->pfForm->paAction==&aImpliesAction;
}

/* GoalFormQ

Description:
	Test if formula is a GOAL modality formula (predicate).
Scheme:

(define (goal-formp form)
  (eq? (get-operator form) 'GOAL))
*/

BOOL GoalFormQ
(
	CELLP pcFormula
)
{
	ENTER("GoalFormQ",TRUE);
	EXIT("GoalFormQ");
	return pcFormula->pfForm->paAction==&aGoalAction;
}

/* PreviousFormQ

Description:
	Test if formula is a PREVIOUS modality formula (predicate).
*/

BOOL PreviousFormQ
(
	CELLP pcFormula
)
{
	ENTER("PreviousFormQ",TRUE);
	EXIT("PreviousFormQ");
	return pcFormula->pfForm->paAction==&aPreviousAction;
}

/* PermuteFormQ

Description:
	Test if formula is a PERMUTE modality formula (predicate).
*/

static BOOL PermuteFormQ
(
	CELLP pcFormula
)
{
	ENTER("PermuteFormQ",TRUE);
	EXIT("PermuteFormQ");
	return pcFormula->pfForm->paAction==&aPermuteAction;
}

/* ForAllFormQ

Description:
	Test if formula is a FORALL formula (quantifier).
Scheme:

(define (forall-formp form)
  (eq? (get-operator form) 'FORALL))
*/

BOOL ForAllFormQ
(
	CELLP pcFormula
)
{
	ENTER("ForAllFormQ",TRUE);
	EXIT("ForAllFormQ");
	return pcFormula->pfForm->paAction==&aForAllAction;
}

/* ExistsFormQ

Description:
	Test if formula is an EXISTS formula (quantifier).
Scheme:

(define (exists-formp form)
  (eq? (get-operator form) 'EXISTS))
*/

BOOL ExistsFormQ
(
	CELLP pcFormula
)
{
	ENTER("ExistsFormQ",TRUE);
	EXIT("ExistsFormQ");
	return pcFormula->pfForm->paAction==&aExistsAction;
}

/* ExistsXFormQ

Description:
	Test if formula is an EXISTS! (exists unique) formula (quantifier).
Scheme:

(define (exists!-formp form)
  (eq? (get-operator form) 'EXISTS!))
*/

BOOL ExistsXFormQ
(
	CELLP pcFormula
)
{
	ENTER("ExistsXFormQ",TRUE);
	EXIT("ExistsXFormQ");
	return pcFormula->pfForm->paAction==&aExistsXAction;
}

/* NextFormQ

Description:
	Test if formula is a NEXT formula (modality).
Scheme:

(define (next-formp form)
  (eq? (get-operator form) 'NEXT))
*/

static BOOL NextFormQ
(
	CELLP pcFormula
)
{
	ENTER("NextFormQ",TRUE);
	EXIT("NextFormQ");
	return pcFormula->pfForm->paAction==&aNextAction;
}

/* EventuallyFormQ

Description:
	Test if formula is an EVENTUALLY formula (modality).
Scheme:

(define (eventually-formp form)
  (eq? (get-operator form) 'EVENTUALLY))
*/

static BOOL EventuallyFormQ
(
	CELLP pcFormula
)
{
	ENTER("EventuallyFormQ",TRUE);
	EXIT("EventuallyFormQ");
	return pcFormula->pfForm->paAction==&aEventuallyAction;
}

/* AlwaysFormQ

Description:
	Test if formula is an ALWAYS formula (modality).
Scheme:

(define (always-formp form)
  (eq? (get-operator form) 'ALWAYS))
*/

static BOOL AlwaysFormQ
(
	CELLP pcFormula
)
{
	ENTER("AlwaysFormQ",TRUE);
	EXIT("AlwaysFormQ");
	return pcFormula->pfForm->paAction==&aAlwaysAction;
}

/* UntilFormQ

Description:
	Test if formula is an UNTIL formula (modality).
Scheme:

(define (until-formp form)
  (eq? (get-operator form) 'UNTIL))
*/

static BOOL UntilFormQ
(
	CELLP pcFormula
)
{
	ENTER("UntilFormQ",TRUE);
	EXIT("UntilFormQ");
	return pcFormula->pfForm->paAction==&aUntilAction;
}

/* MTL Changes. Syntax checkers for bounded temporal operators. */

/* TUntilFormQ

Description:
	Test if formula is a bounded until (modality).
Scheme:

(define (t-until-formp form)
  (eq? (get-operator form) 'T-UNTIL))
*/

static BOOL TUntilFormQ
(
	CELLP pcFormula
)
{
	ENTER("TUntilFormQ",TRUE);
	EXIT("TUntilFormQ");
	return pcFormula->pfForm->paAction==&aTUntilAction;
}

/* TEventuallyFormQ

Description:
	Test if formula is a bounded eventually (modality).
Scheme:

(define (t-eventually-formp form)
  (eq? (get-operator form) 'T-EVENTUALLY))
*/

static BOOL TEventuallyFormQ
(
	CELLP pcFormula
)
{
	ENTER("TEventuallyFormQ",TRUE);
	EXIT("TEventuallyFormQ");
	return pcFormula->pfForm->paAction==&aTEventuallyAction;
}

/* TAlwaysFormQ

Description:
	Test if formula is a bounded always (modality).
Scheme:

(define (t-always-formp form)
  (eq? (get-operator form) 'T-ALWAYS))
*/

static BOOL TAlwaysFormQ
(
	CELLP pcFormula
)
{
	ENTER("TAlwaysFormQ",TRUE);
	EXIT("TAlwaysFormQ");
	return pcFormula->pfForm->paAction==&aTAlwaysAction;
}

/* DeltaFormQ

Description:
	Test if formula is a DELTA formula (modality).
Scheme:

(define (delta-formp form)
  (eq? (get-operator form) 'DELTA))
*/

static BOOL DeltaFormQ
(
	CELLP pcFormula
)
{
	ENTER("DeltaFormQ",TRUE);
	EXIT("DeltaFormQ");
	return pcFormula->pfForm->paAction==&aDeltaAction;
}

/* BindingFormQ

Description:
	Test if formula is a BINDING formula (modality).
Scheme:

(define (binding-formp form)
  (eq? (get-operator form) 'BINDING))
*/

BOOL BindingFormQ
(
	CELLP pcFormula
)
{
	ENTER("BindingFormQ",TRUE);
	EXIT("BindingFormQ");
	return pcFormula->pfForm->paAction==&aBindingAction;
}

/* AtomicFormQ

Description:
	Test if formula is an atomic formula.
Notes...
	We need a more efficient way to do this... a set of flags in the action
	structure would be much better!
Scheme:

(define (atomic-formp form)
  (not
   (or (or-formp form)
	   (and-formp form)
	   (forall-formp form)
	   (exists-formp form)
	   (not-formp form)
	   (implies-formp form)
	   (goal-formp form)
	   (call-formp form)
	   (next-formp form)
	   (eventually-formp form)
	   (always-formp form)
	   (until-formp form)
	   (t-until-formp form)
	   (t-eventually-formp form)
	   (t-always-formp form)
	   (delta-formp form)
	   (binding-formp form))))
*/

BOOL AtomicFormQ
(
	CELLP pcFormula
)
{
	ENTER("AtomicFormQ",TRUE);
	EXIT("AtomicFormQ");
	return 
		!(OrFormQ(pcFormula)||
		AndFormQ(pcFormula)||
		ForAllFormQ(pcFormula)||
		ExistsFormQ(pcFormula)||
		ExistsXFormQ(pcFormula)||
		NotFormQ(pcFormula)||
		ImpliesFormQ(pcFormula)||
		GoalFormQ(pcFormula)||
		PermuteFormQ(pcFormula)||
		PreviousFormQ(pcFormula)||
		NextFormQ(pcFormula)||
		EventuallyFormQ(pcFormula)||
		AlwaysFormQ(pcFormula)||
		UntilFormQ(pcFormula)||
		TUntilFormQ(pcFormula)||
		TEventuallyFormQ(pcFormula)||
		TAlwaysFormQ(pcFormula)||
		DeltaFormQ(pcFormula)||
		BindingFormQ(pcFormula));
}

/* AddFormQ

Description:
	Test if formula is an ADD formula (pseudo-predicate).
*/

BOOL AddFormQ
(
	CELLP pcFormula					/* formula */
)
{
	ENTER("AddFormQ",TRUE);
	EXIT("AddFormQ");
	return pcFormula->pfForm->paAction==&aAddAction;
}

/* DelFormQ

Description:
	Test if formula is a DEL formula (pseudo-predicate).
*/

BOOL DelFormQ
(
	CELLP pcFormula					/* formula */
)
{
	ENTER("DelFormQ",TRUE);
	EXIT("DelFormQ");
	return pcFormula->pfForm->paAction==&aDelAction;
}

/* Binding Formula Routines ---------------------------------------------------- */

/* GetBindingVars

Description:
	Fetch the variables associated with a binding formula.
Scheme:

(define (get-binding-vars binding-form)
  (first (rest binding-form)))
*/

CELLP GetBindingVars
(
	CELLP pcBindingForm
)
{
	ENTER("GetBindingVars",TRUE);
	EXIT("GetBindingVars");
	return pcBindingForm->pfForm->pcVars;
}

/* GetBindingVals

Description:
	Fetch the values associated with a binding formula.
Scheme:

(define (get-binding-vals binding-form)
  (second (rest binding-form)))
*/

CELLP GetBindingVals
(
	CELLP pcBindingForm
)
{
	ENTER("GetBindingVals",TRUE);
	EXIT("GetBindingVals");
	return pcBindingForm->pfForm->pcGenLit;
}

/* GetBindingFormula

Description:
	Fetch the "formula" associated with a binding formula.
Scheme:

(define (get-binding-formula binding-form)
  (third (rest binding-form)))
*/

CELLP GetBindingFormula
(
	CELLP pcBindingForm
)
{
	ENTER("GetBindingFormula",TRUE);
	EXIT("GetBindingFormula");
	return pcBindingForm->pfForm->pcArgs;
}

/* LookupVar

Description:
	Find a variable binding, or complain if none.
Scheme:

(define (lookup-var var bindings)
  (let ((cell (assoc var bindings)))
	(if cell
	(rest cell)
	(error "Formula contains unbound variable ~S" var))))
*/

DECLSPEC CELLP LookupVar
(
	CELLP pcVar,
	BINDINGP pbBindings
)
{
	BINDINGP pb;
	char ac[40];

	ENTER("LookupVar",TRUE);
	for(pb=pbBindings;pb;pb=pb->pbNext)
	{
		if(StringEqQ(IdentName(pcVar->pfForm),IdentName(pb->pcVar->pfForm)))
		{
			EXIT("LookupVar");
			return pb->pcVal;
		}
	}
	Message("LookupVar:  Formula contains unbound variable %s\n"
		"Current bindings:\n",GetName(pcVar->pfForm,ac));
	DumpBindings(pbBindings);
//	if(pjbCurrentJump==&jbTLPlannerJump)
	if(pjbCurrentJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	EXIT("LookupVar");
	return NULL;
}

/* SetVarX

Description:
	Destructively set var's binding in bindings.
Note:
	The passed value becomes the property of SetVarX.
Scheme:

(define (set-var! var val bindings)
  (let ((cell (assoc var bindings)))
	(if cell
	(set-cdr! cell val)
	(error "Formula contains unbound variable ~S" var))))
*/

DECLSPEC void SetVarX
(
	CELLP pcVar,
	CELLP pcVal,
	BINDINGP pbBindings
)
{
	BINDINGP pb;
	char ac[40];

	ENTER("SetVarX",TRUE);
	for(pb=pbBindings;pb;pb=pb->pbNext)
	{
		if(StringEqQ(IdentName(pcVar->pfForm),IdentName(pb->pcVar->pfForm)))
		{
			pb->pcVal=pcVal;
			EXIT("SetVarX");
			return;
		}
	}
	ErrorMessage("SetVarX:  Formula contains unbound variable %s\n",GetName(pcVar->pfForm,ac));
	EXIT("SetVarX");
}

/* AppendVarX

Description:
	Append values to var's binding in bindings.
Note:
	The passed value becomes the property of AppendVarX.
*/

DECLSPEC void AppendVarX
(
	CELLP pcVar,						/* variable to append to */
	CELLP pcVal,						/* value to append */
	BINDINGP pbBindings
)
{
	BINDINGP pb;
	CELLP pc;
	char ac[40];

	ENTER("AppendVarX",TRUE);
	for(pb=pbBindings;pb;pb=pb->pbNext)
	{
		if(StringEqQ(IdentName(pcVar->pfForm),IdentName(pb->pcVar->pfForm)))
		{
			if(FormulaEqQ(pcVar,pb->pcVal))
				pb->pcVal=CopyCell(pcVal);
			else
			{
				for(pc=pb->pcVal;pc->pcNext;pc=pc->pcNext);
					pc->pcNext=CopyCell(pcVal);
			}
			EXIT("AppendVarX");
			return;
		}
	}
	ErrorMessage("AppendVarX:  Formula contains unbound variable %s\n",GetName(pcVar->pfForm,ac));
	EXIT("AppendVarX");
}

DECLSPEC void SetArrayX
(
	CELLP pcArray,
	CELLP pcVal,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BINDINGP pb;
	int nIndex;
	ARRAYINFOP pai;
	char ac[40];

	ENTER("SetArrayX",TRUE);
	for(pb=pbBindings;pb;pb=pb->pbNext)
	{
		if(StringEqQ(IdentName(pcArray->pfForm),IdentName(pb->pcVar->pfForm)))
		{
			if(ArrayIndex(pcArray,pb->pcVal,plpLinearPlan,pbBindings,&nIndex))
			{
				pai=pb->pcVal->pfForm->uValue.paiArrayInfo;
				pai->ppcArray[nIndex]=pcVal;
			}
			EXIT("SetArrayX");
			return;
		}
	}
	ErrorMessage("SetArrayX:  Formula contains unbound variable %s\n",GetName(pcArray->pfForm,ac));
	EXIT("SetArrayX");
}

/* ExtendBindings

Description:
	Extend the old bindings by placing new bindings in front.
	Some of the new bindings may override matching old bindings.
Scheme:

(define (extend-bindings vars vals bindings)
  (append! (map cons vars vals) bindings))
*/

DECLSPEC BINDINGP ExtendBindings
(
	CELLP pcVars,						/* new variables */
	CELLP pcVals,						/* new values */
	BINDINGP pbBindings					/* old bindings */
)
{
	BINDINGP pbStart,pbEnd;				/* bindings listhead */
	CELLP pc1,pc2;

	ENTER("ExtendBindings",TRUE);
	pbStart=NULL;						/* initialize listhead */
	pbEnd=(BINDINGP)&pbStart;

	for(pc1=pcVars,pc2=pcVals;pc1&&pc2;pc1=pc1->pcNext,pc2=pc2->pcNext)
	{
		pbEnd=pbEnd->pbNext=(BINDINGP)MemAlloc(sizeof(BINDING));
		pbEnd->pcVar=pc1;
		pbEnd->pcVal=pc2;
	}
	if(pc1||pc2)
		ErrorMessage("Bindings variable and value lists are different lengths\n");
	pbEnd->pbNext=pbBindings;

	EXIT("ExtendBindings");
	return pbStart;
}

/* CopyBindingsShared

Description:
	Copy a bindings list.  This routine implements shared persistent bindings
	for action promises.  Given two bindings lists that may differ in their
	outer bindings, create a new bindings lists that duplicates the second
	list, but shares as much as possible of the first list.
Notes:
	We assume that the bindings lists are the same length, though the first
	list may be empty.  We also assume that
	the variables in the lists are pair-wise identical, so we do not compare them.
*/

BINDINGP CopyBindingsShared
(
	BINDINGP pbShare,					/* bindings to share */
	BINDINGP pbCopy						/* bindings to copy */
)
{
	BINDINGP pb1,pb2;					/* utility pointers */
	BINDINGP pbShareable;				/* pointer to first shareable binding */
	BINDINGP pbStart,pbEnd;				/* new bindings list pointers */

	ENTER("CopyBindingsShared",TRUE);

	// scan the two bindings lists, looking for the first shareable binding

	pbShareable=NULL;					/* in case the share list is empty */
	for(pb1=pbShare,pb2=pbCopy;pb1;pb1=pb1->pbNext,pb2=pb2->pbNext)
	{
		if(!FormulaEqQ(pb1->pcVal,pb2->pcVal))
			pbShareable=pb1->pbNext;
	}

	// handle an empty share list or an unshareable list by copying the entire copy list

	if(!pbShareable)
	{
		pbStart=NULL;
		pbEnd=(BINDINGP)&pbStart;
		for(pb2=pbCopy;pb2;pb2=pb2->pbNext)
			pbEnd=pbEnd->pbNext=(BINDINGP)CopyAlloc(pb2,sizeof(BINDING));
		pbEnd->pbNext=NULL;
		
		EXIT("CopyBindingsShared");
		return pbStart;
	}
	
	// copy the bindings until we get to the first shareable binding
	
	pbStart=NULL;
	pbEnd=(BINDINGP)&pbStart;
	for(pb1=pbShare,pb2=pbCopy;pb1!=pbShareable;pb1=pb1->pbNext,pb2=pb2->pbNext)
		pbEnd=pbEnd->pbNext=(BINDINGP)CopyAlloc(pb2,sizeof(BINDING));
	pbEnd->pbNext=pbShareable;

	EXIT("CopyBindingsShared");
	return pbStart;
}

/* Miscellaneous Formula Stuff ------------------------------------------------- */

/* GetDeltaFormula

Description:
Scheme:

(define (get-delta-formula delta-form)
  (rest delta-form))
*/

CELLP GetDeltaFormula
(
	CELLP pcDeltaForm
)
{
	ENTER("GetDeltaFormula",TRUE);
	EXIT("GetDeltaFormula");
	return pcDeltaForm->pfForm->pcArgs;
}

/* GetBFISpec

Description:
	Get the interval specification from the arguments of a bounded formula.
Scheme:

(define (get-bf-ispec bf-form)
  (second bf-form))
*/

CELLP GetBFISpec
(
	CELLP pcBFFormula
)
{
	ENTER("GetBFISpec",TRUE);
	EXIT("GetBFISpec");
	return pcBFFormula->pfForm->pcArgs;
}

/* GetBFFormulas

Description:
	Get the formulas arguments of a bounded formula.
Scheme:

(define (get-bf-formulas bf-form)
  (rest (rest bf-form)))
*/

CELLP GetBFFormulas
(
	CELLP pcBFFormula
)
{
	ENTER("GetBFFormulas",TRUE);
	EXIT("GetBFFormulas");
	return pcBFFormula->pfForm->pcArgs->pcNext;
}

/* FnLitQ

Description:
	Test if formula is a function literal.
Scheme:

(define (fn-lit? lit)
  (eq? (first lit) '=))
*/

BOOL FnLitQ
(
	CELLP pcLit							/* literal */
)
{
	BOOL b;

	ENTER("FnLitQ",FALSE);
	b=StringEqQ(IdentName(pcLit->pfForm),apsStringTab[STRING_EQ]);
	EXIT("FnLitQ");
	return b;
}

/* File I/O ----------------------------------------------------------------- */

/* EvalRedirect

Description:
	Redirect a file assignment.
*/

BOOL EvalRedirect
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc1,pc2;
	int nFile1,nFile2;					/* file index */
	int i;

	pc1=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc1,plpLinearPlan,pbBindings,&nFile1))
		return FALSE;
	if(nFile1<0||nFile1>MAXFILE-1)
	{
		ErrorMessage("redirect:  Destination file handle '%d' is out of range [%d,%d]\n",
			nFile1,0,MAXFILE-1);
		return FALSE;
	}
	pc2=pc1->pcNext;
	if(!FormulaToInteger(pc2,plpLinearPlan,pbBindings,&nFile2))
		return FALSE;
	if(nFile2<0||nFile2>MAXFILE-1)
	{
		ErrorMessage("redirect:  Source file handle '%d' is out of range [%d,%d]\n",
			nFile2,0,MAXFILE-1);
		return FALSE;
	}
	if(!aftFileTable[nFile2].pfFile)
	{
		ErrorMessage("redirect:  Source file handle '%d' is not open\n",nFile2);
		return FALSE;
	}
	if(aftFileTable[nFile1].pfFile)
	{
		if(((aftFileTable[nFile1].cStatus=='w'||aftFileTable[nFile1].cStatus=='a')&&
			(aftFileTable[nFile2].cStatus!='w'&&aftFileTable[nFile2].cStatus!='a'))||
			(aftFileTable[nFile1].cStatus=='r'&&aftFileTable[nFile2].cStatus!='r'))
		{
			ErrorMessage("redirect:  Source and destination file handles are incompatible\n");
			return FALSE;
		}

		// close the destination file unless it's stdio or it's open somewhere else

		if(aftFileTable[nFile1].pfFile&&!aftFileTable[nFile1].bStdio)
		{
			for(i=0;i<MAXFILE;i++)
				if(i!=nFile1&&aftFileTable[i].pfFile==aftFileTable[nFile1].pfFile)
					break;
			if(i<MAXFILE)
				fclose(aftFileTable[nFile1].pfFile);
		}
	}

	/* copy the file info from source to destination */
	
	aftFileTable[nFile1]=aftFileTable[nFile2];
	pfError=pfTraceStream=aftFileTable[STDLOG].pfFile;
	return TRUE;
}

/* EvalCloseFile

Description:
	Close a file
*/

BOOL EvalCloseFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int nFile;							/* file index */
	int i;


	if(!FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&nFile))
		return FALSE;
	if(nFile<0||nFile>MAXFILE-1)
	{
		ErrorMessage("close-file:  File handle out of range [%d,%d]\n",
			0,MAXFILE-1);
		return FALSE;
	}


	// If the file has an alias, we simply mark it closed.
	// Otherwise, we close the file, as long as it isn't stdio.

	for(i=0;i<MAXFILE;i++) {
	  if(i!=nFile&&aftFileTable[i].pfFile==aftFileTable[nFile].pfFile)
	    aftFileTable[i].pfFile=NULL;
	}
	
	if (i==MAXFILE) {
	  if(!aftFileTable[nFile].bStdio) {
	    fclose(aftFileTable[nFile].pfFile);
	  }
	  aftFileTable[nFile].pfFile=NULL;
	}

	return TRUE;
}

/* CloseAllFiles

Description:
	Close all files (we're exiting).
*/

void CloseAllFiles(void)
{
	int nFile;							/* file index */

	for(nFile=MINFILE+1;nFile<MAXFILE;nFile++)
	{
		if(aftFileTable[nFile].pfFile)
		{
			fclose(aftFileTable[nFile].pfFile);
			aftFileTable[nFile].pfFile=NULL;
		}
	}
	fclose(pfJBCRC);
}

/* InitFileHandles

Description:
	Fill in the first 4 file handles.
*/

void InitFileHandles(void)
{
	aftFileTable[STDOUT].pfFile=stdout;
	aftFileTable[STDOUT].cStatus='w';
	aftFileTable[STDOUT].bStdio=TRUE;
	aftFileTable[STDIN].pfFile=stdin;
	aftFileTable[STDIN].cStatus='r';
	aftFileTable[STDIN].bStdio=TRUE;
	aftFileTable[STDERR].pfFile=stderr;
	aftFileTable[STDERR].cStatus='w';
	aftFileTable[STDERR].bStdio=TRUE;
	pfError=pfTraceStream=aftFileTable[STDLOG].pfFile=fopen(slogFile,"w");
	pfJBCRC=fopen("crc32.txt","w");
	if(aftFileTable[STDLOG].pfFile)
	{
		aftFileTable[STDLOG].cStatus='w';
		aftFileTable[STDLOG].bStdio=TRUE;
	}
	else
	  ErrorMessage("Failed to open log file: %s.\n",slogFile);


	CommandPrintf(aftFileTable[STDOUT].pfFile,
		      "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
		      "+  Welcome to TLPlan (version April 3rd 2006 0.1).                                 +\n"
		      "+  Copyright (c) 1997, 2003, F. Bacchus & Michael Ady                              +\n"
                      "+  This copy can be used only for non-commercial research purposes.                +\n"
		      "+  You may not reverse engineer, decompile, translate or disassemble the Software. +\n"
		      "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");
	
	
	if(aftFileTable[STDLOG].pfFile)
	  CommandPrintf(aftFileTable[STDLOG].pfFile,
			"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
			"+  Welcome to TLPlan (modified version).                                           +\n"
			"+  Copyright (c) 1997, 2003, F. Bacchus & Michael Ady                              +\n"
			"+  This copy can be used only for non-commercial research purposes.                +\n"
			"+  You may not reverse engineer, decompile, translate or disassemble the Software. +\n"
			"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");
}


/* HandleToFileStream

Description:
	Convert an integer file handle to a file stream.
*/

DECLSPEC FILE *HandleToFileStream
(
	int nFile							/* file handle */
)
{
	if(nFile<0||nFile>MAXFILE-1)
		return NULL;
	return aftFileTable[nFile].pfFile;
}

/* EvalSetSearchDepthLimit

Description:
	Set the search depth limit.
*/

BOOL EvalSetSearchDepthLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int n;

	if(!FormulaToInteger(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&n))
		return FALSE;
	if(n<=0)
	{
		ErrorMessage("set-search-depth-limit:  Search depth must be positive.\n");
		return FALSE;
	}
	nSearchDepthLimit=n;
	return TRUE;
}

/* EvalResetSearchDepthLimit

Description:
	Disable search depth limiting.
*/

BOOL EvalResetSearchDepthLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	nSearchDepthLimit=0;
	return TRUE;
}

/* EvalSetSearchHeuristicLimit

Description:
	Set the search heuristic limit.
*/

BOOL EvalSetSearchHeuristicLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	double df;

	if(!FormulaToDouble(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&df))
		return FALSE;
	if(df<=0.0)
	{
		ErrorMessage("Search heuristic limit must be positive.\n");
		return FALSE;
	}
	dfSearchHeuristicLimit=df;
	return TRUE;
}

/* EvalResetSearchHeuristicLimit

Description:
	Disable the search heuristic limit.
*/

BOOL EvalResetSearchHeuristicLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	dfSearchHeuristicLimit=0.0;
	return TRUE;
}

