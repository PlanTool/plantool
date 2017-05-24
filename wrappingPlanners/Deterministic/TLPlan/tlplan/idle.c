/* idle.c

Copyright C, 1996 - 2001  F. Bacchus

Idle functions for the Temporal Progressor.
Evaluate functions on worlds which are idle.

IdleFormula is just overhead... we can call the idle functions directly.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tlplan.h"
#include "btree.h"
#include "eval.h"
#include "formula.h"
#include "idle.h"
#include "interval.h"
#include "makegen.h"
#include "progress.h"
#include "util.h"
#include "world.h"

/* local structures and definitions */

/* local function prototypes */

/* local data */

/* Run Time Routines ----------------------------------------------------------- */

/* Temporally extended goals. Test to see if a formula is true in
 an sequence consisting of infinitely replicating a single world.
 For timed modalities things will be true for all time.
*/

/* IdleFormula

Description:
	Test a formula on a world that idles (i.e., is true for all future times.
Scheme:

(define (idle-formula form world/action bindings)
  (if (or (eq? form #t) (eq? form #f))
	  form
	(let ((dispatch-fn (lookup-idle-eval (get-operator form))))
	  (if dispatch-fn
		  (dispatch-fn form world/action bindings)
		(eval-atomic (get-operator form)
					 (eval-terms (get-args form) world/action bindings)
					 world/action)))))
*/

BOOL IdleFormula
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleFormula",TRUE);
	b=(*pcFormula->pfForm->paAction->pfIdle)(pcFormula,plpLinearPlan,pbBindings);
	EXIT("IdleFormula");
	return b;
}

/* IdleAnd

Description:
	Test if an AND formula is true on an idling world.
Scheme:

(define (idle-and form world/action bindings)
  (every (lambda (formula) (idle-formula formula world/action bindings))
	 (get-args form)))
*/

BOOL IdleAnd
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("IdleAnd",TRUE);
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if(!IdleFormula(pc,plpLinearPlan,pbBindings))
		{
			EXIT("IdleAnd");
			return FALSE;
		}
	}
	EXIT("IdleAnd");
	return TRUE;
}

/* IdleOr

Description:
	Test if an OR formula is true on an idling world.
Scheme:

(define (idle-or form world/action bindings)
  (some (lambda (formula) (idle-formula formula world/action bindings))
		(get-args form)))
*/

BOOL IdleOr
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;

	ENTER("IdleOr",TRUE);
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if(IdleFormula(pc,plpLinearPlan,pbBindings))
		{
			EXIT("IdleOr");
			return TRUE;
		}
	}
	EXIT("IdleOr");
	return FALSE;
}

/* IdleXor

Description:
	Evaluate an XOR formula on an idling world.
Scheme:

(define (idle-xor form world/action bindings)
  (do ((found-one? #f)
	   (args (get-args form))
	   (false? #f)
	   (arg '()))
	  ((null? args) (and found-one? (not false?)))
	(set! arg (first args))
	(set! args (rest args))
	(if (idle-formula arg world/action bindings)
	(if found-one? (set! false? #t) (set! found-one? #t)))))
*/

BOOL IdleXor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL bFoundOne;
	CELLP pc;

	ENTER("IdleXor",TRUE);
	bFoundOne=FALSE;
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if(IdleFormula(pc,plpLinearPlan,pbBindings))
		{
			if(bFoundOne)
			{
				EXIT("IdleXor");
				return FALSE;
			}
			else
				bFoundOne=TRUE;
		}
	}
	EXIT("IdleXor");
	return bFoundOne;
}

/* IdleImplies

Description:
	Evaluate an IMPLIES formula on an idling world.
Scheme:

(define (idle-implies form world/action bindings)
  (if (idle-formula (first (get-args form)) world/action bindings)
	  (idle-formula (second (get-args form)) world/action bindings)
	  #t))
*/

BOOL IdleImplies
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("IdleImplies",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(IdleFormula(pc,plpLinearPlan,pbBindings))
		b=IdleFormula(pc->pcNext,plpLinearPlan,pbBindings);
	else
		b=TRUE;
	EXIT("IdleImplies");
	return b;
}

/* IdleIfThenElse

Description:
	Evaluate an IFTHENELSE formula on an idling world.
Scheme:

(define (idle-if-then-else form world/action bindings)
  (let ((args (get-args form)))
	(if (idle-formula (first args) world/action bindings)
		(idle-formula (second args) world/action bindings)
	(idle-formula (third args) world/action bindings))))
*/

BOOL IdleIfThenElse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pc;

	ENTER("IdleIfThenElse",TRUE);
	pc=pcFormula->pfForm->pcArgs;
	if(IdleFormula(pc,plpLinearPlan,pbBindings))
		b=IdleFormula(pc->pcNext,plpLinearPlan,pbBindings);
	else
		b=IdleFormula(pc->pcNext->pcNext,plpLinearPlan,pbBindings);
	EXIT("IdleIfThenElse");
	return b;
}

/* IdleNot

Description:
	Evaluate a NOT formula on an idling world.
Scheme:

(define (idle-not form world/action bindings)
  (not (idle-formula (first (get-args form)) world/action bindings)))
*/

BOOL IdleNot
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleNot",TRUE);
	b=!IdleFormula(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	EXIT("IdleNot");
	return b;
}

/* Temporal Formulas ----------------------------------------------------------- */

/* IdleIgnoreOp

Description:
	Idle by ignoring the operator.
	Works for next, eventually, always.
Scheme:

(define (idle-ignore-op form world/action bindings)
  (idle-formula (first (get-args form)) world/action bindings))
*/

BOOL IdleIgnoreOp
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleIgnoreOp",TRUE);
	b=IdleFormula(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
	EXIT("IdleIgnoreOp");
	return b;
}

/* IdleUntil

Description:
	idle an UNTIL modality.
Scheme:

(define (idle-until form world/action bindings)
  (idle-formula (second (get-args form)) world/action bindings))
*/

BOOL IdleUntil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleUntil",TRUE);
	b=IdleFormula(pcFormula->pfForm->pcArgs->pcNext,plpLinearPlan,pbBindings);
	EXIT("IdleUntil");
	return b;
}

/*  MTL Timed Temporal Formulas ------------------------------------------------ */

/* IdleTAlways

Description:
	Idle a timed ALWAYS modality.
Scheme:

(define (idle-t-always form world/action bindings)
  (if (ispec<0 (get-bf-ispec form))
	  #t                                ;timed out
	  (idle-formula
	   (first (get-bf-formulas form)) world/action bindings)))
*/

BOOL IdleTAlways
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleTAlways",TRUE);
	if(ISpecLt0(GetBFISpec(pcFormula)->pfForm->uValue.piISpec))
		b=TRUE;					/* timed out */
	else
		b=IdleFormula(GetBFFormulas(pcFormula),plpLinearPlan,pbBindings);
	EXIT("IdleTAlways");
	return b;
}

/* IdleDelta

Description:
	Idle a delta formula.
	Build a shifted formula and then idle that."
Scheme:

(define (idle-delta form world/action bindings)
	(let* ((formula (get-delta-formula form))
			(ispec (get-bf-ispec formula)))
		(idle-formula
			(append
				(list (get-operator formula)
					(ispec-lshift ispec
						(world/action-action-duration world/action)))
				(get-bf-formulas formula))
			world/action bindings)))
*/

BOOL IdleDelta
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;
	CELLP pcISpec;						/* interval to shift */
	CELLP pcTail;						/* trailing arguments */
	CELLP pcShiftedForm;				/* shifted formula */

	ENTER("IdleDelta",TRUE);

	/* copy the formula */

	pcShiftedForm=CopyCell(pcFormula->pfForm->pcArgs);
	pcShiftedForm->pfForm=CopyAlloc(pcShiftedForm->pfForm,sizeof(FORMULA));
	pcISpec=pcShiftedForm->pfForm->pcArgs;
	pcTail=pcISpec->pcNext;

	/* shift the ispec and reassemble */
	
	pcISpec=LShiftForm(pcISpec,LinearPlanActionDuration(plpLinearPlan));
	pcISpec->pcNext=pcTail;
	pcShiftedForm->pfForm->pcArgs=pcISpec;

	/* idle the result */
	
	b=IdleFormula(pcShiftedForm,plpLinearPlan,pbBindings);
	EXIT("IdleDelta");
	return b;
}

//	BOOL b;
//	ISPECP piISpec;
//	CELLP pcForm;						/* formula to shift */
//	CELLP pcShiftedForm;				/* shifted formula */
//
//	ENTER("IdleDelta",TRUE);
//	pcForm=GetDeltaFormula(pcFormula);
//	piISpec=GetBFISpec(pcForm)->pfForm->uValue.piISpec;
//	pcShiftedForm=(CELLP)MemAlloc(sizeof(CELL));
//	pcShiftedForm->pfForm=pcForm->pfForm;
//	pcShiftedForm->pfForm->pcArgs->pfForm->uValue.piISpec=
//		ISpecLShift(piISpec,LinearPlanActionDuration(plpLinearPlan));
//	b=IdleFormula(pcShiftedForm,plpLinearPlan,pbBindings);
//	EXIT("IdleDelta");
//	return b;

/* IdleTEventually

Description:
	"idle a timed EVENTUALLY modality."
Scheme:

(define (idle-t-eventually form world/action bindings)
  (if (ispec<0  (get-bf-ispec form))
	  #f                               ;timed out
	  (idle-formula
	   (first (get-bf-formulas form)) world/action bindings)))
*/

BOOL IdleTEventually
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleTEventually",TRUE);
	if(ISpecLt0(GetBFISpec(pcFormula)->pfForm->uValue.piISpec))
		b=FALSE;					/* timed out */
	else
		b=IdleFormula(GetBFFormulas(pcFormula),plpLinearPlan,pbBindings);
	EXIT("IdleTEventually");
	return b;
}

/* IdleTUntil

Description:
	"idle a timed UNTIL modality. (Can be timedOut.)"
Scheme:

(define (idle-t-until form world/action bindings)
  (if (ispec<0 (get-bf-ispec form))
	  #f                               ;timed out
	  (idle-formula
	   (second (get-bf-formulas form)) world/action bindings)))
*/

BOOL IdleTUntil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleTUntil",TRUE);
	if(ISpecLt0(GetBFISpec(pcFormula)->pfForm->uValue.piISpec))
		b=FALSE;					/* timed out */
	else
		b=IdleFormula(GetBFFormulas(pcFormula)->pcNext,plpLinearPlan,pbBindings);
	EXIT("IdleTUntil");
	return b;
}

/* Quantified Formulas --------------------------------------------------------- */

/* IdleForAll

Description:
	Idle a FORALL formula.
Scheme:

(define (idle-forall form world/action bindings)
	(let*
		((args (get-args form))
			(variables (get-qf-variables args))
			(gen-lit (get-qf-generator args))
			(formula (get-qf-formula args))
			(generator '()))
		(set! bindings (extend-pbBindings variables variables bindings))
		(set! generator (make-generator gen-lit variables world/action bindings))
		(if generator
			(do ((finish? #f) (result #f))
				(finish? result)
				(cond(
					(generator)
						(if (not (idle-formula formula world/action bindings))
							(begin (set! finish? #t)
							   (set! result #f))))
					(else (set! finish? #t)
						(set! result #t))))
			#t)))
*/

BOOL IdleForAll
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcQfFormula;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index

	ENTER("IdleForAll",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcQfFormula=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);
	pvContext=NULL;
	nBaseSP=GetBTreeSP();
	
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!IdleFormula(pcQfFormula,plpLinearPlan,pbBindings))
		{
			SetBTreeSP(nBaseSP);
			EXIT("IdleForAll");
			return FALSE;
		}
	}
	EXIT("IdleForAll");
	return TRUE;
}

/* IdleExists

Description:
	idle an EXISTS formula.
Scheme:

(define (idle-exists form world/action bindings)
  (let* ((args (get-args form))
		 (variables (get-qf-variables args))
		 (gen-lit   (get-qf-generator args))
		 (formula   (get-qf-formula args))
		 (generator '()))
	(set! bindings (extend-pbBindings variables variables bindings))
	(set! generator
	  (make-generator gen-lit variables world/action bindings))
	(if generator
		(do ((finish? #f)
			 (result #f))
			(finish? result)
		  (cond((generator)
		(if (or (null? formula)
			(idle-formula formula world/action bindings))
			(begin (set! finish? #t)
			   (set! result #t))))
		   (else (set! finish? #t)
			 (set! result #f))))
	#f)))
*/

BOOL IdleExists
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcQfFormula;
	CELLP pcGenLit;
	void *pvContext;
	int nBaseSP;						// saved base btree stack index

	ENTER("IdleExists",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcQfFormula=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!pcQfFormula||IdleFormula(pcQfFormula,plpLinearPlan,pbBindings))
		{
			SetBTreeSP(nBaseSP);
			EXIT("IdleExists");
			return TRUE;
		}
	}
	EXIT("IdleExists");
	return FALSE;
}

/* IdleExistsX

Description:
	idle an EXISTS! (exists unique) formula.
Scheme:

(define (idle-exists! form world/action bindings)
	(let* ((args (get-args form))
			(variables (get-qf-variables args))
			(gen-lit (get-qf-generator args))
			(formula (get-qf-formula args))
			(generator '()))
		(set! bindings (extend-pbBindings variables variables bindings))
		(set! generator (make-generator gen-lit variables world/action bindings))
		(if generator
			(do ((finish? #f) (result #f) (found-one? #f))
				(finish? result)
				(if (generator)
					(if (or (null? formula)
							(idle-formula formula world/action bindings))
						(cond
							(found-one?
								(set! finish? #t)
								(set! result #f))
							(else (set! found-one? #t))))
						(cond
							(found-one?
								(set! finish? #t)
								(set! result #t))
							(else (set! finish? #t)
								(set! result #f)))))
		#f)))
*/

BOOL IdleExistsX
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcQfFormula;
	CELLP pcGenLit;
	void *pvContext;
	BOOL bFoundOne;
	int nBaseSP;						// saved base btree stack index

	ENTER("IdleExistsX",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcQfFormula=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	bFoundOne=FALSE;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!pcQfFormula||IdleFormula(pcQfFormula,plpLinearPlan,pbBindings))
		{
			if(bFoundOne)
			{
				SetBTreeSP(nBaseSP);
				EXIT("IdleExistsX");
				return FALSE;
			}
			bFoundOne=TRUE;
		}
	}
	EXIT("IdleExistsX");
	return bFoundOne;
}

/* Auxiliary Formulas ---------------------------------------------------------- */

/* IdleBinding

Description:
	idle a binding node, by extending the current pbBindings.
Scheme:

(define (idle-binding form world/action bindings)
  (idle-formula
   (get-binding-formula form)
   world/action
   (extend-pbBindings (get-binding-vars form)
					(get-binding-vals form)
					pbBindings)))
*/

BOOL IdleBinding
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BOOL b;

	ENTER("IdleBinding",TRUE);
	b=IdleFormula(GetBindingFormula(pcFormula),plpLinearPlan,
		ExtendBindings(GetBindingVars(pcFormula),GetBindingVals(pcFormula),pbBindings));
	EXIT("IdleBinding");
	return b;
}



