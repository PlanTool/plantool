/* adl.c

Copyright C, 1996 - 2001,  F. Bacchus

This module is a component of the tlplan parser (compiler).
Each time the parser sees an outermost def-adl-operator list,
iDefADLOperator is called with a command list.
iDefADLOperator converts this list into the internal representation of an operator.

ADL Operators ------------------------------------------------------------------

Implementation of ADL Operators.
Like STRIPS the ADL operators are implemented using the first order evaluator.

ADL actions contain conditions for which all matching bindings must be found.
Each matching binding generates a ground predicate that is added or deleted.

ADL operators are stored in the standard operator structure.

Example:

(def-adl-operator (puton ?x ?y)
	(pre
		(?x) (clear ?x)
		(?y) (clear ?y)
		(not (= ?x ?y)))
	(forall
		(?z) (on ?x ?z)
		(and
			(add (clear ?z))
			(del (on ?x ?z))))
	(add
		(on ?x ?y)
		(last-moved ?x))
	(del
		(clear ?y))
	(implies
		(ontable ?x)
		(del (ontable ?x)))
	(forall
		(?z) (last-moved ?z)
		(del (last-moved ?z))))

*/

#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "compute.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "makeform.h"
#include "oper.h"
#include "plan.h"
#include "search.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "var.h"
#include "world.h"
#include "zone.h"

/* global data */

LINEARPLANP plpSuccessors;				/* successor states for an operator */
LINEARPLANP plpLastSuccessor;			/* pointer to end of successor list */
LINEARPLANP plpSuccessorWorld;			/* current successor state */

//double dfSuccessorTime;

/* local structures and definitions */

/* local data */


/* local function prototypes */

static BOOL ADLParse
(
	CELLP pcFormula,					/* input formula */
	CELLP *ppcName,						/* output name */
	CELLP *ppcPre,						/* output preconditions */
	CELLP *ppcMod,						/* output modifier list */
	CELLP *ppcDuration,					/* output delta time */
	CELLP *ppcCost,						/* output delta cost */
	CELLP *ppcPriority					/* output priority */
);

static CELLP CompileSuccessor
(
	CELLP pcVarGens,					/* variable generator pair(s) */
	CELLP pcAntecedent,					/* antecedent (required) */
	CELLP pcConsequent					/* consequent (optional) */
);
static CELLP CompileOtherFormula
(
	CELLP pcVarGens,
	CELLP pcAntecedent,
	CELLP pcConsequent
);
static CELLP CompileAndFormula
(
	CELLP pcVarGens,
	CELLP pcAntecedent,
	CELLP pcConsequent
);
static CELLP CompileAndRecurse
(
	CELLP pcForAllForms,
	CELLP pcConsequent
);
static CELLP MakeOperatorForm
(
	OPERATORP poOperator				/* operator */
);
static CELLP FindFreeVars
(
	CELLP pcFormula,
	CELLP pcBVars,						/* bound variables (initialize to NULL) */
	CELLP pcFVars						/* free variables (initialize to NULL) */
);

/* EvalDefADLOperator

Description:
	Compile an ADL operator into a formula that can be applied to a
	world, then place the operator structure on the list of operators."
Scheme:

(define (def-adl-operator . keyword-args)
	(let (
			(op '())
			(modifier-formulas '())
			(update-form '())
			(new-world '())
			(call-fn '())
			(name-op '())
			(name-args '())
			(successors '())
			(name '())
			(pre '(()))
			(add '())
			(del '())
			(duration 1)
			(cost 1)
			(priority 0)
			(update '()))
		(do ()
			((null? keyword-args))
			(case (first keyword-args)
				(:name
					(set! name (cadr keyword-args)))
				(:pre
					(set! pre (cadr keyword-args)))
				(:add
					(set! add (cadr keyword-args)))
				(:del
					(set! del (cadr keyword-args)))
				(:duration
					(set! duration (cadr keyword-args)))
				(:cost
					(set! cost (cadr keyword-args)))
				(:priority
					(set! priority (cadr keyword-args)))
				(:update
					(set! update (cadr keyword-args)))
				(else
					(error "Illegal keyword: ~A" (first keyword-args))))
			(set! keyword-args (cddr keyword-args)))
		(set! op
			(make-operator :name name :pre pre :add add :del del
				:duration duration :update update :cost cost :priority priority))
		(set! name-op (get-operator name))
		(set! name-args (get-args name))
;;;
;;; First construct the modifier formula. This formula will be
;;; evaluated if the precondition formula succeeds.
;;; The modifier formula is an AND formula whose last conjunct is a
;;; CALL that pushes the new world onto the list of new worlds along
;;; with the world name.
;;;
		(set! call-fn
			(lambda (world/action bindings)
				(set! successors
					(cons (make-world/action
							new-world
							(cons name-op (eval-terms name-args
									world/action bindings))
							(eval-term duration world/action bindings)
							(eval-term cost world/action bindings)
							priority)
						successors))))
;;;     (format #t "~Pushing new world, computed via action")
		(set! modifier-formulas (cons `(CALL ,call-fn) modifier-formulas))
;;;
;;; The middle conjuncts are formulas that do conditional updates to
;;; the new world, depending on the type of condition
;;; (add/delete/update).
;;;
		(for-each
			(lambda (type)
				(let ((conditions (if (eq? type 'del) del add)))
					(for-each
						(lambda (condition)
							(let* ((mod-literal (adl-mod-literal condition))
									(mod-operator (get-operator mod-literal))
									(mod-args (get-args mod-literal))
									(update-formula '()))
								(set! call-fn
									(if (eq? type 'del)
										(lambda (world/action bindings)
											(delete-world-lit
												mod-operator
												(eval-terms mod-args world/action
													bindings)
												new-world))
										(lambda (world/action bindings)
											(add-world-lit
												mod-operator
												(eval-terms mod-args world/action
													bindings)
												new-world))))
								(set! update-formula
									(adl-compile-lambda
										(adl-mod-var-gens condition)
										(adl-mod-formula condition)
										`(CALL ,call-fn)))
								(set! modifier-formulas
									(cons update-formula modifier-formulas))))
						conditions)))
			'(add del))					;Do Deletes prior to adds!

		(for-each
			(lambda (conditions)
				(for-each
					(lambda (condition)
						(let* ((mod-literal (adl-mod-literal condition))
								(mod-operator (get-fn-name mod-literal))
								(mod-args (get-fn-args mod-literal))
								(mod-value (get-fn-value mod-literal))
								(update-formula '()))
							(set! call-fn
								(lambda (world/action bindings)
									(update-world-fn
										mod-operator
										(eval-terms mod-args world/action bindings)
										(eval-term mod-value world/action bindings)
										new-world)))
							(set! update-formula
								(adl-compile-lambda
									(adl-mod-var-gens condition)
									(adl-mod-formula condition)
									`(CALL ,call-fn)))
							(set! modifier-formulas
								(cons update-formula modifier-formulas))))
					conditions))
			update)
;;;
;;; Finally the first conjunct creates a new world. This world is only
;;; created if the preconditions succeed.
;;;
		(set! call-fn
			(lambda
				(world/action bindings)
				(set! new-world
					(copy-world (world/action-world world/action)))))
		(set! modifier-formulas
			(cons `(CALL ,call-fn) modifier-formulas))
		(set! modifier-formulas (cons 'AND modifier-formulas))
;;;
;;; The final formula to be evaluated is an implication that starts
;;; with an evaluation of the preconditions.
;;;
		(set! update-form
			(adl-compile-lambda
				(adl-pre-var-gens pre)
				(adl-pre-formula pre)
				modifier-formulas))
;;;
;;; Print out the formula for information.
;;;
		(if *verbose*
			(begin
				(format #t "~%ADL ~S operator compiled into formula:" name)
				(format #t  "~%~S~%~%" update-form)))
;;;
;;; The actual function that updates the world evaluates the update
;;; formula and then returns the list of updated worlds.
;;;
		(set-operator-eval-object! op
			(lambda (world/action)
				(set! successors '())
				(eval-formula update-form world/action '())
				successors))
		(add-op op)))
*/

BOOL EvalDefADLOperator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcSuccessor;					/* successor formula */
	CELLP pcName;						/* operator name */
	CELLP pcPre;						/* preconditions */
	CELLP pcMod;						/* modifier list */
	CELLP pcDuration;					/* delta time */
	CELLP pcCost;						/* delta cost */
	CELLP pcPriority;					/* priority */
	CELLP pcModifyWorld;				/* modify world formula */
	OPERATORP po;						/* compiled operator */
	char ac[128];						/* string buffer */

	/* break the command string into its components */

	if(!ADLParse(pcFormula,&pcName,&pcPre,&pcMod,&pcDuration,&pcCost,&pcPriority))
		return FALSE;					/* on error, return */

	/* convert modifiers to modify world formula */

	if(pcMod&&pcMod->pcNext)
		pcMod=MakeAndForm(FALSE,pcMod);
	pcModifyWorld=MakeModifyWorldForm(FALSE,NULL,pcMod);

	/* compile the successor formula */

	if(pcPre)
		pcSuccessor=CompileSuccessor(pcPre->pfForm->pcVars,pcPre->pfForm->pcArgs,pcModifyWorld);
	else
		pcSuccessor=pcModifyWorld;

	/* create the operator and thread it back into modify-world */

	po=MakeOperator(pcName,pcSuccessor,pcDuration,pcCost,pcPriority);
	pcModifyWorld->pfForm->pcArgs->pfForm->uValue.poOperator=po;

	/* Print out the formula for information. */

	if(bVerbose)
	{
		CommandPrintf(pfTraceStream,"\"%s\" operator successor formula:\n",
			GetName(pcName->pfForm,ac));
		PrintFormula(pfTraceStream,pcSuccessor,0);
		CommandPrintf(pfTraceStream,"\n");
	}
	return TRUE;
}

/* Parse Routines -------------------------------------------------------------- */

/* ADLParse

Description:
	Locate all of the parts of an ADL operator.
Notes:
	The modifier list is returned in reverse order.
*/

static BOOL ADLParse
(
	CELLP pcFormula,					/* input formula */
	CELLP *ppcName,						/* output name */
	CELLP *ppcPre,						/* output preconditions */
	CELLP *ppcMod,						/* output modifier list */
	CELLP *ppcDuration,					/* output delta time */
	CELLP *ppcCost,						/* output delta cost */
	CELLP *ppcPriority					/* output priority */
)
{
	CELLP pcName;						/* output name */
	CELLP pcPre;						/* output preconditions */
	CELLP pcMod;						/* output modifier list */
	CELLP pcDuration;					/* output delta time */
	CELLP pcCost;						/* output delta cost */
	CELLP pcPriority;					/* output priority */
	CELLP pc,pc1;
	char ac[40];						/* string buffer */

	/* set defaults */

	pcName=NULL;
	pcPre=NULL;
	pcMod=NULL;
	pcDuration=NULL;
	pcCost=NULL;
	pcPriority=NULL;

	/* get the name and argument list */

	pcName=pcFormula->pfForm->pcArgs;
	if(!pcName)
	{
		ErrorMessage("def-adl-operator:  No name or argument list specified.\n");
		return FALSE;
	}

	/* scan clause list */

	for(pc=pcName->pcNext;pc;pc=pc1)
	{
		pc1=pc->pcNext;
		pc->pcNext=NULL;
		if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_DURATION]))
		{
			if(pcDuration)
			{
				ErrorMessage("def-adl-operator:  %s has more than one duration clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcDuration=pc->pfForm->pcArgs;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_COST]))
		{
			if(pcCost)
			{
				ErrorMessage("def-adl-operator:  %s has more than one cost clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcCost=pc->pfForm->pcArgs;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_PRIORITY]))
		{
			if(pcPriority)
			{
				ErrorMessage("def-adl-operator:  %s has more than one priority clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcPriority=pc->pfForm->pcArgs;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_PRE]))
		{
			if(pcPre)
			{
				ErrorMessage("def-adl-operator:  %s has more than one preconditions clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcPre=pc;
		}
		else							/* must be modifier clause */
		{
			pc->pcNext=pcMod;			/* prefix to modifier list */
			pcMod=pc;
		}
	}
	*ppcMod=pcMod;
	*ppcPre=pcPre;
	*ppcDuration=pcDuration?pcDuration:MakeFloatForm(bConcurrentPlanning?0.0:1.0);
	*ppcCost=pcCost?pcCost:MakeFloatForm(1.0);
	*ppcPriority=pcPriority?pcPriority:MakeFloatForm(0.0);
	*ppcName=pcName;
	return TRUE;
}

/* ModifyWorld Formula Support ------------------------------------------------- */

/* MakeModifyWorldForm

Description:
	Make a modify-world formula.
*/

CELLP MakeModifyWorldForm
(
	BOOL bCopy,							/* copy flag */
	OPERATORP poOperator,				/* operator */
	CELLP pcMod							/* modifier formula */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeModifyWorldForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->psName=IdentAlloc("modify-world");
	pf->paAction=&aModifyWorldAction;
	pf->pcArgs=MakeOperatorForm(poOperator);
	if(bCopy)
		pf->pcArgs->pcNext=CopyCell(pcMod);
	else
	{
		pf->pcArgs->pcNext=pcMod;
		if(pcMod)
			pf->pcArgs->pcNext->pcNext=NULL;
	}
	EXIT("MakeModifyWorldForm");
	return pc;
}

/* MakeOperatorForm

Description:
	Make an operator pointer formula.
*/

static CELLP MakeOperatorForm
(
	OPERATORP poOperator				/* operator */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("MakeOperatorForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_OPERATORP;
	pf->psName=IdentAlloc("operator");
	pf->uValue.poOperator=poOperator;
	pf->paAction=&aOperatorAction;
	EXIT("MakeOperatorForm");
	return pc;
}

/* EvalModifyWorld

Description:
	Copy the current world/action.
	Label it with the operator that created it and apply the appropriate
	modifications.
*/

BOOL EvalModifyWorld
(
	CELLP pcFormula,					/* modify world formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current variable bindings */
)
{
	BTREEP *apbtNewWorld;
	CELLP pcLabel;
	CELLP pcMod;
	double dfDuration;
	double dfCost;
	double dfPriority;
	OPERATORP po;						/* operator */

	ENTER("EvalModifyWorld",TRUE);

	// Special processing for action promises (depth-first search only)
	
	if(pfSearchStrategy==DepthFirst||pfSearchStrategy==DepthFirstPriority)
	{
		// Process previously generated action-promises

		if(!plpLinearPlan->apbtWorld)
		{
			// fetch our stashed bindings and modify formula
			
			pbBindings=(BINDINGP)plpLinearPlan->nSignature;
			pcMod=pcFormula->pfForm->pcArgs->pcNext;

			/* copy the parent world */

			apbtNewWorld=CopyWorld(LinearPlanWorld(plpLinearPlan->plpParent));
			plpLinearPlan->apbtWorld=apbtNewWorld;

			/* create a label for the world */

			if(nTrace<3)				// if we haven't already done this
			{
				po=pcFormula->pfForm->pcArgs->pfForm->uValue.poOperator;
				pcLabel=CopyFormula(po->pcName);	/* this is rather inefficient! */
				if(po->pcName->pfForm->pcArgs)
				{
					pcLabel->pfForm->pcArgs=ComputeTerms(po->pcName->pfForm->pcArgs,plpLinearPlan,pbBindings);
					if(!pcLabel->pfForm->pcArgs)
						TermError("eval-modify-world",pcFormula,pbBindings);
				}
				plpLinearPlan->pcActionName=pcLabel;
			}

			/* handle modifications */

			plpSuccessorWorld=plpLinearPlan;
			pcMod=pcFormula->pfForm->pcArgs->pcNext;
			if(pcMod)
				(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan->plpParent,pbBindings);
			UpdateWorld();					/* apply adds and dels */
			WorldSignature(plpLinearPlan);
			plpSuccessorWorld=NULL;

			EXIT("EvalModifyWorld");
			return TRUE;
		}
		
		// compute the numeric parameters

		po=pcFormula->pfForm->pcArgs->pfForm->uValue.poOperator;
		FormulaToDouble(po->pcDuration,plpLinearPlan,pbBindings,&dfDuration);
		FormulaToDouble(po->pcCost,plpLinearPlan,pbBindings,&dfCost);
		FormulaToDouble(po->pcPriority,plpLinearPlan,pbBindings,&dfPriority);

		/* link this world to the end of the successor list */

		plpSuccessorWorld=MakeWorldAction(NULL,NULL,dfDuration,dfCost,dfPriority);
		if(bConcurrentPlanning)			/* pass parent's time */
			plpSuccessorWorld->dfTime=plpLinearPlan->dfTime;
		plpSuccessorWorld->pcPromise=pcFormula;	// save our modify formula
		assert(plpLastSuccessor);		/* this should be pointing at our succesor list */
		plpSuccessorWorld->nSignature=(unsigned int)CopyBindingsShared((BINDINGP)(
			(plpLastSuccessor==(LINEARPLANP)&plpSuccessors)?NULL:plpLastSuccessor->nSignature),pbBindings);
		plpLastSuccessor=plpLastSuccessor->plpNext=plpSuccessorWorld;
		plpLastSuccessor->plpNext=NULL;

		/* create a label for the world */

		if(nTrace>=3)					// give the successor worlds labels for the trace
		{
			pcLabel=CopyFormula(po->pcName);	/* this is rather inefficient! */
			if(po->pcName->pfForm->pcArgs)
			{
				pcLabel->pfForm->pcArgs=ComputeTerms(po->pcName->pfForm->pcArgs,plpLinearPlan,pbBindings);
				if(!pcLabel->pfForm->pcArgs)
					TermError("eval-modify-world",pcFormula,pbBindings);
			}
			plpSuccessorWorld->pcActionName=pcLabel;
		}

		EXIT("EvalModifyWorld");
		return TRUE;
	}

	/* create a label for the new world */

	po=pcFormula->pfForm->pcArgs->pfForm->uValue.poOperator;
	pcLabel=CopyFormula(po->pcName);	/* this is rather inefficient! */
	if(po->pcName->pfForm->pcArgs)
	{
		pcLabel->pfForm->pcArgs=ComputeTerms(po->pcName->pfForm->pcArgs,plpLinearPlan,pbBindings);
		if(!pcLabel->pfForm->pcArgs)
			TermError("eval-modify-world",pcFormula,pbBindings);
	}

	// compute the numeric parameters

	FormulaToDouble(po->pcDuration,plpLinearPlan,pbBindings,&dfDuration);
	FormulaToDouble(po->pcCost,plpLinearPlan,pbBindings,&dfCost);
	FormulaToDouble(po->pcPriority,plpLinearPlan,pbBindings,&dfPriority);

	/* copy the current world */

	apbtNewWorld=CopyWorld(LinearPlanWorld(plpLinearPlan));
	plpSuccessorWorld=MakeWorldAction(apbtNewWorld,pcLabel,dfDuration,dfCost,dfPriority);
	if(bConcurrentPlanning)				/* pass parent's time */
		plpSuccessorWorld->dfTime=plpLinearPlan->dfTime;

	/* handle modifications */

	pcMod=pcFormula->pfForm->pcArgs->pcNext;
	if(pcMod)
		(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
	UpdateWorld();						/* apply adds and dels */

	// special handling for depth-first-no-backtracking search

	if(pfSearchStrategy==DepthFirstNoBacktracking)
	{
		CELLP pcTLForm;
		CELLP pcProgressedTLForm;
		int nImmutable;

		plpSuccessorWorld->plpParent=plpLinearPlan;	// need this for cycle checking
		pcProgressedTLForm=NULL;
		if(!bCycleChecking||CycleFilter(plpSuccessorWorld))
		{	
			srSearchResult.nWorldsSearched++;
			StartTimer(tsTimers.adfProgress);
			pcTLForm=LinearPlanTLForm(plpLinearPlan);
			SetZone(&zScratch);
			pcProgressedTLForm=(*pcTLForm->pfForm->paAction->pfProgress)
				(pcTLForm,plpSuccessorWorld,pbBindings,&nImmutable);
			SetZone(&zPermanent);
			ZoneCopyFormula(pcProgressedTLForm);
			ZoneReloc((void **)&pcProgressedTLForm);
			ZoneRelocFormula(pcProgressedTLForm);
			ZoneClear(&zScratch);
			StopTimer(tsTimers.adfProgress);
			if(FalseFormQ(pcProgressedTLForm))
			{
				plpSuccessorWorld=NULL;	// prune bad world
				srSearchResult.nWorldsPruned++;
			}
		}
		else
		{
			plpSuccessorWorld=NULL;		// discard cyclic world
			srSearchResult.nWorldsDiscarded++;
		}
		if(!plpSuccessorWorld)
		{
			EXIT("EvalModifyWorld");
			return TRUE;				// keep looking for a viable world
		}
		
		/* link this world to the end of the successor list */

		plpSuccessorWorld->pcTLForm=pcProgressedTLForm;

		plpLastSuccessor=plpLastSuccessor->plpNext=plpSuccessorWorld;
		plpLastSuccessor->plpNext=NULL;

		EXIT("EvalModifyWorld");
		return FALSE;					// we have a viable world, terminate searching
	}
	
	/* link this world to the end of the successor list */

	plpLastSuccessor=plpLastSuccessor->plpNext=plpSuccessorWorld;
	plpLastSuccessor->plpNext=NULL;
	
	EXIT("EvalModifyWorld");
	return TRUE;
}

/* ProgressModifyWorld

Description:
	Evaluate a modify-world for side effects during formula progression.
	Always returns TRUE!
Notes:
	The code here would be identical to EvalModifyWorld,
	except that we need to return a formula, not a boolean.
*/

CELLP ProgressModifyWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
)
{
	CELLP pc;

	ENTER("ProgressModifyWorld",TRUE);
	EvalModifyWorld(pcFormula,plpLinearPlan,pbBindings);
	pc=MakeTrueForm();
	EXIT("ProgressModifyWorld");
	return pc;
}

/* CurrentModifyWorld

Description:
	Evaluate a modify-world for side effects during formula Currention.
	Always returns TRUE!
*/

int CurrentModifyWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("CurrentModifyWorld",TRUE);
	EvalModifyWorld(pcFormula,plpLinearPlan,pbBindings);
	EXIT("CurrentModifyWorld");
	return TRUE;
}

/* GenerateSuccessors

Description:
	Return successors of a world applying all applicable actions.
Scheme:

(define (generate-successors world/action)
	(do ((successors '())
		(operators *operators*)
		(op '()))
	((null? operators) successors)
	(set! op (first operators))
	(set! operators (rest operators))
	(set! successors
		(append! (apply-operator op world/action) successors))))
*/

LINEARPLANP GenerateSuccessors
(
	LINEARPLANP plpStart,				/* initial plan */
	BINDINGP pbBindings					/* bindings */
)
{
	LINEARPLANP plpFirst,plpLast;		/* successor list head */
	OPERATORP po;						/* operator pointer */

	ENTER("GenerateSuccessors",TRUE);
	plpFirst=NULL;						/* initialize list head */
	plpLast=(LINEARPLANP)&plpFirst;

	/* make a list, including each operator's successors */

	if(pfSearchStrategy==DepthFirstNoBacktracking)
	{
		for(po=poOperators;po;po=po->poNext)
		{
			plpSuccessors=NULL;			/* initialize the successor list */
			plpLastSuccessor=(LINEARPLANP)&plpSuccessors;
//dfSuccessorTime-=GetInternalRunTime();
			(*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,plpStart,pbBindings);
//dfSuccessorTime+=GetInternalRunTime();
			if(plpSuccessors)
			{
				EXIT("GenerateSuccessors");
				return plpSuccessors;
			}
		}
	}
	else
	{
		for(po=poOperators;po;po=po->poNext)
		{
			plpSuccessors=NULL;			/* initialize the successor list */
			plpLastSuccessor=(LINEARPLANP)&plpSuccessors;
			(*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,plpStart,pbBindings);
//			plpLast->plpNext=plpSuccessors;
//			while(plpLast->plpNext)		// advance pointer to end of list
//				plpLast=plpLast->plpNext;
			if(plpSuccessors)
			{
				plpLast->plpNext=plpSuccessors;
				plpLast=plpLastSuccessor;
			}
		}
	}
	EXIT("GenerateSuccessors");
	return plpFirst;
}

/* OperatorSuccessor

Description:
	This is the successor routine for operators.
*/

//LINEARPLANP OperatorSuccessor
//(
//	OPERATORP po,						/* operator */
//	LINEARPLANP plpLinearPlan,			/* plan */
//	BINDINGP pbBindings					/* bindings */
//)
//{
//	ENTER("OperatorSuccessor",TRUE);
//	plpSuccessors=NULL;					/* initialize the successor list */
//	plpLastSuccessor=(LINEARPLANP)&plpSuccessors;
//	(*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,plpLinearPlan,pbBindings);
//	EXIT("OperatorSuccessor");
//	return plpSuccessors;
//}

/* Formula compile routines ---------------------------------------------------- */

/* CompileSuccessor

Description:
	Convert an ADL "pre" formula which may contain multiple
	variable/generator pairs (var-gens) into a nested forall formula,
	where each forall has exactly one var-gen.
	The routine assumes it is passed an "implies" formula that has already
	been split into antecedent and consequent.  If that isn't the case,
	then the entire formula should be passed as the antecedent.
	Special optimizations are performed when we have an AND formula
	in the antecedent.
Scheme:

(define (adl-compile-lambda var-gens formula final-form)
	(cond
		((and formula (or (and-formp formula) (atomic-formp formula)))
			(adl-compile-and-lambda var-gens formula final-form))
		(else
			(adl-compile-others-lambda var-gens formula final-form))))
*/

static CELLP CompileSuccessor
(
	CELLP pcVarGens,					/* variable generator pair(s) */
	CELLP pcAntecedent,					/* antecedent (required) */
	CELLP pcConsequent					/* consequent (optional) */
)
{
	if(pcAntecedent&&(AndFormQ(pcAntecedent)||AtomicFormQ(pcAntecedent)))
		return CompileAndFormula(pcVarGens,pcAntecedent,pcConsequent);
	return CompileOtherFormula(pcVarGens,pcAntecedent,pcConsequent);
}

/* CompileOtherFormula

Description:
	Generate a forall formula for a formula other than an and formula.
Scheme:

(define (adl-compile-others-lambda var-gens formula final-form)
  "Generate a forall formula for a formula other than an and formula."
  (cond
   ((and (null? var-gens) (null? formula))
	final-form)
   ((null? var-gens)
	(list 'IMPLIES formula final-form))
   (else
	`(FORALL ,(adl-get-vars (first var-gens))
			 ,(adl-get-gen (first var-gens))
			 ,(adl-compile-others-lambda
			   (rest var-gens) formula final-form)))))
*/

static CELLP CompileOtherFormula
(
	CELLP pcVarGens,
	CELLP pcAntecedent,
	CELLP pcConsequent
)
{
	if(!pcVarGens)
	{
		if(!pcAntecedent)
			return pcConsequent;
		if(!pcConsequent)
			return pcAntecedent;
		return MakeImpliesForm(FALSE,pcAntecedent,pcConsequent);
	}
	return MakeForAllForm(FALSE,pcVarGens->pfForm->pcVars,pcVarGens->pfForm->pcGenLit,
		CompileOtherFormula(pcVarGens->pcNext,pcAntecedent,pcConsequent));
}

/* CompileAndFormula

Description:
	Generate a forall formula for an "and" or an "atomic" formula.
Notes:
	And formulas should have conjuncts move up to the place where the
	variables are first generated. E.g., if we have the VarGens

	(((?x) (R ?x)) ((?y) (Q ?x ?y))

	and the formula

	(and (P ?x) (A ?x ?y))

	we want to generate

	(FORALL (?x) (R ?x)
		(implies (P ?x) (FORALL (?y) (Q ?x ?y) (A ?x ?y))))

	ADLCompileOtherFormula would generate the following less efficient to
	evaluate formula:

	(FORALL (?x) (R ?x) (FORALL (?y) (Q ?x ?y) (and (P ?x) (A ?x ?y))))

	The formula generated by ADLCompileAndFormula is logically
	equivalent (of course).

Scheme:

(define (adl-compile-and-lambda var-gens formula final-form)
	(let ((conjuncts '())
			(sentences '())
			(recursive-form '())
			(var-gen-forms '()))
		(set! conjuncts
			(if (and-formp formula)
				(get-args formula)
				(list formula)))  ;handle atomic as single argument and
		(set! var-gen-forms
			(reverse (map
					(lambda (var-gen) (list var-gen '()))
					var-gens)))
		(for-each
			(lambda (conj)
				(let ((vars (find-free-vars conj))
						(outer #t))
					(if (null? vars)
						(set! sentences (cons conj sentences))
						(begin
							(call-with-current-continuation
								(lambda (return)
									(for-each
										(lambda (var-gen-form)
											(if (not (null? (intersection
															vars
															(adl-get-vars (first var-gen-form)))))
												(begin
													(set-car! (rest var-gen-form)
														(cons conj (cadr var-gen-form)))
													(set! outer '())
													(return #t))))
										var-gen-forms)))
							(if outer (set! sentences (cons conj sentences)))))))
			conjuncts)
		(set! var-gen-forms (reverse var-gen-forms))
		(for-each (lambda (var-gen-form)
				(set-car! (rest var-gen-form)
					(reverse (second var-gen-form))))
			var-gen-forms)
		(set! recursive-form (adl-compile-and-recurse var-gen-forms final-form))
		(cond
			((and sentences recursive-form (length=1 sentences))
				`(IMPLIES ,@sentences ,recursive-form))
			((and sentences recursive-form)
				`(IMPLIES (AND ,@sentences) ,recursive-form))
			(else
				recursive-form))))
*/

static CELLP CompileAndFormula
(
	CELLP pcVarGens,
	CELLP pcAntecedent,
	CELLP pcConsequent
)
{
	CELLP pcConjuncts;					/* list of formulas */
	CELLP pcSentences;					/* list of unbound formulas */
	CELLP pcForAllForms;				/* list of forall formulas */
	CELLP pc,pc1;
	CELLP pcVars;
	CELLP pcConj1,pcConj2;
	BOOL bBound;						/* formula has be bound to a quantifier */

	if(AndFormQ(pcAntecedent))
		pcConjuncts=pcAntecedent->pfForm->pcArgs;
	else
		pcConjuncts=pcAntecedent;		/* handle atomic as single argument AND */

	/* Create a reverse order list of quantified formulas from the vargens. */

	pcForAllForms=NULL;
	for(pc1=pcVarGens;pc1;pc1=pc1->pcNext)
	{
		pc=MakeForAllForm(FALSE,pc1->pfForm->pcVars,pc1->pfForm->pcGenLit,NULL);
		pc->pcNext=pcForAllForms;
		pcForAllForms=pc;
	}

	/* Move each conjunct to its outermost position */

	pcSentences=NULL;
	for(pcConj1=pcConjuncts;pcConj1;pcConj1=pcConj2)
	{
		pcConj2=pcConj1->pcNext;
		pcConj1->pcNext=NULL;
		pcVars=FindFreeVars(pcConj1,NULL,NULL);
		if(!pcVars)						/* if no free variables */
		{
			pcConj1->pcNext=pcSentences;
			pcSentences=pcConj1;
		}
		else							/* look for the inner most formula with a match */
		{
			bBound=FALSE;
			for(pc=pcForAllForms;pc;pc=pc->pcNext)
			{
				if(FormulaIntersects(pcVars,pc->pfForm->pcVars))
				{
					pcConj1->pcNext=pc->pfForm->pcArgs;
					pc->pfForm->pcArgs=pcConj1;
					bBound=TRUE;
					break;
				}
			}
			if(!bBound)
			{
				pcConj1->pcNext=pcSentences;
				pcSentences=pcConj1;
			}
		}
	}

	/* put everything back in original order */

	pcSentences=ReverseFormulaList(pcSentences);
	pcForAllForms=ReverseFormulaList(pcForAllForms);
	for(pc=pcForAllForms;pc;pc=pc->pcNext)
		pc->pfForm->pcArgs=ReverseFormulaList(pc->pfForm->pcArgs);

	/* Reassemble the formula */

	pc=CompileAndRecurse(pcForAllForms,pcConsequent);
	if(pcSentences)
	{
		pc=AppendFormula(FALSE,pcSentences,pc);
		if(pc->pcNext)
			pc=MakeAndForm(FALSE,pc);
	}
	return pc;
}

/* CompileAndRecurse

Description:
	Formulas have been placed in their correct place by CompileAndFormula,
	now reassemble the formula.
Note:
	If we started from a reversed list, it would be simple to convert this to
	an iterative routine!.
Scheme:

(define (adl-compile-and-recurse var-gen-forms final-form)
  (let ((var-gen-form (first var-gen-forms)))
	(cond
	 ((null? var-gen-form)
	  final-form)
	 ((null? (second var-gen-form))     ;No formula, just generator.
	  `(FORALL ,(adl-get-vars (first var-gen-form))
			   ,(adl-get-gen (first var-gen-form))
			   ,(adl-compile-and-recurse (rest var-gen-forms) final-form)))
	 (else                                 ;Formula.
	  `(FORALL ,(adl-get-vars (first var-gen-form))
			   ,(adl-get-gen (first var-gen-form))
			   (IMPLIES
				,(if (length=1 (second var-gen-form))
					 (first (second var-gen-form))
				   (cons 'AND (second var-gen-form)))
				,(adl-compile-and-recurse
				  (rest var-gen-forms) final-form)))))))
*/

static CELLP CompileAndRecurse
(
	CELLP pcForAllForms,				/* list of forall formulas */
	CELLP pcConsequent					/* innermost formula */
)
{
	CELLP pc;

	if(!pcForAllForms)					/* no formula or generator */
		pc=pcConsequent;
	else
	{
		pc=CompileAndRecurse(pcForAllForms->pcNext,pcConsequent);
		if(!pcForAllForms->pfForm->pcArgs)		/* no formula, just generator */
			pcForAllForms->pfForm->pcArgs=pc;
		else							/* both formula and generator */
		{
			if(pcForAllForms->pfForm->pcArgs->pcNext)	/* if list of formulas, enclose with and */
				pcForAllForms->pfForm->pcArgs=MakeAndForm(FALSE,pcForAllForms->pfForm->pcArgs);
			if(pc)						/* if consequent */
				pcForAllForms->pfForm->pcArgs=MakeImpliesForm(FALSE,pcForAllForms->pfForm->pcArgs,pc);
		}
		pcForAllForms->pcNext=NULL;
		pc=pcForAllForms;
	}
	return pc;
}

/* FindFreeVars

Description:
	Return the free variables of a single formula.
Notes:
	This routine does not work properly if it is passed a list of formulas!
Scheme:

(define (find-free-vars form . optional-args)
	(let ((bound-vars '())
			(free-vars '()))
		(if (not (null? optional-args))
			(begin (set! bound-vars (first optional-args))
				(set! optional-args (rest optional-args))))
		(if (not (null? optional-args))
			(begin (set! free-vars (first optional-args))
				(set! optional-args (rest optional-args))))
		(cond
			((or (call-formp form) (null? form))
				free-vars)
			((or (forall-formp form) (exists-formp form))
				(map (lambda (var)
						(if (not (memq var bound-vars))
							(set! bound-vars (cons var bound-vars))))
					(get-qf-variables (get-args form)))
				(map (lambda (var)
						(if (not (memq var free-vars))
							(set! free-vars (cons var free-vars))))
					(set-difference (variables-in (get-qf-generator (get-args form)))
						bound-vars))
				(find-free-vars (get-qf-formula (get-args form))
					bound-vars free-vars))
			((binding-formp form)
				(map (lambda (var)
						(if (not (memq var bound-vars))
							(set! bound-vars (cons var bound-vars))))
					(get-binding-vars form))
				(find-free-vars (get-binding-formula form)
					bound-vars free-vars))
			((atomic-formp form)
				(map (lambda (var)
						(if (not (memq var free-vars))
							(set! free-vars (cons var free-vars))))
					(set-difference (variables-in form) bound-vars))
				free-vars)
			(else
				(do ((args (get-args form))
						(current-free-vars free-vars)
						(arg '()))
						((null? args) free-vars)
						(set! arg (first args))
						(set! args (rest args))
						(for-each (lambda (var)
								(if (not (memq var free-vars))
									(set! free-vars (cons var free-vars))))
							(find-free-vars arg bound-vars current-free-vars)))))))
*/

static CELLP FindFreeVars
(
	CELLP pcFormula,
	CELLP pcBVars,						/* bound variables (initialize to NULL) */
	CELLP pcFVars						/* free variables (initialize to NULL) */
)
{
	CELLP pc;
	CELLP pcBoundVars;
	CELLP pcFreeVars;
	CELLP pcVar1,pcVar2;

	ENTER("FindFreeVars",TRUE);
	pcBoundVars=pcBVars;
	pcFreeVars=pcFVars;

	if(!pcFormula)
		pc=pcFreeVars;
	else if(ForAllFormQ(pcFormula)||ExistsFormQ(pcFormula)||ExistsXFormQ(pcFormula))
	{
		for(pcVar1=pcFormula->pfForm->pcVars;pcVar1;pcVar1=pcVar1->pcNext)
		{
			if(!FormulaMemQ(pcVar1,pcBoundVars))
			{
				pcVar2=CopyCell(pcVar1);
				pcVar2->pcNext=pcBoundVars;
				pcBoundVars=pcVar2;
			}
		}
		for(pcVar1=FormulaDifference(VariablesIn(pcFormula->pfForm->pcGenLit),pcBoundVars);
			pcVar1;pcVar1=pcVar2)

/*		for(pcVar1=FindFreeVars(pcFormula->pcGenLit,pcBoundVars,pcFreeVars);
			pcVar1;pcVar1=pcVar2)
*/
		{
			pcVar2=pcVar1->pcNext;
			pcVar1->pcNext=NULL;
			if(!FormulaMemQ(pcVar1,pcFreeVars))
			{
				pcVar1->pcNext=pcFreeVars;
				pcFreeVars=pcVar1;
			}
		}
		pc=FindFreeVars(pcFormula->pfForm->pcArgs,pcBoundVars,pcFreeVars);
	}
	else if(BindingFormQ(pcFormula))
	{
		for(pcVar1=GetBindingVars(pcFormula);pcVar1;pcVar1=pcVar1->pcNext)
		{
			if(!FormulaMemQ(pcVar1,pcBoundVars))
			{
				pcVar2=CopyCell(pcVar1);
				pcVar2->pcNext=pcBoundVars;
				pcBoundVars=pcVar2;
			}
		}
		pc=FindFreeVars(GetBindingFormula(pcFormula),pcBoundVars,pcFreeVars);
	}
	else if(AtomicFormQ(pcFormula))
	{
		for(pcVar1=FormulaDifference(VariablesIn(pcFormula),pcBoundVars);
			pcVar1;pcVar1=pcVar2)
		{
			pcVar2=pcVar1->pcNext;
			pcVar1->pcNext=NULL;
			if(!FormulaMemQ(pcVar1,pcFreeVars))
			{
				pcVar1->pcNext=pcFreeVars;
				pcFreeVars=pcVar1;
			}
		}
		pc=pcFreeVars;
	}
	else								/* this must be a formula */
	{
		CELLP pcCurrentFreeVars;
		CELLP pcArg;

		pcCurrentFreeVars=pcFreeVars;
		for(pcArg=pcFormula->pfForm->pcArgs;pcArg;pcArg=pcArg->pcNext)
		{
			for(pcVar1=FindFreeVars(pcArg,pcBoundVars,pcCurrentFreeVars);
				pcVar1;pcVar1=pcVar2)
			{
				pcVar2=pcVar1->pcNext;
				if(!FormulaMemQ(pcVar1,pcFreeVars))
				{
					pcVar1->pcNext=pcFreeVars;
					pcFreeVars=pcVar1;
				}
			}
		}
		pc=pcFreeVars;
	}
	EXIT("FindFreeVars");
	return pc;
}

