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
#include <math.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "compute.h"
#include "eval.h"
#include "formula.h"
#include "hashrelax.h"
#include "iface.h"
#include "makeform.h"
#include "makegen.h"
#include "oper.h"
#include "plan.h"
#include "search.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "var.h"
#include "world.h"
#include "zone.h"
#include "domain.h"
#include "hbtree.h"
#include "oper.h"
#include "queue.h"

/* global data */

LINEARPLANP plpSuccessors;				/* successor states for an operator */
LINEARPLANP plpLastSuccessor;			/* pointer to end of successor list */
LINEARPLANP plpSuccessorWorld;			/* current successor state */


LINEARPLANP plpNegPlan;  /* plan for world of negated facts */
LINEARPLANP plpLevel0Plan;  /* plan for level-0 (positive) world of negated facts */
BTREEP *apbtNegWorld; /* pointer to the negated world */
HBTREEP *aphbtNegWorld; /* pointer to the h-negated world */

BOOL bRelaxedModeNeg=0; /* whether or not we are in relaxed considering negations */
BOOL bUnderNot=0; /* whether or not while evaluating a formula, we've seen a not */
BOOL bUnderRelaxedModifyWorld=0; /* true whenever evaluating under a relaxed modify world operator*/
BOOL bUseHWorld=0; /* whether or not some functions must make reference to the H-world instead
		      of the standard mail */
BOOL bUseHWorldFF=0; /* whether or not some functions must make reference to the H-world instead
		      of the standard mail */
BOOL bMutexMode=0; /* whether or not we are computing mutexes */

BOOL bExtractingRelaxedPlan=0; /* whether or not we are extracting a relaxed plan, this variable 
			      affects the behaviour of EvalDescPredicate */

BOOL bVerifyingPreservedProperty=0; /* whether or not we are verifying a preserved property */


BOOL nOcclusionPenalty; // the occlusion penalty when computing occlusions
BOOL nMutexPenalty; // the occlusion penalty when computing occlusions
BOOL bComputingOcclusions=0;  /* whether or not we are computing occlusions */
BOOL bOcclusions=0;  /* whether or not occlusions will be computed */
static BOOL bRealOcclusions=1;
static BOOL bGoalOcclusions=1; /* whether or not goal occlusions will be computed */
static BOOL bTryToPreserve=1;


static LISTP plThisLevelActions; // relaxed plan actions at this level. Used for occlusion penalty computation
static LISTP plGoalFacts; // facts that are used to make goal true at the en of relaxed plan extraction


/* FF variables */
BOOL bFFSimplifyRelaxedPlan=FALSE;      // whether or not to do relaxed plan simplification
int nFFDepth;       /* depth at which a fact was added  */
int nFFOperNumber;  /* number of the operator that added a fact */
int nFFOperInstance;/* number of the instantiation of an operator that added a fact */
CELLP pcFFActionInstance; /* pointer to the actual action instance that added a fact */
BINDINGP pbCurrentActionBindings; /* Bindings for the current action in execution */

int nGenSuccessorsOperNumber;     // operator number that is currently generating a successor
int nGenSuccessorsInstanceNumber; // instance number of the operator that is currently generating a successor
OPERINSTANCEP poiCurrentInstance;  // current instance


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
	CELLP *ppcPriority,					/* output priority */
	BOOL  *bHSPIgnore,
	BOOL  *pbUnrelEff
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

static void change_nonadding_forall_actions
(
 CELLP pc, 
 ACTION *paAction
);

void PrintHNode
(
 FILE *fp,
 HBTREEP phbtTree
);


static void SetQuantifierEvalersFF(void); /* set quantifier evaluators for FF heuristic */

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
	BOOL  bHSPIgnore;
	BOOL  bUnrelEff;
	OPERATORP po;						/* compiled operator */
	char ac[128];						/* string buffer */

	/* break the command string into its components */

	if(!ADLParse(pcFormula,&pcName,&pcPre,&pcMod,&pcDuration,&pcCost,&pcPriority,&bHSPIgnore,&bUnrelEff))
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

	po=MakeOperator(pcName,pcSuccessor,pcDuration,pcCost,pcPriority,bHSPIgnore,bUnrelEff);
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


BOOL EvalDefADLHOperator
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
	BOOL  bHSPIgnore;
	BOOL  bUnrelEff;
	OPERATORP po;						/* compiled operator */
	char ac[128];						/* string buffer */

	/* break the command string into its components */

	if(!ADLParse(pcFormula,&pcName,&pcPre,&pcMod,&pcDuration,&pcCost,&pcPriority,&bHSPIgnore,&bUnrelEff))
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

	po=MakeHOperator(pcName,pcSuccessor,pcDuration,pcCost,pcPriority,bHSPIgnore,bUnrelEff);
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
	CELLP *ppcPriority,					/* output priority */
	BOOL  *pbHSPIgnore,
	BOOL  *pbUnrelEff
)
{
	CELLP pcName;						/* output name */
	CELLP pcPre;						/* output preconditions */
	CELLP pcMod;						/* output modifier list */
	CELLP pcDuration;					/* output delta time */
	CELLP pcCost;						/* output delta cost */
	CELLP pcPriority;					/* output priority */
	BOOL  bHSPIgnore;                                       /* whether the operator must be ignored by the HSP heuristic */
	BOOL  bUnrelEff;                                        /* whether the operator has only unrelaxed effects */
	CELLP pc,pc1;
	char ac[40];						/* string buffer */

	/* set defaults */

	pcName=NULL;
	pcPre=NULL;
	pcMod=NULL;
	pcDuration=NULL;
	pcCost=NULL;
	pcPriority=NULL;
	bHSPIgnore=FALSE;
	bUnrelEff=FALSE;

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
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_HSP_IGNORE]))
		{
		        bHSPIgnore=TRUE;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_UNREL_EFFECTS]))
	        {
		        bUnrelEff=TRUE;
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
	*pbHSPIgnore=bHSPIgnore;
	*pbUnrelEff=bUnrelEff;
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
	int nPosition;

	ENTER("MakeModifyWorldForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->psName=IdentAllocAndPos("modify-world",&nPosition);
	pf->nHashPos=nPosition;
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
	int nPosition;

	ENTER("MakeOperatorForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_OPERATORP;
	pf->psName=IdentAllocAndPos("operator",&nPosition);
	pf->nHashPos=nPosition;
	pf->uValue.poOperator=poOperator;
	pf->paAction=&aOperatorAction;
	EXIT("MakeOperatorForm");
	return pc;
}




/* EvalModifyWorldRelaxed

Description:
        Apply the action in the *current* world, as opposed to
        EvalModifyWorld, this function *does not* generate a new world
	We don't label the world with the action that created it
*/

BOOL EvalModifyWorldRelaxed
(
	CELLP pcFormula,				/* modify world formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings				/* current variable bindings */
)
{
    CELLP pcMod;
    OPERATORP po;

    /* handle modifications */    
    bUnderRelaxedModifyWorld = 1;
    
    //   if (bVerbose) { /* necessary information to print the relaxed plan in the log file */
    if (bVerboseFFShowRelaxedPlans) { 
      po=pcFormula->pfForm->pcArgs->pfForm->uValue.poOperator;
      pcFFActionInstance=CopyFormula(po->pcName);	/* this is rather inefficient! */
      if(po->pcName->pfForm->pcArgs) {
	pcFFActionInstance->pfForm->pcArgs=ComputeTerms(po->pcName->pfForm->pcArgs,plpLinearPlan,pbBindings);
	if(!pcFFActionInstance->pfForm->pcArgs)
	  TermError("eval-modify-world",pcFormula,pbBindings);
      }
      /* END: code to store instantiated action */
    }
    
    // want to print actions per level?
    //    if (njbNodeCounter==25) PrintFormula(HandleToFileStream(3),pcFFActionInstance,0);

    if (bOcclusions) {
      pbCurrentActionBindings=CopyBindings(pbBindings); 
    } else {
      pbCurrentActionBindings=NULL;
    }
    
    // now process rest of the formula
    
    poiCurrentInstance=(OPERINSTANCEP)MemAlloc(sizeof(OPERINSTANCE));
    poiCurrentInstance->pbBindings=pbCurrentActionBindings;
    poiCurrentInstance->pcSupFacts=pcFFSupFacts;
    poiCurrentInstance->nOperNumber=nFFOperNumber;
    poiCurrentInstance->nOperInstance=nFFOperInstance;
    poiCurrentInstance->nDepth=nFFDepth;
    poiCurrentInstance->pcActionInstance=pcFFActionInstance;
    poiCurrentInstance->dHspCost=dEvalTotalCost;
    poiCurrentInstance->nAddedFacts=0;
    poiCurrentInstance->plFlipUndo=NULL;
    poiCurrentInstance->plFlipUndoMin=NULL;
    
    pcMod=pcFormula->pfForm->pcArgs->pcNext;
    if(pcMod)
	(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
    
    ++nFFOperInstance; 
    
    bUnderRelaxedModifyWorld = 0;

    return TRUE;
}


BOOL EvalModifyWorldNullAdvance
(
	CELLP pcFormula,				/* modify world formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings				/* current variable bindings */
)
{
  SetQuantifierEvalersFF(); /* set quantifiers to "normal" */
  plThisLevelActions=plThisLevelActions->plNext;
  return TRUE; // don't do anything :)
}



BOOL EvalModifyWorldExecute
(
	CELLP pcFormula,				/* modify world formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings				/* current variable bindings */
)
{
    CELLP pcMod;

    /* handle modifications */    
    bUnderRelaxedModifyWorld = 1;


    SetQuantifierEvalersFF(); /* set quantifiers to "normal" */
	  
    pcMod=pcFormula->pfForm->pcArgs->pcNext;
    
    if(pcMod) {
      (*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
      plThisLevelActions=plThisLevelActions->plNext;
    }

    bUnderRelaxedModifyWorld = 0;
    
    return TRUE;
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
			UpdateWorld();
			/* Now do post-action sequence in the new world (not parent world) */
			for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext)
			{
			  (*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
			  UpdateWorld();					/* apply changes after every formula */
			}

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
        /* Now do post-action sequence in the new world */
	for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext)
	{
	  (*pcMod->pfForm->paAction->pfEval)(pcMod,plpSuccessorWorld,pbBindings);
	  UpdateWorld();					/* apply changes after every formula */
	}

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
	
	

	/* tag this world as produced by a helpful action if it was */
	//fprintf(HandleToFileStream(3),"Expanding (%d,%d) pl=%p ",nGenSuccessorsOperNumber,nGenSuccessorsInstanceNumber,plCurrentHelpfulActions);
	if (plCurrentHelpfulActions &&
	    plCurrentHelpfulActions->uValue.poiOperInstance->nOperNumber == nGenSuccessorsOperNumber &&
	    plCurrentHelpfulActions->uValue.poiOperInstance->nOperInstance == nGenSuccessorsInstanceNumber) {
	  //  fprintf(HandleToFileStream(3),"yes\n"); 
	  plpSuccessorWorld->bIsPreferredNode=1;
	  plCurrentHelpfulActions=plCurrentHelpfulActions->plNext; /* advance the helpful action list */
	}
	else {
	  //  fprintf(HandleToFileStream(3),"no\n"); 
	  plpSuccessorWorld->bIsPreferredNode=0;
	}
	/* link this world to the end of the successor list */

	plpLastSuccessor=plpLastSuccessor->plpNext=plpSuccessorWorld;
	plpLastSuccessor->plpNext=NULL;
	
	nGenSuccessorsInstanceNumber++;  /* increment the instance number */

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
	  for(po=poOperators,nGenSuccessorsOperNumber=0;po;po=po->poNext,nGenSuccessorsOperNumber++)
		{
			plpSuccessors=NULL;			/* initialize the successor list */
			plpLastSuccessor=(LINEARPLANP)&plpSuccessors;

			nGenSuccessorsInstanceNumber=0;  /* initialize the instance number */

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

/* ComputeGoalDistance Functions*/

int firstRunGoalDistances=1; // whether or not ComputeGoalDistance_CRC
                             // is called for the first time
                             // (this variable is reset in ResetGoalDistanceStatics() 

int nOcclusionPenalty=0;
BOOL bComputeOcclusion=0;

int totalGoals=-1;          
int totalPreferences=-1;
int total_preferences = -1;             // remove when convinced about ComputeGoalDistance_CRC
double dDiscountedMetricMultiplier=0.0;  // discounted metric multiplier
double dMetricMultiplier=0.0;           // metric multiplier
double dBestMetricMultiplier=0.0;       // best metric multiplier
double dOptimisticMetricMultiplier=0.0; // optimistic metric multiplier
double dPreferenceMultiplier=0.0;       // preference multiplier
double dGoalMultiplier=1.0;             // goal multiplier
double dHeuristicExponent=.2313774486;  // heuristic exponent 
double dMetricDiscountFactor=0.9;       // metric discount factor

double dGoalOcclusionMultiplier=1;   // Goal occlusion penalty multiplier
double dPrecondOcclusionMultiplier=1;   // Penalty occlusion penalty multiplier

int nDiscountedMetricPriority=-1;       // discounted metric priority
int nOptimisticMetricPriority=-1;       // optimistic metric priority
int nBestRelaxedMetricPriority=-1;      // best relaxed metric priority
int nPreferenceDistancePriority=-1;     // preference distance priority
int nMetricPriority=-1;                 // priority of the world metric


/* static variables */

static int nRelPlanDepth; /* depth of relaxed plan, used by EvalDescPredicate_Extract_Relaxed_Plan */
 

void ResetGoalDistanceStatics() 
{
    
   firstRunGoalDistances=1;
   
   totalGoals=-1;  // this should be removed from here.
                   // ComputeGoalDistances_CRC sets this appropriately
                   // on its first run

   total_preferences = -1; // this variable should soon be deprecated
                           // it is now replaced by totalPreferences
                           // in ComputeGoalDistance_CRC
}



int TotalNumberOfGoals() {
    CELLP pc;
    int goals = 0;
    if (!pcGoalFormula) return 0; /* there's no goal */
    if (!pcGoalFormula->pfForm->pcArgs) return 1; /* there is a single goal (so not an and formula)*/
    for(pc=pcGoalFormula->pfForm->pcArgs; pc ; pc=pc->pcNext) {
      ++goals;
    }
    return goals;
}


int NumberOfGoalsSatisfied(LINEARPLANP plan,BINDINGP pbBindings) {
    CELLP pc;
    int goals = 0;
    if (!pcGoalFormula) return 0; /* there's no goal */
    if (!pcGoalFormula->pfForm->pcArgs) {
      if((*pcGoalFormula->pfForm->paAction->pfEval)(pcGoalFormula,plan,pbBindings))
	return 1; /* satisfied sole goal */
      else return 0;
    }
    //Finally we have a list of goals.
    for(pc=pcGoalFormula->pfForm->pcArgs; pc ; pc=pc->pcNext) {
	if((*pc->pfForm->paAction->pfEval)(pc,plan,pbBindings))
	    ++goals;
    }
    return goals;
}


int NumberOfGoalsSatisfied_H(LINEARPLANP plan,BINDINGP pbBindings,double *cost) {
    CELLP pc;
    int goals = 0;
    if (!pcGoalFormula) return 0; /* there's no goal */
    if (!pcGoalFormula->pfForm->pcArgs) {
      if((*pcGoalFormula->pfForm->paAction->pfEval)(pcGoalFormula,plan,pbBindings)) {
	*cost=dEvalTotalCost;
	return 1; /* satisfied sole goal */
      } else return 0;
    }
    //Finally we have a list of goals.
    for(pc=pcGoalFormula->pfForm->pcArgs; pc ; pc=pc->pcNext) {
	if((*pc->pfForm->paAction->pfEval)(pc,plan,pbBindings))
	    ++goals;
    }
    *cost=dEvalTotalCost;
    return goals;
}


int NumberOfGoalsSatisfied_H_costvector(LINEARPLANP plan,BINDINGP pbBindings,double *cost,double *cost_vector) {
    CELLP pc;
    int goals = 0;
    double dPreviousEvalTotalCost;
    dEvalTotalCost=0;
    if (!pcGoalFormula) return 0; /* there's no goal */
    if (!pcGoalFormula->pfForm->pcArgs) {
      if((*pcGoalFormula->pfForm->paAction->pfEval)(pcGoalFormula,plan,pbBindings)) {
	*cost=dEvalTotalCost;
	return 1; /* satisfied sole goal */
      } 
      else return 0;
    }
    //Finally we have a list of goals.
    for(pc=pcGoalFormula->pfForm->pcArgs; pc ; pc=pc->pcNext) {
      dPreviousEvalTotalCost=dEvalTotalCost;
      if((*pc->pfForm->paAction->pfEval)(pc,plan,pbBindings)) {
	cost_vector[goals]=dEvalTotalCost-dPreviousEvalTotalCost;
	++goals;
      }
    }
    *cost=dEvalTotalCost;
    return goals;
}



int TotalNumberOfPreferences(LINEARPLANP plan,BINDINGP pbBindings) {
    int goals = 0;
    if (!pcTotalPrefsFn) return 0; /* there's no preference counting formula */
    FormulaToInteger(pcTotalPrefsFn,plan,pbBindings,&goals);
    return goals;
}

//int NumberOfPreferencesSatisfied(LINEARPLANP plan,BINDINGP pbBindings) {
//    CELLP pc;
//    int goals = 0;
//    if (!pcPreferenceFormula) return 0; /* there's no preference */
//    if (!pcPreferenceFormula->pfForm->pcArgs) {
//      if((*pcPreferenceFormula->pfForm->paAction->pfEval)(pcPreferenceFormula,plan,pbBindings))
//	return 1; /* satisfied sole preferences */
//      else return 0;
//    }
    //Finally we have a list of preferences
//    for(pc=pcPreferenceFormula->pfForm->pcArgs; pc ; pc=pc->pcNext) {
//	if((*pc->pfForm->paAction->pfEval)(pc,plan,pbBindings))
//	    ++goals;
//    }
//    return goals;
//}

int NumberOfPreferencesSatisfied(LINEARPLANP plan,BINDINGP pbBindings) {
    int goals = 0;
    if (!pcCountSatPrefsFn) return 0; /* there's no preference counting formula */
    FormulaToInteger(pcCountSatPrefsFn,plan,pbBindings,&goals);
    return goals;
}

int NumberOfPreferencesSatisfiedWithCost(LINEARPLANP plan,BINDINGP pbBindings,double *cost) {
    int goals = 0;
    if (!pcCountSatPrefsFn) return 0; /* there's no preference counting formula */
    dEvalTotalCost=0;
    FormulaToInteger(pcCountSatPrefsFn,plan,pbBindings,&goals);
    *cost=dEvalTotalCost;
    return goals;
}


void InitGoalDistance() /* ComputeGoalDistance calls this function in the first run */
{ 
/* setup the hash table */
    RelCRCHashCreate();
/* compute the number of goals and number of preferences */
    totalGoals=TotalNumberOfGoals();

    /* we do not use a totalPreferences any more */  
//    totalPreferences=TotalNumberOfPreferences();

    firstRunGoalDistances=0;

}


void InitGoalDistance2(LINEARPLANP plan,BINDINGP pbBindings) /* Like InitGoalDistance but does additional things to make everything
			    suitable for real HSP-like costs */
{ 
  OPERATORP po;
  CELLP pc;
  
/* setup the hash table */
    RelCRCHashCreate();
/* compute the number of goals and number of preferences */
    totalGoals=TotalNumberOfGoals();

    /* we do not use a totalPreferences any more */  
    
    totalPreferences=TotalNumberOfPreferences(plan,pbBindings); /* this version does use the total number 
						    or preferences to stop graph expansion */

    firstRunGoalDistances=0;


    /* preprocess foralls that do not have an add or del as child are now modified
       not to reset the costs */
    
    for(po=poOperators;po;po=po->poNext) {
      change_nonadding_forall_actions(po->pcSuccessor, &aForAllActionNoReset);
    }
    
    for(pc=pcPostActionSequence;pc;pc=pc->pcNext) {
      change_nonadding_forall_actions(pc, &aForAllActionNoReset);
    }						
    
    if (pcCountSatPrefsFn) {
      change_nonadding_forall_actions(pcCountSatPrefsFn->pfForm->uValue.psiSymbolInfo->pcFormula, &aForAllActionNoReset);
    }
}



CELLP ComputeGoalDistance_simple
/* we assume (world-mode) is a the END of the list of goals */
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
    CELLP pc;
    CELLP pcMod;
    BTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    int distance=0;
    ZONE *saveZone = pzCurrent;
    const int DISTANCE_LIMIT = 10; 
    OPERATORP po;

    ENTER("ComputeGoalDistance_simple",TRUE);
    
    /* redefining modify world action and del action */
 
    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aDelAction.pfEval = EvalNODel;

    /* create a new plan */
    SetZone(&zScratch);
    newWorld = CopyWorld(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 

    /* now code from generate successor */
    while ( !PlanGoalQ(temporalPlan) && distance < DISTANCE_LIMIT) {
      int a;
      for(po=poOperators;po;po=po->poNext) /* execute all possible actions */
	(*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
      a=AddsFromUpdateWorld();             /* add the updates then do post actions*/
      for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	(*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	a+=AddsFromUpdateWorld();
      }
      if(a < 1) /* do the updates and count the number of adds */
	break;
      distance++; 
    }  
    ZoneClear(&zScratch);
    SetZone(saveZone);

    /* ModifyWorld back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    //fprintf(stderr, "/%d", distance);
    pc=MakeFloatForm(distance);
    EXIT("ComputeGoalDistance_simple");
    return pc;   
}


WORLDSUMP ComputeAllDistances
(
    LINEARPLANP temporalPlan, 
    BINDINGP pbBindings,
    int nDepth,         
    int nDepthLimit
) 
{
    int goals;
    int preferences;
    double metric;
    unsigned int worldCRC;
    int a;
//    int stopCondition;
    OPERATORP po;
    WORLDSUMP pWorldSummary;
    WORLDSUMP pNextWorldSummary;
    CELLP pcMod;

    ENTER("ComputeAllGoalDistances",TRUE);

    if (nDepth==nDepthLimit) return NULL; /* there's no next world */
    
    /* the current world is not in the hash table, we add it */

    goals=preferences=0;
    metric=0.0;
    goals = NumberOfGoalsSatisfied(temporalPlan,pbBindings);
    
    if (dPreferenceMultiplier!=0.0) {
	preferences = NumberOfPreferencesSatisfied(temporalPlan,pbBindings);
    }

    FormulaToDouble(pcMetricFn,temporalPlan,pbBindings,&metric);

    worldCRC = temporalPlan->nSignature; // the signature was computed in the previous recursion
    pWorldSummary = RelCRCHashInsert(worldCRC,goals,preferences,metric);




    /* for debugging, print the current world in the log file */
    //    if (nDepth == 0) {
    //	fprintf(HandleToFileStream(3),"World at depth %d (%u):",nDepth,temporalPlan->nSignature);
    //	PrintWorld(3,temporalPlan,pbBindings);
    //    }

    /* compute the stop condition for the recursion */

// since it's very unlikely that we'll satisfy all the preference
// we stop now only when a fixed point is reached
//    stopCondition=1;
//    stopCondition&=(goals>=totalGoals);
//    stopCondition&=(preferences>=totalPreferences);
    
//    if (stopCondition) {  // we've satisfied all the goals/preferences
//	pWorldSummary->pNext=NULL;
//	return pWorldSummary;
//    }

    /* we compute the successor world */
    for(po=poOperators;po;po=po->poNext)
	(*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
    a = AddsFromUpdateWorld();
    
    for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	(*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	a+=AddsFromUpdateWorld();
    }

    /* for debugging, print the current world in the log file */

    

    if(a < 1) {  /* we have reached a fix point */
	pWorldSummary->pNext = NULL; /* no next world */
	return pWorldSummary;
    }
    WorldSignature(temporalPlan);

    /* debugging */
    //fprintf(HandleToFileStream(3),"World at depth %d: (%u)",nDepth,temporalPlan->nSignature);
    //PrintWorld(3,temporalPlan,pbBindings);

    //if (bRelaxedModeNeg) {
    //	fprintf(HandleToFileStream(3),"Negated world at depth %d: (%u)",nDepth,temporalPlan->nSignature);
    //	PrintWorld(3,plpNegPlan,pbBindings);
    //}
    


    //  fprintf(pfJBCRC,"%u\n",temporalPlan->nSignature);
    if (! (pNextWorldSummary = RelCRCHashSearch(temporalPlan->nSignature)))
	pNextWorldSummary=ComputeAllDistances(temporalPlan,pbBindings,nDepth+1,nDepthLimit);
    
    pWorldSummary->pNext = pNextWorldSummary; // linking the list

    EXIT("ComputeAllDistances");
    return pWorldSummary;
}

CELLP ComputeGoalDistance_CRC
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
) 
{
    CELLP pc;
    BTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    WORLDSUMP pWorldSummary;
    const int DISTANCE_LIMIT = 100;
    double best_metric=100000.0;
    double discountedMetric=100000.0;
    double metric;
    int goals, preferences;
    int numGoalsAchieved,numPreferencesAchieved;
    int distance;
    const int decimalsToSave = pow(10, 3);
    double goalDistanceSum,preferenceDistanceSum;
    double optimisticMetric;
    double last_metric;
    ZONE *saveZone = pzCurrent;

    ENTER("ComputeGoalDistance_CRC",TRUE);

        
    /* redefining modify world action and del action */
    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aDelAction.pfEval = EvalNODel;
    //aNotAction.pfEval = EvalNotRelaxed;
    

    bRelaxedModeNeg = 0;

/* create a new plan */
    SetZone(&zScratch);
    
    if (firstRunGoalDistances) {
	InitGoalDistance();
    }
    /* build dummy plan */
    newWorld = CopyWorld(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 
    WorldSignature(temporalPlan);
    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
	pWorldSummary=ComputeAllDistances(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    }

    ZoneClear(&zScratch);
    SetZone(saveZone);
      
    /* we compute the heuristic value */

    distance=0;
    numGoalsAchieved=numPreferencesAchieved=0;
    goalDistanceSum=0.0;
    preferenceDistanceSum=0.0;
    metric=0.0;
    
    metric=pWorldSummary->dMetric;
    
    /*debugging */
    //printf("\n");

    last_metric=-1; // give it a value to avoid warning
    for (;pWorldSummary;pWorldSummary=pWorldSummary->pNext,distance++) {
	if (dGoalMultiplier!=0.0) {
	    goals = pWorldSummary->nGoalsSatisfied;
	    if (goals>numGoalsAchieved)
		goalDistanceSum += (goals - numGoalsAchieved)*pow(distance,dHeuristicExponent);
	    numGoalsAchieved = goals;
	}

	/*debugging */
	//printf(".%d",numGoalsAchieved);
	
	if (dPreferenceMultiplier!=0.0) {
	    preferences = pWorldSummary->nPreferencesSatisfied;
	    if (preferences>numPreferencesAchieved)
		preferenceDistanceSum += (preferences - numPreferencesAchieved)*pow(distance,dHeuristicExponent);
	    numPreferencesAchieved = preferences;
	}
	if (dDiscountedMetricMultiplier!=0.0) {

	  if (distance==0)
		discountedMetric=last_metric=pWorldSummary->dMetric;
	    else {
	      fprintf(HandleToFileStream(3),"WorldSumm Metrc: %lf",pWorldSummary->dMetric);
	      fprintf(HandleToFileStream(3),"Last Metrc: %lf",last_metric);
	      discountedMetric+= (pWorldSummary->dMetric-last_metric)*pow(dMetricDiscountFactor,distance-1);
	      last_metric=pWorldSummary->dMetric;
	    }
	     fprintf(HandleToFileStream(3),"dm(%d): %lf\n",distance,discountedMetric);
	}
	if (best_metric>pWorldSummary->dMetric)
	    best_metric = pWorldSummary->dMetric;
    }



    if (totalGoals > numGoalsAchieved) /* punish this state-- does not satisfy the goal */
	goalDistanceSum += (totalGoals - numGoalsAchieved) * 1e20;

    /* debugging */
    //    printf("=%lf",goalDistanceSum);
    
    // don't count the preferences that are out of the reach
    // this fact is already inside optimistic-metric
//    if (dPreferenceMultiplier!=0.0)
//	preferenceDistanceSum += (totalPreferences - numPreferencesAchieved) * pow(DISTANCE_LIMIT,dHeuristicExponent);
    
//    fprintf(stderr, " %.2lf/%.2lf/%d", goalDistanceSum,best_metric,distance-1);

    
    /* compute the optimistic-metric of this world */
    /* now best_metric is our optimistic-metric! */
    if (dOptimisticMetricMultiplier!=0.0 && pcOptimisticMetricFn) {
	FormulaToDouble(pcOptimisticMetricFn,plpLinearPlan,pbBindings,&optimisticMetric);
    }

    plpLinearPlan->dfBestRelMetric = best_metric;

     /* ModifyWorld back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    
    goalDistanceSum *= decimalsToSave;
    preferenceDistanceSum *= decimalsToSave;

    pc=MakeFloatForm(dGoalMultiplier*floor(goalDistanceSum)+
		     dPreferenceMultiplier*floor(preferenceDistanceSum)+
		     dDiscountedMetricMultiplier*floor(discountedMetric)+
		     dMetricMultiplier*floor(metric)+
		     dBestMetricMultiplier*floor(best_metric)+
		     dOptimisticMetricMultiplier*floor(optimisticMetric));
    EXIT("ComputeGoalDistance_CRC");
    return pc;
} 



CELLP ComputeGoalDistance_CRC_Negations
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
) 
{
    CELLP pc;
    BTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    WORLDSUMP pWorldSummary;
    const int DISTANCE_LIMIT = 100;
    double best_metric=100000.0;
    double discountedMetric=100000.0;
    double metric;
    int goals, preferences;
    int numGoalsAchieved,numPreferencesAchieved;
    int distance;
    const int decimalsToSave = pow(10, 3);
    double goalDistanceSum,preferenceDistanceSum;
    double optimisticMetric;
    ZONE *saveZone = pzCurrent;

    ENTER("ComputeGoalDistance_CRC",TRUE);

        
    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

    bUnderNot = 0;
    bRelaxedModeNeg = 1;
//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;
    
/* create a new plan */
    SetZone(&zScratch);
    
    if (firstRunGoalDistances) {
	InitGoalDistance();
    }
    /* build dummy plan */
    newWorld = CopyWorld(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 
    WorldSignature(temporalPlan);

    apbtNegWorld = MakeWorld(); /* create a new empty world for negative facts */

    plpNegPlan = MakeWorldAction(apbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
	pWorldSummary=ComputeAllDistances(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    }

    ZoneClear(&zScratch);
    SetZone(saveZone);
      
    /* we compute the heuristic value */

    distance=0;
    numGoalsAchieved=numPreferencesAchieved=0;
    goalDistanceSum=0.0;
    preferenceDistanceSum=0.0;
    metric=0.0;
    
    metric=pWorldSummary->dMetric;

     /*debugging */
    printf("\n");

    for (;pWorldSummary;pWorldSummary=pWorldSummary->pNext,distance++) {
	double last_metric=-1; // give it a value to avoid warning
	if (dGoalMultiplier!=0.0) {
	    goals = pWorldSummary->nGoalsSatisfied;
	    if (goals>numGoalsAchieved)
		goalDistanceSum += (goals - numGoalsAchieved)*pow(distance,dHeuristicExponent);
	    numGoalsAchieved = goals;
	}

	/* debugging */
	printf(".%d",numGoalsAchieved); 

	if (dPreferenceMultiplier!=0.0) {
	    preferences = pWorldSummary->nPreferencesSatisfied;
	    if (preferences>numPreferencesAchieved)
		preferenceDistanceSum += (preferences - numPreferencesAchieved)*pow(distance,dHeuristicExponent);
	    numPreferencesAchieved = preferences;
	}
	if (dDiscountedMetricMultiplier!=0.0) {
	    if (distance==0)
		discountedMetric=last_metric=pWorldSummary->dMetric;
	    else {
		discountedMetric+=(pWorldSummary->dMetric-last_metric)*pow(dMetricDiscountFactor,distance-1);
		last_metric=pWorldSummary->dMetric;
	    }
	}
	if (best_metric>pWorldSummary->dMetric)
	    best_metric = pWorldSummary->dMetric;
    }

    if (totalGoals > numGoalsAchieved) /* punish this state-- does not satisfy the goal */
	goalDistanceSum += (totalGoals - numGoalsAchieved) * 1e20;

    /* debugging */
    printf("=%lf",goalDistanceSum);
    
    // don't count the preferences that are out of the reach
    // this fact is already inside optimistic-metric
//    if (dPreferenceMultiplier!=0.0)
//	preferenceDistanceSum += (totalPreferences - numPreferencesAchieved) * pow(DISTANCE_LIMIT,dHeuristicExponent);
    
//    fprintf(stderr, " %.2lf/%.2lf/%d", goalDistanceSum,best_metric,distance-1);

    
    /* compute the optimistic-metric of this world */
    /* now best_metric is our optimistic-metric! */
    if (dOptimisticMetricMultiplier!=0.0 && pcOptimisticMetricFn) {
	FormulaToDouble(pcOptimisticMetricFn,plpLinearPlan,pbBindings,&optimisticMetric);
    }

    plpLinearPlan->dfBestRelMetric = best_metric;

     /* ModifyWorld back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    
    goalDistanceSum *= decimalsToSave;
    preferenceDistanceSum *= decimalsToSave;

    pc=MakeFloatForm(dGoalMultiplier*floor(goalDistanceSum)+
		     dPreferenceMultiplier*floor(preferenceDistanceSum)+
		     dDiscountedMetricMultiplier*floor(discountedMetric)+
		     dMetricMultiplier*floor(metric)+
		     dBestMetricMultiplier*floor(best_metric)+
		     dOptimisticMetricMultiplier*floor(optimisticMetric));

    bRelaxedModeNeg = 0;

    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
} 

CELLP ComputeGoalDistance
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
    
    return ComputeGoalDistance_CRC(pcFormula,plpLinearPlan,pbBindings);
//    return ComputeGoalDistance_CRC_Negations(pcFormula,plpLinearPlan,pbBindings);
//  return ComputeGoalDistance_hsp(pcFormula,plpLinearPlan,pbBindings);
//  return ComputeGoalPrefDistance(pcFormula,plpLinearPlan,pbBindings);
//  return ComputeGoalDistance_hsp_crc(pcFormula,plpLinearPlan,pbBindings);
}

CELLP ComputeGoalDistanceNeg
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{

    return ComputeGoalDistance_CRC(pcFormula,plpLinearPlan,pbBindings);
  //    return ComputeGoalDistance_CRC_Negations(pcFormula,plpLinearPlan,pbBindings);


//  return ComputeGoalDistance_hsp(pcFormula,plpLinearPlan,pbBindings);
//  return ComputeGoalPrefDistance(pcFormula,plpLinearPlan,pbBindings);
//  return ComputeGoalDistance_hsp_crc(pcFormula,plpLinearPlan,pbBindings);
}




/*************************************************/
/*       Qualitative Preference Stuff            */
/*************************************************/

int nQualPreferenceNumber=0;



/* int QualPreferenceValue(LINEARPLANP plan,BINDINGP pbBindings) { */
/*     int goals = 0; */
/*     if (!pcQualpreferenceValueFn) return 0; /\* there's no preference counting formula *\/ */
    
/*     FormulaToInteger(pcQualpreferenceValueFn,plan,pbBindings,&goals); */
/*     return goals; */
/* } */


int QualPreferenceValue(LINEARPLANP plan,BINDINGP pbBindings) {
  CELLP pcMod;
  int value=0;
  
  for (value=nQualPreferenceNumber,pcMod = pcBDF; pcMod; pcMod=pcMod->pcNext,value--) {
    if ((*pcMod->pfForm->paAction->pfEval)(pcMod,plan,pbBindings)) break;
  }
  return value;
}



WORLDSUMP ComputeAllDistances_Qual
(
    LINEARPLANP temporalPlan, 
    BINDINGP pbBindings,
    int nDepth,         
    int nDepthLimit
) 
{
    int goals;
    int preferences;
    double metric;
    unsigned int worldCRC;
    int a;
//    int stopCondition;
    OPERATORP po;
    WORLDSUMP pWorldSummary;
    WORLDSUMP pNextWorldSummary;
    CELLP pcMod;
    BOOL stopCondition;

    ENTER("ComputeAllGoalDistances",TRUE);

    if (nDepth==nDepthLimit) return NULL; /* there's no next world */
    
    /* the current world is not in the hash table, we add it */

    goals=preferences=0;
    metric=0.0;
    goals = NumberOfGoalsSatisfied(temporalPlan,pbBindings);
    
    /*if (dPreferenceMultiplier!=0.0) {*/
    preferences = QualPreferenceValue(temporalPlan,pbBindings);
	/*}*/

    /*
    FormulaToDouble(pcMetricFn,temporalPlan,pbBindings,&metric);
    */

    worldCRC = temporalPlan->nSignature; // the signature was computed in the previous recursion
    pWorldSummary = RelCRCHashInsert_Qual(worldCRC,goals,preferences);


    /* for debugging, print the current world in the log file */
    /*
      if (nDepth == 0) {
      fprintf(HandleToFileStream(3),"World at depth %d (%u):",nDepth,temporalPlan->nSignature);
      PrintWorld(3,temporalPlan,pbBindings);
      }
    */

    /* compute the stop condition for the recursion */

    
    stopCondition=1;
    stopCondition&=(goals>=totalGoals);
    stopCondition&=(preferences>=nQualPreferenceNumber);
    
    if (stopCondition) {  // we've satisfied all the goals/preferences
	pWorldSummary->pNext=NULL;
	return pWorldSummary;
    }

    /* we compute the successor world */
    for(po=poOperators;po;po=po->poNext)
      (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
    a = AddsFromUpdateWorld();
    
    for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	(*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	a+=AddsFromUpdateWorld();
    }

    /* for debugging, print the current world in the log file */

    
    if(a < 1) {  /* we have reached a fixed point */
	pWorldSummary->pNext = NULL; /* no next world */
	return pWorldSummary;
    }
    /*    WorldSignature(temporalPlan);*/ 
    /* this should be CHECKED! Adding these is not the best thing to do */ 
    WorldSignature(temporalPlan);WorldSignature(plpNegPlan);
    temporalPlan->nSignature+=plpNegPlan->nSignature;

    /* debugging */

    /*
    fprintf(HandleToFileStream(3),"World at depth %d: (%u)",nDepth,temporalPlan->nSignature);
    PrintWorld(3,temporalPlan,pbBindings);
    */

    /*
    if (bRelaxedModeNeg) {
	fprintf(HandleToFileStream(3),"Negated world at depth %d: (%u)",nDepth,temporalPlan->nSignature);
	PrintWorld(3,plpNegPlan,pbBindings);
    }
    */


    //  fprintf(pfJBCRC,"%u\n",temporalPlan->nSignature);
    if (! (pNextWorldSummary = RelCRCHashSearch(temporalPlan->nSignature)))
	pNextWorldSummary=ComputeAllDistances_Qual(temporalPlan,pbBindings,nDepth+1,nDepthLimit);
    
    pWorldSummary->pNext = pNextWorldSummary; // linking the list

    EXIT("ComputeAllDistances_Qual");
    return pWorldSummary;
}





CELLP ComputeGoalDistance_CRC_Negations_Qualprefs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
) 
{
    CELLP pc;
    BTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    WORLDSUMP pWorldSummary;
    const int DISTANCE_LIMIT = 100;
    double best_preference_value=-1;   /* best_metric now contains the best value... */      /*    double discountedMetric=100000.0; */
    /* double metric; */
    int goals;
    int numGoalsAchieved,numPreferencesAchieved;
    int distance;
    double goalDistanceSum;
    ZONE *saveZone = pzCurrent;
    int i;

    ENTER("ComputeGoalDistance_CRC_Negations_Qualprefs",TRUE);

        
    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

    

    bUnderNot = 0;
    bRelaxedModeNeg = 1;
    //    EvalDescPredicate = (EVALP)EvalDescPredicateRelaxedNeg;
//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;
    
/* create a new plan */
    SetZone(&zScratch);
    
    if (firstRunGoalDistances) {
	InitGoalDistance();
    }
    /* build dummy plan */
    newWorld = CopyWorld(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 


    apbtNegWorld = MakeWorld(); /* create a new empty world for negative facts */
    
    
    plpNegPlan = MakeWorldAction(apbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    WorldSignature(temporalPlan);WorldSignature(plpNegPlan);
    temporalPlan->nSignature+=plpNegPlan->nSignature;
    

    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
	pWorldSummary=ComputeAllDistances_Qual(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    }

    ZoneClear(&zScratch);
    SetZone(saveZone);
      
    /* we compute the heuristic value */

    distance=0;
    numGoalsAchieved=numPreferencesAchieved=0;
    goalDistanceSum=0.0;

     /*debugging */
    /* printf("\n");*/

    /* Initialize  the array of distances to values */

    plpLinearPlan->afDistancesToPrefValues = (double*)ZoneAlloc((1+nQualPreferenceNumber)*sizeof(double));
    for (i=0;i<=nQualPreferenceNumber; i++) 
      plpLinearPlan->afDistancesToPrefValues[i]=INFTY;
    
    for (;pWorldSummary;pWorldSummary=pWorldSummary->pNext,distance++) {
      /*
	double last_metric=-1;*/ // give it a value to avoid warning
	/*	if (dGoalMultiplier!=0.0) { */
	goals = pWorldSummary->nGoalsSatisfied;
	if (goals>numGoalsAchieved)
	  goalDistanceSum += (goals - numGoalsAchieved)*pow(distance,dHeuristicExponent);
	numGoalsAchieved = goals;
	/* } */

	/* debugging */
	/*printf(".[%u]%d/%d",pWorldSummary->worldCRC,numGoalsAchieved,pWorldSummary->nPreferenceValue); */

	/* store the distance in the vector */
	
	if (plpLinearPlan->afDistancesToPrefValues[pWorldSummary->nPreferenceValue] == INFTY) {
	  //	  printf("assigning [%d] to %d\n",pWorldSummary->nPreferenceValue,distance);
	  plpLinearPlan->afDistancesToPrefValues[pWorldSummary->nPreferenceValue] = distance;
	  
	  /* propagate down this value until we see something different from -1 */
	  for (i=pWorldSummary->nPreferenceValue-1;
	       i>=1&& plpLinearPlan->afDistancesToPrefValues[i]==INFTY; i--) 
	       plpLinearPlan->afDistancesToPrefValues[i]=distance;
	}
	
	if (best_preference_value<pWorldSummary->nPreferenceValue)
	    best_preference_value = pWorldSummary->nPreferenceValue;
	
    }

    if (totalGoals > numGoalsAchieved) /* punish this state-- does not satisfy the goal */
	goalDistanceSum += (totalGoals - numGoalsAchieved) * 1e20;

    /* debugging */
    /*printf("=%lf",goalDistanceSum);*/
    
    /* preference distance vector */
    /*printf("\n-->");
    for (i=1; i<=nQualPreferenceNumber; i++) 
    printf("/%.1lf",plpLinearPlan->afDistancesToPrefValues[i]);*/
    
   
    
    // don't count the preferences that are out of the reach
    // this fact is already inside optimistic-metric
    // if (dPreferenceMultiplier!=0.0)
    //	preferenceDistanceSum += (totalPreferences - numPreferencesAchieved) * pow(DISTANCE_LIMIT,dHeuristicExponent);
    
    //  fprintf(stderr, " %.2lf/%.2lf/%d", goalDistanceSum,best_metric,distance-1);

    
    /* compute the optimistic-metric of this world */
    /* now best_metric is our optimistic-metric! */
    /*
      if (dOptimisticMetricMultiplier!=0.0 && pcOptimisticMetricFn) {
      FormulaToDouble(pcOptimisticMetricFn,plpLinearPlan,pbBindings,&optimisticMetric);
      }
    */
    plpLinearPlan->dfBestPreferenceValue = best_preference_value;

    /*
    plpLinearPlan->dfBestRelMetric = best_metric;
    */

     /* ModifyWorld back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    
    /*    goalDistanceSum *= decimalsToSave;
	  preferenceDistanceSum *= decimalsToSave;*/
    /*
    pc=MakeFloatForm(dGoalMultiplier*floor(goalDistanceSum)+
		     dPreferenceMultiplier*floor(preferenceDistanceSum)+
		     dDiscountedMetricMultiplier*floor(discountedMetric)+
		     dMetricMultiplier*floor(metric)+
		     dBestMetricMultiplier*floor(best_metric)+
		     dOptimisticMetricMultiplier*floor(optimisticMetric)); */

    pc=MakeFloatForm(goalDistanceSum);

    bRelaxedModeNeg = 0;
    //    EvalDescPredicate = (EVALP)EvalDescPredicateStd;
    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
} 



BOOL modifying_subformula(CELLP pc) {
  CELLP pcMod;
  
  if (pc->pfForm->paAction == &aAddAction || pc->pfForm->paAction == &aDelAction) 
    return TRUE;
  
  for (pcMod = pc->pfForm->pcArgs; pcMod; pcMod=pcMod->pcNext) 
    if (modifying_subformula(pcMod)) return TRUE;

  return FALSE;
  
}


/* change_forall_actions(pfForm,paAction) changes the action of all forall subformulae
   of pcCell, that do not add anything to paAction */ 

static void change_nonadding_forall_actions(CELLP pc, ACTION *paAction) {
  CELLP pcMod;
  if (pc->pfForm->paAction == &aForAllAction) 
    if (!modifying_subformula(pc)) {
      pc->pfForm->paAction = paAction; /* modifying the action */
    
      printf("Modifying a forall action\n");
      
    }
  /* continue with the descendants, if any */

  for (pcMod = pc->pfForm->pcArgs; pcMod; pcMod=pcMod->pcNext) 
    change_nonadding_forall_actions(pcMod,paAction);
}


void InitGoalDistanceQual() /* ComputeGoalDistance calls this function in the first run */
{ 
  OPERATORP po;
/* setup the hash table */
    RelCRCHashCreate();
/* compute the number of goals and number of preferences */
    totalGoals=TotalNumberOfGoals();

    /* we do not use a totalPreferences any more */  
//    totalPreferences=TotalNumberOfPreferences();

    nQualPreferenceNumber=0;
    if (pcBDF) {  /* There is a BDF, we compute the number of constituents */
      CELLP pc;
      for (pc=pcBDF;pc;pc=pc->pcNext)
	++nQualPreferenceNumber;
    }
    
    firstRunGoalDistances=0;

    
    /* preprocess foralls that do not have an add or del as child are now modified
       not to reset the costs */
    
    for(po=poOperators;po;po=po->poNext) {
      change_nonadding_forall_actions(po->pcSuccessor, &aForAllActionNoReset);
    }
    
    for(po=poHOperators;po;po=po->poNext) {
      change_nonadding_forall_actions(po->pcSuccessor, &aForAllActionNoReset);
    }


}


BOOL bHSPIgnoreOperator; /* tells whether the operator currently beign evaluated should be ignored by the HSP heuristic */


CELLP ComputeGoalDistance_CRC_Negations_Qualprefs_HSP
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
) 
{
    CELLP pc;
    CELLP pcMod;
    HBTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    int best_preference_value=-1;   /* best_metric now contains the best value... */
    /*    double discountedMetric=100000.0; */
    /* double metric; */
    int goals, preferences;
    double goalDistanceSum;
    double goal_cost=0.0;
    int a;
    BOOL bRealActionPerformed;
    BOOL bChange;

    ZONE *saveZone = pzCurrent;
    int i;
    int nDepth;
    int nFixPointLimit=45; 


    ENTER("ComputeGoalDistance_CRC_Negations_Qualprefs",TRUE);

    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

   
    bUnderNot = 0;


    bUseHWorld = 1;

    aAddAction.pfEval = EvalAdd_H;
    aDelAction.pfEval = EvalDel_H;
    aForAllAction.pfEval = EvalForAll_H;
    aForAllActionNoReset.pfEval = EvalForAll_HNoReset;
    aExistsAction.pfEval = EvalExists_H;
    aExistsXAction.pfEval = EvalExistsX_H;


//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;
    

    if (firstRunGoalDistances) {
	InitGoalDistanceQual();
    }

    // distances to preferences:
    plpLinearPlan->afDistancesToPrefValues = (double*)ZoneAlloc((1+nQualPreferenceNumber)*sizeof(double));

/* create a new plan */
    SetZone(&zScratch);



    /* build dummy plan */
    newWorld = CopyWorld_H(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction_H(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 


    aphbtNegWorld = MakeWorld_H(); /* create a new empty world for negative facts */
    
    
    plpNegPlan = MakeWorldAction_H(aphbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    //    WorldSignature(temporalPlan);WorldSignature(plpNegPlan);
    //    temporalPlan->nSignature+=plpNegPlan->nSignature;
    

    //    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
    //	pWorldSummary=ComputeAllDistances_Qual(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    //    }


    
    nDepth=0;
    a=0;

    bChange= bRealActionPerformed=FALSE;
    

    nFixPoints = 0;

    
    /*    fprintf(HandleToFileStream(3),"World at depth %d:",nDepth);
	  PrintHWorld(3,temporalPlan,pbBindings);*/
    
    //   fprintf(stderr,"-");
    while (1) {
      OPERATORP po;
      int a1=0,a2=0;
      CELLP pcOldAddEnd,pcOldDelEnd; 


      goals = NumberOfGoalsSatisfied_H(temporalPlan,pbBindings,&goal_cost);
      preferences = QualPreferenceValue(temporalPlan,pbBindings);


      if (nDepth>0&&a==0) nFixPoints++;



      //      fprintf(HandleToFileStream(3),"World at depth %d(%d/%d/%d/%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,bChange,bRealActionPerformed,goals,totalGoals,(nDepth>0&&(!bChange||(a==0&&bRealActionPerformed))));
      fprintf(HandleToFileStream(3),"World at depth %d(%d/%d/%d/%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,bChange,bRealActionPerformed,goals,totalGoals,(nDepth>0&&((!bChange||(a==0&&nFixPoints>=nFixPointLimit)))));
	      PrintHWorld(3,temporalPlan,pbBindings);

       
      fprintf(HandleToFileStream(3),"Negated World at depth %d:\n",nDepth);
      PrintHWorld(3,plpNegPlan,pbBindings);
      

      
      /* end loop if all goals are satisfied and preference value is the maximum */

      if (best_preference_value<preferences)
	best_preference_value = preferences;
      
    
      if (goals>=totalGoals&&preferences>=nQualPreferenceNumber) break;


      if (nDepth>0&&(!bChange||(a==0&&nFixPoints>=nFixPointLimit))) break;

      //      if (nDepth>0&&(!bChange||(a==0&&bRealActionPerformed))) break;
     
      
      /* we compute the successor world */
      a=0;

      bChange=FALSE;
      bRealActionPerformed=FALSE;
      
      if (!poHOperators) { /* if there are no heuristic-specific operators 
			      use the domain operators */

	for(po=poOperators;po;po=po->poNext) {
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  pcOldAddEnd=pcAddEnd; pcOldDelEnd=pcDelEnd; 
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	  if (pcOldAddEnd!=pcAddEnd||pcOldDelEnd!=pcDelEnd) {
	    bChange=TRUE; /* some operator affected the relaxed world */
	    if (!po->bUnrelEff)  
	      bRealActionPerformed=TRUE;  /* a world-altering operator changed the relaxed world */
	  }
	    
	}
      } else {
	for(po=poHOperators;po;po=po->poNext) {
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  pcOldAddEnd=pcAddEnd; pcOldDelEnd=pcDelEnd; 
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	  if (pcOldAddEnd!=pcAddEnd||pcOldDelEnd!=pcDelEnd) {
	    bChange=TRUE; /* some operator affected the relaxed world */
	    if (!po->bUnrelEff)  
	      bRealActionPerformed=TRUE;  /* a world-altering operator changed the relaxed world */
	  }

	}
      }
      
      /* process the action costs where each action has cost 1 */

      //     fprintf(HandleToFileStream(3),"Processing operators -- \n");

      a1=a=AddsFromUpdateWorld_H(1.0);
      
      
      aIfThenElseAction.pfEval = EvalIfThenElse_RESET; 
       
       
      /* probably is best to do this with the and, and reset the cost only
	 if the initial cost was also 0 */
      for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	/* this does not work if the post action sequence contains nested ands */
	BOOL aux_bool;
	dEvalTotalCost = 0;

	//	if (*pcMod->pfForm->paAction->pfEval == EvalAnd) {  /* this is an and, we need to be careful not to add up costs */
	// for (pcMod1=pcMod->pfForm->pcArgs; pcMod1; pcMod1=pcMod1->pcNext) {
	//   (*pcMod1->pfForm->paAction->pfEval)(pcMod1,temporalPlan,pbBindings);
	//   dEvalTotalCost = 0;
	// }
	//} else {
	  	
	  aux_bool = (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	  
	  //}

	  
      //  (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);

	a2=AddsFromUpdateWorld_H(0.0);
	a+=a2;
      }
      aIfThenElseAction.pfEval = EvalIfThenElse; 
      
      ++nDepth;
      
      //      printf("d=%d,a=%d\n",nDepth,a);

    }


    //    fprintf(HandleToFileStream(3),"World at final depth %d:\n",nDepth);
    //PrintHWorld(3,temporalPlan,pbBindings);

    
    //    fprintf(stderr,"!%d",nDepth);


    // trivially assigning infinity to everything
    for (i=0;i<=nQualPreferenceNumber; i++) 
      plpLinearPlan->afDistancesToPrefValues[i]=INFTY;
    
    plpLinearPlan->afDistancesToPrefValues[0]=0; /* to achieve nothing is immediate */
    
    // now compute the costs to the preferences

    for (i=nQualPreferenceNumber, pcMod=pcBDF; pcMod; pcMod=pcMod->pcNext,i--) {
      dEvalTotalCost=0; /* setting evaluation cost to 0 */
      if ((*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings)) /* if the preference is true in the final world */
	plpLinearPlan->afDistancesToPrefValues[i]=dEvalTotalCost; 
    }	
    
    
    ZoneClear(&zScratch);
    SetZone(saveZone);
   
   
    if (goals<totalGoals) goal_cost = INFTY; /* penalizing when goals are not achieved */
    goalDistanceSum = goal_cost;
    

    /* set the best prefernce value */
    plpLinearPlan->dfBestPreferenceValue = best_preference_value;
   

    //    fprintf(stderr,"\n=%.1lf",goalDistanceSum); 
    /*    fprintf(HandleToFileStream(3),"\n!!%.1lf>%d>",goalDistanceSum,best_preference_value); */
/*    for (i=0;i<=nQualPreferenceNumber; i++)
    printf("%.1lf/",plpLinearPlan->afDistancesToPrefValues[i]); 
    */

 
    /* goalDistanceSum = CostOfGoal */
    /* Iterate over preferences to compute their cost */


    
    /*    goalDistanceSum *= decimalsToSave;
	  preferenceDistanceSum *= decimalsToSave;*/
    /*
    pc=MakeFloatForm(dGoalMultiplier*floor(goalDistanceSum)+
		     dPreferenceMultiplier*floor(preferenceDistanceSum)+
		     dDiscountedMetricMultiplier*floor(discountedMetric)+
		     dMetricMultiplier*floor(metric)+
		     dBestMetricMultiplier*floor(best_metric)+
		     dOptimisticMetricMultiplier*floor(optimisticMetric)); */

    pc=MakeFloatForm(goalDistanceSum);

    bRelaxedModeNeg = 0;


     /* All functions back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aAddAction.pfEval = EvalAdd;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    aForAllAction.pfEval = EvalForAll;
    aForAllActionNoReset.pfEval = EvalForAll;
    aExistsAction.pfEval = EvalExists;
    aExistsXAction.pfEval = EvalExistsX;


    bUseHWorld = 0;


    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
}


int nFixPoints;
int nRealFixPoints;


CELLP ComputeGoalDistance_CRC_Negations_Qualprefs_HSP_unreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
) 
{
    CELLP pc;
    CELLP pcMod;
    HBTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    int best_preference_value=-1;   /* best_metric now contains the best value... */
    /*    double discountedMetric=100000.0; */
    /* double metric; */
    int goals, preferences;
    double goalDistanceSum;
    double goal_cost=0.0;
    int a;
    BOOL bRealActionPerformed;
    BOOL bChange;
    int act,actual_adds; /* actual adds */
    int a1=0,a2=0;
      

    ZONE *saveZone = pzCurrent;
    int i;
    int nDepth;

    int nFixPointLimit=45; 


    ENTER("ComputeGoalDistance_CRC_Negations_Qualprefs",TRUE);

    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

   
    bUnderNot = 0;


    bUseHWorld = 1;

    aAddAction.pfEval = EvalAdd_H;
    aDelAction.pfEval = EvalDel_H;
    aForAllAction.pfEval = EvalForAll_H;
    aForAllActionNoReset.pfEval = EvalForAll_HNoReset;
    aExistsAction.pfEval = EvalExists_H;
    aExistsXAction.pfEval = EvalExistsX_H;


//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;
    

    if (firstRunGoalDistances) {
	InitGoalDistanceQual();
    }

    // distances to preferences:
    plpLinearPlan->afDistancesToPrefValues = (double*)ZoneAlloc((1+nQualPreferenceNumber)*sizeof(double));

/* create a new plan */
    SetZone(&zScratch);



    /* build dummy plan */
    newWorld = CopyWorld_H(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction_H(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 


    aphbtNegWorld = MakeWorld_H(); /* create a new empty world for negative facts */
    
    
    plpNegPlan = MakeWorldAction_H(aphbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    //    WorldSignature(temporalPlan);WorldSignature(plpNegPlan);
    //    temporalPlan->nSignature+=plpNegPlan->nSignature;
    

    //    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
    //	pWorldSummary=ComputeAllDistances_Qual(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    //    }


        nDepth=0;
    a=0;

    bChange= bRealActionPerformed=FALSE;
    
    actual_adds=0;
    nFixPoints = 0;
    nRealFixPoints = 0;

    
    /*    fprintf(HandleToFileStream(3),"World at depth %d:",nDepth);
	  PrintHWorld(3,temporalPlan,pbBindings);*/
    
    //   fprintf(stderr,"-");
    while (1) {
      OPERATORP po;
      //      CELLP pcOldAddEnd,pcOldDelEnd; 


      goals = NumberOfGoalsSatisfied_H(temporalPlan,pbBindings,&goal_cost);
      preferences = QualPreferenceValue(temporalPlan,pbBindings);

      /* updating nFixPoints and nRealFixPoints */

      if (nDepth>0&&a==0) nFixPoints++;  
      if (a>0) nFixPoints=0;

      if (nDepth>0&&actual_adds==0) nRealFixPoints++;
      if (actual_adds>0) nRealFixPoints=0;

      //      fprintf(HandleToFileStream(3),"World at depth %d(%d/%d/%d/%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,bChange,bRealActionPerformed,goals,totalGoals,(nDepth>0&&(!bChange||(a==0&&bRealActionPerformed))));
    
      //  fprintf(HandleToFileStream(3),"World at depth %d(a1=%d/a2=%d/actual_adds=%d/nf=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));
      // if (njbNodeCounter==7||njbNodeCounter==17) 
      // PrintHWorld(3,temporalPlan,pbBindings);
		
       
      //	    fprintf(HandleToFileStream(3),"Negated World at depth %d:\n",nDepth);
	    //PrintHWorld(3,plpNegPlan,pbBindings);
      

      
      /* end loop if all goals are satisfied and preference value is the maximum */

      if (best_preference_value<preferences)
	best_preference_value = preferences;
      
    
      if (goals>=totalGoals&&preferences>=nQualPreferenceNumber) break;


      //      if (nDepth>0&&(actual_adds==0||(a==0&&nFixPoints>=nFixPointLimit))) break;

      if (nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))) break;

      //      if (nDepth>0&&(!bChange||(a==0&&bRealActionPerformed))) break;
     
      
      /* we compute the successor world */
      a=0;

      bChange=FALSE;
      bRealActionPerformed=FALSE;
      
      if (!poHOperators) { /* if there are no heuristic-specific operators 
			      use the domain operators */

	for(po=poOperators;po;po=po->poNext) {
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	}
      } else {
	for(po=poHOperators;po;po=po->poNext) {
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	}
      }
      
      /* process the action costs where each action has cost 1 */

      //     fprintf(HandleToFileStream(3),"Processing operators -- \n");

      a1=a=AddsFromUpdateWorld_H_unreleffs(1.0,&act);
      actual_adds=act;
      
      aIfThenElseAction.pfEval = EvalIfThenElse_RESET; 
       
       
      /* probably is best to do this with the and, and reset the cost only
	 if the initial cost was also 0 */
      for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	/* this does not work if the post action sequence contains nested ands */
	BOOL aux_bool;
	dEvalTotalCost = 0;

	//	if (*pcMod->pfForm->paAction->pfEval == EvalAnd) {  /* this is an and, we need to be careful not to add up costs */
	// for (pcMod1=pcMod->pfForm->pcArgs; pcMod1; pcMod1=pcMod1->pcNext) {
	//   (*pcMod1->pfForm->paAction->pfEval)(pcMod1,temporalPlan,pbBindings);
	//   dEvalTotalCost = 0;
	// }
	//} else {
	  	
	  aux_bool = (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	  
	  //}

	  
      //  (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);

	  a2=AddsFromUpdateWorld_H_unreleffs(0.0,&act);
	  a+=a2;
	  actual_adds+=act;
      }
      aIfThenElseAction.pfEval = EvalIfThenElse; 
      
      ++nDepth;
      
      //      printf("d=%d,a=%d\n",nDepth,a);

    }


    fprintf(HandleToFileStream(3),"Final depth %d(a1=%d/a2=%d/actual_adds=%d/nf=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));

    //    fprintf(HandleToFileStream(3),"World at final depth %d:\n",nDepth);
    //PrintHWorld(3,temporalPlan,pbBindings);

    
    //    fprintf(stderr,"!%d",nDepth);


    // trivially assigning infinity to everything
    for (i=0;i<=nQualPreferenceNumber; i++) 
      plpLinearPlan->afDistancesToPrefValues[i]=INFTY;
    
    plpLinearPlan->afDistancesToPrefValues[0]=0; /* to achieve nothing is immediate */
    
    // now compute the costs to the preferences

    for (i=nQualPreferenceNumber, pcMod=pcBDF; pcMod; pcMod=pcMod->pcNext,i--) {
      dEvalTotalCost=0; /* setting evaluation cost to 0 */
      if ((*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings)) /* if the preference is true in the final world */
	plpLinearPlan->afDistancesToPrefValues[i]=dEvalTotalCost; 
    }	
    
    
    ZoneClear(&zScratch);
    SetZone(saveZone);
   
   
    if (goals<totalGoals) goal_cost = INFTY; /* penalizing when goals are not achieved */
    goalDistanceSum = goal_cost;
    

    /* set the best prefernce value */
    plpLinearPlan->dfBestPreferenceValue = best_preference_value;
   

    //    fprintf(stderr,"\n=%.1lf",goalDistanceSum); 
    /*    fprintf(HandleToFileStream(3),"\n!!%.1lf>%d>",goalDistanceSum,best_preference_value); */
/*    for (i=0;i<=nQualPreferenceNumber; i++)
    printf("%.1lf/",plpLinearPlan->afDistancesToPrefValues[i]); 
    */

 
    /* goalDistanceSum = CostOfGoal */
    /* Iterate over preferences to compute their cost */


    
    /*    goalDistanceSum *= decimalsToSave;
	  preferenceDistanceSum *= decimalsToSave;*/
    /*
    pc=MakeFloatForm(dGoalMultiplier*floor(goalDistanceSum)+
		     dPreferenceMultiplier*floor(preferenceDistanceSum)+
		     dDiscountedMetricMultiplier*floor(discountedMetric)+
		     dMetricMultiplier*floor(metric)+
		     dBestMetricMultiplier*floor(best_metric)+
		     dOptimisticMetricMultiplier*floor(optimisticMetric)); */

    pc=MakeFloatForm(goalDistanceSum);

    bRelaxedModeNeg = 0;


     /* All functions back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aAddAction.pfEval = EvalAdd;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    aForAllAction.pfEval = EvalForAll;
    aForAllActionNoReset.pfEval = EvalForAll;
    aExistsAction.pfEval = EvalExists;
    aExistsXAction.pfEval = EvalExistsX;


    bUseHWorld = 0;


    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
}

static LISTP * pplActionInstances;
static LISTP * pplIdealActionInstances;
static int nFFActionInstanceCounter;
static LISTP plHelpfulActions;
static LISTP * pplRelaxedPlan;
static LISTP * pplIdealRelaxedPlan;
static int * anIdealMutexPenalties;
static BOOL bStoreRelaxedPlan=1; // whether or not relaxed plan will be stored

static LISTP AddToHelpful
(
 LISTP plList,
 int nOperNumber, 
 int nOperInstance,
 BINDINGP pbActionBindings,
 BOOL bPermanent
)
{
  ZONE *saveZone = pzCurrent;
  
  if (!plList || 
      plList->uValue.poiOperInstance->nOperNumber > nOperNumber ||
      (plList->uValue.poiOperInstance->nOperNumber == nOperNumber &&
       plList->uValue.poiOperInstance->nOperInstance > nOperInstance)) {
    LISTP plNewNode;
    OPERINSTANCEP pffhAction;

    if (bPermanent) SetZone(&zPermanent);
    pffhAction=(OPERINSTANCEP)MemAlloc(sizeof(OPERINSTANCE));
    pffhAction->nOperNumber=nOperNumber;
    pffhAction->nOperInstance=nOperInstance;
    pffhAction->pbBindings=pbActionBindings;
    plNewNode=(LISTP)MemAlloc(sizeof(struct List));
    plNewNode->uValue.poiOperInstance=pffhAction;
    plNewNode->plNext=plList;
    if (bPermanent) SetZone(saveZone);

    return plNewNode;

  } else {
    plList->plNext = AddToHelpful(plList->plNext,nOperNumber,nOperInstance,pbActionBindings,bPermanent);
    return plList;
  }
}


static LISTP AddToHelpful2
(
 LISTP plList,
 OPERINSTANCEP poiOperInstance,
 BOOL bPermanent
)
{
  ZONE *saveZone = pzCurrent;  
  int nOperNumber=poiOperInstance->nOperNumber;
  int nOperInstance=poiOperInstance->nOperInstance;
    
  if (!plList || 
      plList->uValue.poiOperInstance->nOperNumber > nOperNumber ||
      (plList->uValue.poiOperInstance->nOperNumber == nOperNumber &&
       plList->uValue.poiOperInstance->nOperInstance > nOperInstance)) {
    LISTP plNewNode;
    OPERINSTANCEP pffhAction;

    if (bPermanent) SetZone(&zPermanent);
    
    if (bPermanent) {
      pffhAction=(OPERINSTANCEP)MemAlloc(sizeof(OPERINSTANCE));
      memcpy(pffhAction,poiOperInstance,sizeof(OPERINSTANCE));
    } else {
      pffhAction=poiOperInstance;
    }
    plNewNode=(LISTP)MemAlloc(sizeof(struct List));
    plNewNode->uValue.poiOperInstance=pffhAction;
    plNewNode->plNext=plList;
    if (bPermanent) SetZone(saveZone);

    return plNewNode;

  } else {
    plList->plNext = AddToHelpful2(plList->plNext,poiOperInstance,bPermanent);
    return plList;
  }
}



static HBTREEP complementNode
(
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 HBTREEP aphbtNode
)
{
  HBTREEP targetTree=NULL;
  HBTREEP complementNode;

  
  if (aphbtNode->aphbtComplement) return aphbtNode->aphbtComplement;
  
  if (aphbtNode->apbtOwner==plpPositivePlan->aphbtWorld) {
    targetTree=plpNegativePlan->aphbtWorld[aphbtNode->nIndex];
  } else if (aphbtNode->apbtOwner==plpNegativePlan->aphbtWorld) {
    targetTree=plpPositivePlan->aphbtWorld[aphbtNode->nIndex];
  }

  complementNode=HBTreeSearch(aphbtNode->pcKey,targetTree);

  // caching the complement
  if (complementNode) {
    complementNode->aphbtComplement=aphbtNode;
    aphbtNode->aphbtComplement=complementNode;
  }
  
  return complementNode;
  
  
}


static OPERINSTANCEP RelPlanSelectOperator_IDEAL 
// "IDEAL plan" version
// standard, discounts if an alternative, more mutex free plan could have been constructed
(
 HBTREEP phbtTree,
 LISTP *pplActionList,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepthLimit
)
{
  OPERINSTANCEP poiBestCandidate,poiDefaultInstance;
  int minMutexes;
  int mutexes;
  LISTP plActions,plSupFacts,plRelPlanActions,plCompAddActions;
  int nDepth;

  poiDefaultInstance=NULL; // avoid warning

  if (phbtTree->poiOperInstance->nDepth<=nDepthLimit) {
    poiDefaultInstance=phbtTree->poiOperInstance;
  } else {
    LISTP pl;
    for (pl=phbtTree->plActionList; pl; pl=pl->plNext) {
      if (pl->uValue.poiOperInstance->nDepth<=nDepthLimit) {
        poiDefaultInstance=pl->uValue.poiOperInstance;
	break;
      }
    }
  }
  
  nDepth=poiDefaultInstance->nDepth;  // this is the depth we are working at
     
  if (pplRelaxedPlan[nDepth-1]!=NULL) {
    
    minMutexes=100000; poiBestCandidate=NULL;
    for (plActions=phbtTree->plActionList; 
	 plActions; 
	 plActions=plActions->plNext) { 
      
      mutexes=0;
      
      if (plActions->uValue.poiOperInstance->nDepth!=nDepth) continue;
      
      for (plSupFacts=plActions->uValue.poiOperInstance->pcSupFacts;   // looping over facts required
	   plSupFacts; 
	   plSupFacts=plSupFacts->plNext) {
	HBTREEP pbhtReqNode=plSupFacts->uValue.phbtTree;
	HBTREEP pbhtCompNode=complementNode(plpPositivePlan, 
					    plpNegativePlan, 
					    pbhtReqNode);
	
	if (!pbhtCompNode) continue; // the complement was not added by any action, nothing to add
	//	if (pbhtReqNode->nIndex!=0) continue;

	for (plRelPlanActions=pplIdealRelaxedPlan[nDepth-1];  // looping over actions in the relaxed plan
	     plRelPlanActions; 
	     plRelPlanActions=plRelPlanActions->plNext) {
	  
	  for (plCompAddActions=pbhtCompNode->plActionList; 
	       plCompAddActions;
	       plCompAddActions=plCompAddActions->plNext) {
	    if (plCompAddActions->uValue.poiOperInstance->nOperNumber==
		plRelPlanActions->uValue.poiOperInstance->nOperNumber &&
		plCompAddActions->uValue.poiOperInstance->nOperInstance==
		plRelPlanActions->uValue.poiOperInstance->nOperInstance &&
		plCompAddActions->uValue.poiOperInstance->nDepth==nDepth) {
	      
	      if (!(plCompAddActions->uValue.poiOperInstance->nOperNumber==
		    plActions->uValue.poiOperInstance->nOperNumber &&
		    plCompAddActions->uValue.poiOperInstance->nOperInstance==
		    plActions->uValue.poiOperInstance->nOperInstance))
		++mutexes;  
	    
	  }	  
	  }	
	}
	
	
      }
      if (mutexes<minMutexes) {
	minMutexes=mutexes;
	poiBestCandidate=plActions->uValue.poiOperInstance;
      }
    }
  } else {
    minMutexes=0; // previously there was no action at this layer
    poiBestCandidate=poiDefaultInstance;
  }
  
  //  printf("Going from %d to ",anIdealMutexPenalties[nDepth-1]);
  anIdealMutexPenalties[nDepth-1]+=minMutexes;
  //  printf("%d\n ",anIdealMutexPenalties[nDepth-1]);
  // BEGIN action addition to ideal relaxed plan

  pplIdealRelaxedPlan[poiBestCandidate->nDepth-1]=AddToHelpful(pplIdealRelaxedPlan[poiBestCandidate->nDepth-1],
							       poiBestCandidate->nOperNumber,
							       poiBestCandidate->nOperInstance,
							       poiBestCandidate->pbBindings,
							       0);
  
    
  // END action addition to ideal relaxed plan


  fprintf(HandleToFileStream(3),"Would return at depth %d (%d,%d) [m=%d]:",poiBestCandidate->nDepth,poiBestCandidate->nOperNumber,poiBestCandidate->nOperInstance,minMutexes);
  PrintFormula(HandleToFileStream(3),poiBestCandidate->pcActionInstance,0);

  // not returning ideal but default default instance

  return poiDefaultInstance;
}



static OPERINSTANCEP RelPlanSelectOperator_MIN
(
 HBTREEP phbtTree,
 LISTP *pplActionList,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepthLimit
)
{
  OPERINSTANCEP poiBestCandidate;
  int minMutexes;
  int mutexes;
  LISTP plActions,plSupFacts,plRelPlanActions,plCompAddActions;
  int nDepth;
  OPERINSTANCEP poiDefaultInstance;
  
  poiDefaultInstance=NULL; // avoid warning
  
  if (phbtTree->poiOperInstance->nDepth<=nDepthLimit) {
    poiDefaultInstance=phbtTree->poiOperInstance;
  } else {
    LISTP pl;
    for (pl=phbtTree->plActionList; pl; pl=pl->plNext) {
      if (pl->uValue.poiOperInstance->nDepth<=nDepthLimit) {
        poiDefaultInstance=pl->uValue.poiOperInstance;
	break;
      }
    }
  }

  nDepth=poiDefaultInstance->nDepth;  // this is the depth we are working at

  // no actions at this level? return default action
  if (pplRelaxedPlan[nDepth-1]==NULL)
    return poiDefaultInstance;
    
  minMutexes=100000; poiBestCandidate=NULL;
  for (plActions=phbtTree->plActionList; 
       plActions; 
       plActions=plActions->plNext) { 
    
    mutexes=0;

    if (plActions->uValue.poiOperInstance->nDepth!=nDepth) continue;

    for (plSupFacts=plActions->uValue.poiOperInstance->pcSupFacts;   // looping over facts required
	 plSupFacts; 
	 plSupFacts=plSupFacts->plNext) {
      HBTREEP pbhtReqNode=plSupFacts->uValue.phbtTree;
      HBTREEP pbhtCompNode=complementNode(plpPositivePlan, 
					  plpNegativePlan, 
					  pbhtReqNode);

      //      if (pbhtReqNode->nIndex!=0) continue; // only count mutexes on one fluent

      if (!pbhtCompNode) continue; // the complement was not added by any action, nothing to add
      
      for (plRelPlanActions=pplRelaxedPlan[poiDefaultInstance->nDepth-1];  // looping over actions in the relaxed plan
	   plRelPlanActions; 
	   plRelPlanActions=plRelPlanActions->plNext) {
	
	for (plCompAddActions=pbhtCompNode->plActionList; 
	     plCompAddActions;
	     plCompAddActions=plCompAddActions->plNext) {
	  if (plCompAddActions->uValue.poiOperInstance->nOperNumber==
	      plRelPlanActions->uValue.poiOperInstance->nOperNumber &&
	      plCompAddActions->uValue.poiOperInstance->nOperInstance==
	      plRelPlanActions->uValue.poiOperInstance->nOperInstance) {

	    if (!(plCompAddActions->uValue.poiOperInstance->nOperNumber==
		  plActions->uValue.poiOperInstance->nOperNumber &&
		  plCompAddActions->uValue.poiOperInstance->nOperInstance==
		  plActions->uValue.poiOperInstance->nOperInstance))
	      ++mutexes;  
	    
	  }	  
	}	
      }
    }



    if (mutexes<minMutexes) {
      minMutexes=mutexes;
      poiBestCandidate=plActions->uValue.poiOperInstance;
    }
  }

  anIdealMutexPenalties[nDepth-1]+=minMutexes;

  return poiBestCandidate;
}

static OPERINSTANCEP RelPlanSelectOperator_BASIC
(
 HBTREEP phbtTree,
 LISTP *pplActionList,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepthLimit
 
)
{
  if (phbtTree->poiOperInstance->nDepth<=nDepthLimit)
    return phbtTree->poiOperInstance;
  else { /* look for another action!! */
    LISTP pl;
    for (pl=phbtTree->plActionList; pl; pl=pl->plNext) {
      if (pl->uValue.poiOperInstance->nDepth<=nDepthLimit)
	return pl->uValue.poiOperInstance;
    }
  }
  printf("Relaxed Plan Extraction: No action could be found. This should not happen");
  return NULL;
	
}


static OPERINSTANCEP RelPlanSelectOperator_BASIC_backtrack
(
 HBTREEP phbtTree,
 LISTP *pplActionList,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepthLimit,
 BOOL bFirstTime
)
{
  static LISTP pl;

  if (bFirstTime) {  // we are not backtracking
    /* look for another action!! */
    for (pl=phbtTree->plActionList; pl; pl=pl->plNext) {
      if (pl->uValue.poiOperInstance->nDepth<=nDepthLimit) {
	//	fprintf(HandleToFileStream(3),"considering following action at depth %d, cost=%lf",pl->uValue.poiOperInstance->nDepth, pl->uValue.poiOperInstance->dHspCost);
	//	PrintFormula(HandleToFileStream(3),pl->uValue.poiOperInstance->pcActionInstance,0);
	//{
	//LISTP pl;
	//fprintf(HandleToFileStream(3),"Other actions achieving same objective:\n");
	// for (pl=phbtTree->plActionList; pl; pl=pl->plNext) {
	//   if (pl->uValue.poiOperInstance->nDepth<=nDepthLimit) {
	//      fprintf(HandleToFileStream(3),"action at depth %d, cost=%lf",pl->uValue.poiOperInstance->nDepth, pl->uValue.poiOperInstance->dHspCost);
	//      PrintFormula(HandleToFileStream(3),pl->uValue.poiOperInstance->pcActionInstance,0);
	//    }
	//  }
	//}
	return pl->uValue.poiOperInstance;
      }

      
    }
  } else if (pl) {
    // pl is already set
    for (pl=pl->plNext; pl; pl=pl->plNext) {
      if (pl->uValue.poiOperInstance->nDepth<=nDepthLimit) {
	//	fprintf(HandleToFileStream(3),"re-considering following action at depth %d, cost=%lf",pl->uValue.poiOperInstance->nDepth, pl->uValue.poiOperInstance->dHspCost);
	//PrintFormula(HandleToFileStream(3),pl->uValue.poiOperInstance->pcActionInstance,0);
	return pl->uValue.poiOperInstance;
      }
    }
  }
  
  // printf("Relaxed Plan Extraction FAILED: No action could be found pl=%p bFirst=%d, Target Depth=%d. Depths:\n",pl,bFirstTime,nDepthLimit);
  //for (pl=phbtTree->plActionList; pl; pl=pl->plNext) {
  //  printf("cost=%lf depth=%d\n",pl->uValue.poiOperInstance->dHspCost,pl->uValue.poiOperInstance->nDepth);
  // }
  
  
  //  fflush(stdout);
  // }
  return NULL;
}



static OPERINSTANCEP (*RelPlanSelectOperator)(HBTREEP,LISTP *,LINEARPLANP,LINEARPLANP,int)=RelPlanSelectOperator_BASIC; 
  

void RelaxedPlanFromNode_ORIG
(
 HBTREEP phbtTree,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepthLimit,
 BINDINGP pbBindings
) 
{
  int nIndex;
  LISTP plList;
  LISTP plNewCell;

  BOOL bFound;
  OPERINSTANCEP poiToRelPlan;
  
  if (phbtTree->poiOperInstance->nDepth==0) {
    //    fprintf(HandleToFileStream(3),"depth 0 by operator(%d,%d), returning on %p\n",phbtTree->poiOperInstance->nOperNumber,phbtTree->poiOperInstance->nOperInstance,phbtTree);
    return;
  }
  if (phbtTree->bFFSearched||phbtTree->bAddedByRelaxed) {
    // fprintf(HandleToFileStream(3),"already searched, returning on %p\n",phbtTree);
    return;
  }
  nIndex = phbtTree->poiOperInstance->nOperNumber;
  
  /* first look for the action instance */
  
  bFound=FALSE; /* has the instance that added this node been considered yet? */
  for (plList=pplActionInstances[nIndex]; plList; plList=plList->plNext) {
    if (plList->uValue.pffActionInstance->nFFOperInstance==phbtTree->poiOperInstance->nOperInstance &&
	plList->uValue.pffActionInstance->nFFDepth==phbtTree->poiOperInstance->nDepth) {

      bFound=TRUE;
      break;
    }  
  }

  if (!bFound) { /* we allocate a new instance */
    ++nFFActionInstanceCounter;   
    
    // first determine whether the operator has been used before in this level
    poiToRelPlan=(*RelPlanSelectOperator)(phbtTree,pplActionInstances,plpPositivePlan,plpNegativePlan,nDepthLimit);
    
    
    plNewCell=(LISTP)MemAlloc(sizeof(struct List));
    plNewCell->uValue.pffActionInstance=(struct FFActionInstance*)MemAlloc(sizeof(struct FFActionInstance));
    plNewCell->uValue.pffActionInstance->nFFOperInstance=poiToRelPlan->nOperInstance;
    plNewCell->uValue.pffActionInstance->nFFDepth=poiToRelPlan->nDepth;
    plNewCell->uValue.pffActionInstance->plSupFacts=poiToRelPlan->pcSupFacts;
    
    /* if this is a helpful action, add it to the list */
    if (!poHOperators && (poiToRelPlan->nDepth==1 || bStoreRelaxedPlan)) { 
      
      pplRelaxedPlan[poiToRelPlan->nDepth-1]=AddToHelpful(pplRelaxedPlan[poiToRelPlan->nDepth-1],
							  poiToRelPlan->nOperNumber,
							  poiToRelPlan->nOperInstance,
							  poiToRelPlan->pbBindings,
							  (poiToRelPlan->nDepth==1));
    }


    plNewCell->plNext=pplActionInstances[poiToRelPlan->nOperNumber];
    pplActionInstances[poiToRelPlan->nOperNumber]=plNewCell;
    
    if (bVerboseFFShowRelaxedPlans) {
      fprintf(HandleToFileStream(3),"At depth %d (%d,%d):",poiToRelPlan->nDepth,poiToRelPlan->nOperNumber,poiToRelPlan->nOperInstance);
      PrintFormula(HandleToFileStream(3),poiToRelPlan->pcActionInstance,0);
    }

  } else {
    if (bVerboseFFShowRelaxedPlans) {
      fprintf(HandleToFileStream(3),"Action (%d,%d) at depth %d already found in relaxed plan: ",phbtTree->poiOperInstance->nOperNumber,phbtTree->poiOperInstance->nOperInstance,phbtTree->poiOperInstance->nDepth);
      PrintFormula(HandleToFileStream(3),phbtTree->poiOperInstance->pcActionInstance,0);
    }
    poiToRelPlan=phbtTree->poiOperInstance;
  }
 
  
  /* Now recursive call for all supporting facts */
  
  for (plList=poiToRelPlan->pcSupFacts; plList; plList=plList->plNext) {
    //CELLP pc;
    // fprintf(HandleToFileStream(3),"Recursing on node %p ");
    // PrintHNode(plList->uValue.phbtTree);
    //fprintf(HandleToFileStream(3),"\n");
    //    fprintf(HandleToFileStream(3),"Recursing on node %p, with  arguments:",plList->uValue.phbtTree);
    //for (pc=plList->uValue.phbtTree->pcKey; pc; pc=pc->pcNext) {
    //  fprintf(HandleToFileStream(3),"%s,",IdentName(pc->pfForm));
    //}
    //    fprintf(HandleToFileStream(3),"\n");
    RelaxedPlanFromNode_ORIG(plList->uValue.phbtTree,plpPositivePlan,plpNegativePlan,poiToRelPlan->nDepth-1,pbBindings);
  }
  phbtTree->bFFSearched=TRUE;
  //  fprintf(HandleToFileStream(3),"returning on %p\n",phbtTree);
}


void undoInstanceRelPlan
(
 OPERINSTANCEP poiToRelPlan
)
{
  LISTP pl,plu,plum;
  for (plu=poiToRelPlan->plFlipUndo, 
	 plum=poiToRelPlan->plFlipUndoMin, 
	 pl=poiToRelPlan->plAddedFacts; 
       pl  ; 
       pl=pl->plNext,
	 plu=plu->plNext,
	 plum=plum->plNext) {
    pl->uValue.phbtTree->nDepthAddedByRelaxed=plu->uValue.nInteger;
    pl->uValue.phbtTree->nMinDepthAddedByRelaxed=plum->uValue.nInteger;
  }
}


void checkFlipVectors
(
 OPERINSTANCEP poiOperInstance
)   
{                 // create flip lists if necessary
  int i;
  LISTP newFlipUndo,newFlipUndoMin;
  if (!poiOperInstance->plFlipUndo) {
    for (i=0; i<poiOperInstance->nAddedFacts;i++) {
      newFlipUndo=(LISTP)MemAlloc(sizeof(LIST));
      newFlipUndo->uValue.nInteger=-1;
      newFlipUndo->plNext=poiOperInstance->plFlipUndo;
      poiOperInstance->plFlipUndo=newFlipUndo;
      
      newFlipUndoMin=(LISTP)MemAlloc(sizeof(LIST));
      newFlipUndoMin->uValue.nInteger=1000;
      newFlipUndoMin->plNext=poiOperInstance->plFlipUndoMin;
      poiOperInstance->plFlipUndoMin=newFlipUndoMin;
    }
  }
}

BOOL propertyIsPreserved
(
 OPERINSTANCEP poiToRelPlan,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plNegativePlan,
 BINDINGP pbBindings
)
{
  LISTP pl,plu,plum;
  BOOL bAddsWatched;
  BOOL b;

  checkFlipVectors(poiToRelPlan);

  bAddsWatched=FALSE;

  for (plu=poiToRelPlan->plFlipUndo,
	 plum=poiToRelPlan->plFlipUndoMin,
	 pl=poiToRelPlan->plAddedFacts; 
       pl  ; 
       pl=pl->plNext,plu=plu->plNext,plum=plum->plNext) {

      if (abWatchedPreserve[pl->uValue.phbtTree->nIndex] && pl->uValue.phbtTree->apbtOwner==plpPositivePlan->aphbtWorld) {
	bAddsWatched=TRUE;  /* only watch POSITIVE instances of watched predicates */
      }

      plu->uValue.nInteger=pl->uValue.phbtTree->nDepthAddedByRelaxed; // remember old value
      plum->uValue.nInteger=pl->uValue.phbtTree->nMinDepthAddedByRelaxed; // remember old value
      
      pl->uValue.phbtTree->nMinDepthAddedByRelaxed=MIN(pl->uValue.phbtTree->nMinDepthAddedByRelaxed, poiToRelPlan->nDepth);
      pl->uValue.phbtTree->nDepthAddedByRelaxed=MAX(pl->uValue.phbtTree->nDepthAddedByRelaxed, poiToRelPlan->nDepth);
      
    }
  
  if (!bAddsWatched) {
    //  fprintf(HandleToFileStream(3),"prop. not checked\n");
    return TRUE; /* no watched literals, no need to verify property */
  }

  bExtractingRelaxedPlan=FALSE; // change behaviour of EvalDescPredicate. only added fluents should be considered true
  bVerifyingPreservedProperty=TRUE;
  
  if (pcHPreserved)
    b=(*pcHPreserved->pfForm->paAction->pfEval)(pcHPreserved,plpPositivePlan,pbBindings);
  else {
    b=TRUE;  // assume property is satisfied
  }
  
  //  fprintf(HandleToFileStream(3),"Property evaluates to %d\n",b);

  bExtractingRelaxedPlan=TRUE; // restore behaviour of EvalDescPredicate
  bVerifyingPreservedProperty=FALSE;
  
  if (!b) { // this action does not preserve the property, we undo...
    undoInstanceRelPlan(poiToRelPlan);
  }
  
  return b;
  // first check whether there are any watched predicates in the added fluents
  
}

BOOL bDebugRelPlanExtraction=FALSE;

BOOL RelaxedPlanFromNode_BACKTRACK
(
 HBTREEP phbtTree,
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepthLimit,
 BINDINGP pbBindings
) 
{
  int nIndex;
  LISTP plList;
  LISTP plNewCell;

  BOOL bFound;
  OPERINSTANCEP poiToRelPlan;
  if (bDebugRelPlanExtraction)
    fprintf(HandleToFileStream(3),"min depth: %d\n",phbtTree->nMinDepthAddedByRelaxed);
  if (phbtTree->poiOperInstance->nDepth==0) {
    if (bDebugRelPlanExtraction) {
      PrintHNode(HandleToFileStream(3),phbtTree);
      fprintf(HandleToFileStream(3),"depth 0 by operator(%d,%d), returning on %p\n",phbtTree->poiOperInstance->nOperNumber,phbtTree->poiOperInstance->nOperInstance,phbtTree);
    }
    return TRUE;
  }
  if (//phbtTree->bFFSearched || 
      phbtTree->nMinDepthAddedByRelaxed<=nDepthLimit) {
    if (bDebugRelPlanExtraction) {
      PrintHNode(HandleToFileStream(3),phbtTree);
      fprintf(HandleToFileStream(3)," already true at level %d\n",phbtTree->nMinDepthAddedByRelaxed);
    }
    return TRUE;
  }
  nIndex = phbtTree->poiOperInstance->nOperNumber;

  /* first look for the action instance */
  
  bFound=FALSE; /* has the instance that added this node been considered yet? */
  for (plList=pplActionInstances[nIndex]; plList; plList=plList->plNext) {
    if (plList->uValue.pffActionInstance->nFFOperInstance==phbtTree->poiOperInstance->nOperInstance &&
	plList->uValue.pffActionInstance->nFFDepth==phbtTree->poiOperInstance->nDepth) {

      bFound=TRUE;
      break;
    }  
  }

  if (!bFound) { /* we allocate a new instance */
    ++nFFActionInstanceCounter;   
    
    // first determine whether the operator has been used before in this level

    // finding the first action (this does not fail)
    poiToRelPlan=RelPlanSelectOperator_BASIC_backtrack(phbtTree,pplActionInstances,plpPositivePlan,plpNegativePlan,nDepthLimit,1);
    
    if (!propertyIsPreserved(poiToRelPlan,plpPositivePlan,plpNegativePlan,pbBindings)) {
      
      while ( (poiToRelPlan=RelPlanSelectOperator_BASIC_backtrack(phbtTree,pplActionInstances,plpPositivePlan,plpNegativePlan,nDepthLimit,0)) ) {
	if (propertyIsPreserved(poiToRelPlan,plpPositivePlan,plpNegativePlan,pbBindings)) break; // loop until property is satisfied
      }
    }
    if (!poiToRelPlan) {
      return 0;
    }
    
    plNewCell=(LISTP)MemAlloc(sizeof(struct List));
    plNewCell->uValue.pffActionInstance=(struct FFActionInstance*)MemAlloc(sizeof(struct FFActionInstance));
    plNewCell->uValue.pffActionInstance->nFFOperInstance=poiToRelPlan->nOperInstance;
    plNewCell->uValue.pffActionInstance->nFFDepth=poiToRelPlan->nDepth;
    plNewCell->uValue.pffActionInstance->plSupFacts=poiToRelPlan->pcSupFacts;
    
    /* if this is a helpful action, add it to the list */
    if (!poHOperators && (poiToRelPlan->nDepth==1 || bStoreRelaxedPlan)) { 
      
      pplRelaxedPlan[poiToRelPlan->nDepth-1]=AddToHelpful2(pplRelaxedPlan[poiToRelPlan->nDepth-1],
							   poiToRelPlan,
							   (poiToRelPlan->nDepth==1));
    }


    plNewCell->plNext=pplActionInstances[poiToRelPlan->nOperNumber];
    pplActionInstances[poiToRelPlan->nOperNumber]=plNewCell;
    
    if (bVerboseFFShowRelaxedPlans) {
      fprintf(HandleToFileStream(3),"At depth %d (%d,%d):",poiToRelPlan->nDepth,poiToRelPlan->nOperNumber,poiToRelPlan->nOperInstance);
      PrintFormula(HandleToFileStream(3),poiToRelPlan->pcActionInstance,0);
    }

  } else {
    fprintf(HandleToFileStream(3),"Action (%d,%d) at depth %d already found in relaxed plan: ",phbtTree->poiOperInstance->nOperNumber,phbtTree->poiOperInstance->nOperInstance,phbtTree->poiOperInstance->nDepth);
    PrintFormula(HandleToFileStream(3),phbtTree->poiOperInstance->pcActionInstance,0);
    poiToRelPlan=phbtTree->poiOperInstance;
  }
 
  
  /* Now recursive call for all supporting facts */
  
  for (plList=poiToRelPlan->pcSupFacts; plList; plList=plList->plNext) {
    //CELLP pc;
    if (bDebugRelPlanExtraction) {
      fprintf(HandleToFileStream(3),"Recursing on node ");
      PrintHNode(HandleToFileStream(3),plList->uValue.phbtTree);
      fprintf(HandleToFileStream(3),"\n");
    }
    //    fprintf(HandleToFileStream(3),"Recursing on node %p, with  arguments:",plList->uValue.phbtTree);
    //for (pc=plList->uValue.phbtTree->pcKey; pc; pc=pc->pcNext) {
    //  fprintf(HandleToFileStream(3),"%s,",IdentName(pc->pfForm));
    //}
    //    fprintf(HandleToFileStream(3),"\n");
    RelaxedPlanFromNode_BACKTRACK(plList->uValue.phbtTree,plpPositivePlan,plpNegativePlan,poiToRelPlan->nDepth-1,pbBindings);
  }
  phbtTree->bFFSearched=TRUE;
  //  fprintf(HandleToFileStream(3),"returning on %p\n",phbtTree);
  return TRUE;
}





BOOL EvalDescPredicate_Extract_Relaxed_Plan 
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELL pcTerms[50];
	int nIndex;
	BOOL b,bTerms;
	HBTREEP foundNode;
	
	//	plGoalFacts=NULL;

	ENTER("EvalDescPredicateStd",TRUE);
	nIndex=pcFormula->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
	if(pcFormula->pfForm->pcArgs)
	{
	    bTerms=ComputeTermsNoCopy(pcFormula->pfForm->pcArgs,pcTerms,plpLinearPlan,pbBindings);
	    if(!bTerms)
		TermError("eval-desc-predicate",pcFormula,pbBindings);
	}
	
	foundNode=HBTreeSearch(pcTerms,plpLinearPlan->aphbtWorld[nIndex]);

	foundNode->bAddedByRelaxed=1; // since this is a goal fact, we mark it as part of the 
	                              // relaxed plan
	b=(BOOL)foundNode;
	
	if (foundNode) {
	  LISTP plNewHead;
	  if (bDebugRelPlanExtraction) {
	    fprintf(HandleToFileStream(3),"CALLING RelPlanFromNode to satisfy:");
	    PrintHNode(HandleToFileStream(3),foundNode);
	    fprintf(HandleToFileStream(3),"\n");
	  }
	  RelaxedPlanFromNode_BACKTRACK(foundNode,plpLinearPlan,plpNegPlan,nRelPlanDepth,pbBindings);
	  plNewHead=(LISTP)MemAlloc(sizeof(LIST));
	  plNewHead->uValue.phbtTree=foundNode;
	  plNewHead->plNext=plGoalFacts;
	  plGoalFacts=plNewHead;
	}

	EXIT("EvalDescPredicateStd");
	return b;
}



void SimplifyRelaxedPlan   // removes useless actions from the relaxed plan
(
 int nDepth
 )
{
  BOOL bSimplifiable;
  LISTP plThisLevelActions;
  LISTP *pplPrev;
  int depth;
  LISTP plAction,plFacts;


  
  for (depth=0;depth<nDepth;depth++) {

  restart:
    
    plThisLevelActions=pplRelaxedPlan[depth];
    
    // first pass: reset counters
    
    
    for (plAction=plThisLevelActions; 
	 plAction; 
	 plAction=plAction->plNext) {
      
      for (plFacts=plAction->uValue.poiOperInstance->plAddedFacts;
	   plFacts;
	   plFacts=plFacts->plNext) {
	plFacts->uValue.phbtTree->nSimpCount=0;
	
      }
    }

    bSimplifiable=FALSE;
    for (plAction=plThisLevelActions; 
	 plAction; 
	 plAction=plAction->plNext) {
      
      for (plFacts=plAction->uValue.poiOperInstance->plAddedFacts;
	   plFacts;
	   plFacts=plFacts->plNext) {
	plFacts->uValue.phbtTree->nSimpCount++;
	if (plFacts->uValue.phbtTree->nSimpCount>1) 
	  bSimplifiable=TRUE;
      }
    }

    if (bSimplifiable) {
      pplPrev=pplRelaxedPlan+depth;
      
      for (plAction=plThisLevelActions; 
	   plAction; 
	   pplPrev=&plAction->plNext,plAction=plAction->plNext) {
	BOOL bSimplifiable=TRUE;
	for (plFacts=plAction->uValue.poiOperInstance->plAddedFacts;
	     plFacts;
	     plFacts=plFacts->plNext) {
	  if (plFacts->uValue.phbtTree->bFFSearched && plFacts->uValue.phbtTree->nSimpCount<2)
	    bSimplifiable=FALSE;
	}
	
	if (bSimplifiable) {
	  *pplPrev=plAction->plNext; // removing action from relaxed plan list
	  --nFFActionInstanceCounter;
	  if (bVerboseFFShowRelaxedPlans) {
	    fprintf(HandleToFileStream(3),"Simplifying following action: ");
	    PrintFormula(HandleToFileStream(3),plAction->uValue.poiOperInstance->pcActionInstance,0);
	  }
	  goto restart;
	}
	
      }
    }
    
  }
}

int RelaxedPlanLength
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	int nDepth,
	BOOL bPreserving // whether or not we are preserving a property
) 
{
  //CELLP pc;
  int nSize;
  
  if (bGoalOcclusions) plGoalFacts=NULL; 

  bTryToPreserve=1;
  nRelPlanDepth=nDepth;
  if (!pcGoalFormula) return 0; /* there's no goal */
  if (nDepth==0) {  /* goal is satisfied already */
    pplRelaxedPlan=NULL;
    return 0;
  }
  nSize=(poHOperators)?nNumberOfHOperators:nNumberOfOperators;


  if (bVerboseFFShowRelaxedPlans)
    fprintf(HandleToFileStream(3),"Starting relaxed plan extraction ----------------------\n");

  bRelaxedModeNeg=bUseHWorld=bUseHWorldFF=FALSE; /* ensure we are using the right
						      version of EvalDescPredicate */

  bExtractingRelaxedPlan=TRUE;
  
 
 
  /* Allocate space for relaxed plan */
  if (nDepth>0) {
    pplRelaxedPlan=(LISTP*)MemAlloc(sizeof(LISTP)*nDepth);
    memset(pplRelaxedPlan,0x00,sizeof(LISTP)*nDepth);
    
    pplIdealRelaxedPlan=(LISTP*)MemAlloc(sizeof(LISTP)*nDepth);
    memset(pplIdealRelaxedPlan,0x00,sizeof(LISTP)*nDepth);
    
    anIdealMutexPenalties=(int*)MemAlloc(sizeof(int)*nDepth);
    memset(anIdealMutexPenalties,0x00,sizeof(int)*nDepth);
  } 

  /* Allocate space for the action instance array */
  pplActionInstances=(LISTP*)MemAlloc(sizeof(LISTP)*nSize);
  memset(pplActionInstances,0x00,sizeof(LISTP)*nSize);

  pplIdealActionInstances=(LISTP*)MemAlloc(sizeof(LISTP)*nSize);
  memset(pplIdealActionInstances,0x00,sizeof(LISTP)*nSize);

  /* Reset the action instance counter */
  nFFActionInstanceCounter=0;
  /* Evaluate the goal formula */
  (*pcGoalFormula->pfForm->paAction->pfEval)(pcGoalFormula,plpLinearPlan,pbBindings);


  if (bVerboseFFShowRelaxedPlans) {
    LISTP plList;

    fprintf(HandleToFileStream(3),"Helpful Action List (p=%p): ",pplRelaxedPlan[0]);
    for (plList=pplRelaxedPlan[0]; plList; plList=plList->plNext)
      fprintf(HandleToFileStream(3), " (%d,%d)",
	      plList->uValue.poiOperInstance->nOperNumber,
	      plList->uValue.poiOperInstance->nOperInstance);
    
    fprintf(HandleToFileStream(3),"\nRelaxed plan extraction ended ----------------------\n");
  }

  if (bFFSimplifyRelaxedPlan) SimplifyRelaxedPlan(nDepth);
  
  fflush(HandleToFileStream(3));
  /* setting booleans back */
  bUseHWorldFF=TRUE; 
  bExtractingRelaxedPlan=FALSE;

  return nFFActionInstanceCounter;
}



void Penalties
(
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	int nDepth,
	double *pdOcclusionPenalty,
	double *pdMutexPenalty
) 
{
  LINEARPLANP temporalPlan;
  HBTREEP *newWorld;
  int depth;
  OPERATORP po;
  
  // create copies of local plan
  plpLevel0Plan=plpLinearPlan;
  
  /* build dummy plan */
  newWorld = CopyWorld_H(LinearPlanWorld(plpLinearPlan));
  temporalPlan = MakeWorldAction_H(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 
  
  
  aphbtNegWorld = MakeWorld_H(); /* create a new empty world for negative facts */
  
  
  plpNegPlan = MakeWorldAction_H(aphbtNegWorld,  /* dummy plan for the negated facts world */
				 LinearPlanActionName(plpLinearPlan),
				 LinearPlanActionDuration(plpLinearPlan), 
				 LinearPlanActionCost(plpLinearPlan), 
				 LinearPlanActionPriority(plpLinearPlan)); 


  
  bUnderNot=0;

  
  nMutexPenalty=nOcclusionPenalty=0;
    
  bComputingOcclusions=1;

  for (depth=0;depth<nDepth;depth++) {
    int dummy;
    nFFDepth=depth+1;
    plThisLevelActions=pplRelaxedPlan[depth];
    
    printf("HERE TOO");

    if (bVerboseFFShowRelaxedWorlds) {
      fprintf(HandleToFileStream(3),"EXECUTED world at depth %d\n",depth);
      fprintf(HandleToFileStream(3),"Evals=%d\n",njbEval);      
      PrintHWorld(3,temporalPlan,pbBindings);
      fprintf(HandleToFileStream(3),"\nNegated World at depth %d:\n",nDepth);
      PrintHWorld(3,plpNegPlan,pbBindings);
    }

    aModifyWorldAction.pfEval = EvalModifyWorldExecute; 

    while (plThisLevelActions) {
      int counter;
      for (counter=0,po=poOperators;
	   counter<plThisLevelActions->uValue.poiOperInstance->nOperNumber; 
	   counter++,po=po->poNext);

      /* make forall and exists to ignore their generators */      
      aForAllAction.pfEval = EvalForAllNoGenerator;
      aExistsAction.pfEval = EvalExistsNoGenerator;
      aExistsXAction.pfEval = EvalExistsXNoGenerator;

      if (bVerboseFFShowRelaxedPlans) {
	fprintf(HandleToFileStream(3),"Executing (%d,%d)\n",
		plThisLevelActions->uValue.poiOperInstance->nOperNumber,
		plThisLevelActions->uValue.poiOperInstance->nOperInstance);
	//DumpBindings(plThisLevelActions->uValue.poiOperInstance->pbBindings);
      }

      nFFOperNumber=plThisLevelActions->uValue.poiOperInstance->nOperNumber;
      nFFOperInstance=plThisLevelActions->uValue.poiOperInstance->nOperInstance;
	
      (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,plThisLevelActions->uValue.poiOperInstance->pbBindings);
      
    }
    AddsFromUpdateExecute(1,&dummy);

    

    if (RelPlanSelectOperator!=RelPlanSelectOperator_IDEAL && RelPlanSelectOperator!=RelPlanSelectOperator_MIN) {
      // repeating the loop, now we compute the mutexes
      
      /* change behaviour of EvalDescPredicate, now we want to compute mutexes */
      bUseHWorldFF=FALSE;
      bMutexMode=TRUE;
      aModifyWorldAction.pfEval = EvalModifyWorldNullAdvance; 
      
      
      plThisLevelActions=pplRelaxedPlan[depth];
      while (plThisLevelActions) {
	int counter;
	for (counter=0,po=poOperators;
	     counter<plThisLevelActions->uValue.poiOperInstance->nOperNumber; 
	     counter++,po=po->poNext);
	
	/* make forall and exists to ignore their generators */
	aForAllAction.pfEval = EvalForAllNoGenerator;
	aExistsAction.pfEval = EvalExistsNoGenerator;
	aExistsXAction.pfEval = EvalExistsXNoGenerator;
	
	if (bVerboseFFShowRelaxedPlans) {
	  fprintf(HandleToFileStream(3),"Executing --  (%d,%d)\n",
		  plThisLevelActions->uValue.poiOperInstance->nOperNumber,
		  plThisLevelActions->uValue.poiOperInstance->nOperInstance);
	}
	
	nFFOperNumber=plThisLevelActions->uValue.poiOperInstance->nOperNumber;
	nFFOperInstance=plThisLevelActions->uValue.poiOperInstance->nOperInstance;
	
	(*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,plThisLevelActions->uValue.poiOperInstance->pbBindings);

      }

      /* restore behaviour of EvalDescPredicate */
      bUseHWorldFF=TRUE;
      bMutexMode=FALSE;
      aModifyWorldAction.pfEval = EvalModifyWorldExecute; 
    }
  }
  
  nMutexPenalty=0;
  if (RelPlanSelectOperator==RelPlanSelectOperator_IDEAL||RelPlanSelectOperator==RelPlanSelectOperator_MIN) {
    // computing mutex penalty from array
    int j;
    nMutexPenalty=0;
    fprintf(HandleToFileStream(3),"Ideal mutex vector: ");
    for (j=0;j<nDepth;j++) {
      fprintf(HandleToFileStream(3),"%d ",anIdealMutexPenalties[j]);
      nMutexPenalty+=anIdealMutexPenalties[j];
    }
    fprintf(HandleToFileStream(3),"\n ");
  }
  

  aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
  bComputingOcclusions=0;

  if (bVerboseFFShowRelaxedPlans) {
    fprintf(HandleToFileStream(3),"Occlusion Penalty: %d   MutexPenalty: %d\n",nOcclusionPenalty,nMutexPenalty);
  }


 
  //*pdOcclusionPenalty=nOcclusionPenalty;
  *pdOcclusionPenalty=nOcclusionPenalty; //nOcclusionPenalty;
  //  *pdMutexPenalty=sqrt(1/4+nMutexPenalty)-1/2;
   *pdMutexPenalty= 2*sqrt(nMutexPenalty);
   // *pdMutexPenalty= nMutexPenalty;
  //  *pdMutexPenalty=nMutexPenalty;
  //  *pdMutexPenalty=0;

  if (bVerboseFFShowRelaxedPlans) {
    fprintf(HandleToFileStream(3),"Now Evaluating Goal Formula\n");
  }
 /* Evaluate the goal formula */
  (*pcGoalFormula->pfForm->paAction->pfEval)(pcGoalFormula,temporalPlan,pbBindings);

  if (bVerboseFFShowRelaxedPlans) {
    fprintf(HandleToFileStream(3),"Occlusion Penalty: %lf   MutexPenalty: %lf\n",*pdOcclusionPenalty,*pdMutexPenalty);
  }

}

void PrintHNode
(
 FILE *fp,
 HBTREEP phbtTree
)
{
  CELLP pc;
  fprintf(fp,"(%s",asiWorldSymbols[phbtTree->nIndex].psName);
  for (pc=phbtTree->pcKey; pc; pc=pc->pcNext) {
      fprintf(HandleToFileStream(3)," %s",IdentName(pc->pfForm));
  }
  fprintf(fp,")");
}
  


BOOL NeededBy
(
 OPERINSTANCEP poiOperInstanceA, 
 OPERINSTANCEP poiOperInstanceB
) 
{
  LISTP plFacts;
  LISTP plAddingActions;
  
  for (plFacts=poiOperInstanceB->pcSupFacts;
       plFacts;
       plFacts=plFacts->plNext)  {// looping over preconditions
    
    // A is needed by B
    
    // if B has a precondition that has only been added by A:
    
    if (plFacts->uValue.phbtTree->plActionListRelPlan) { 
      BOOL b;
      
      if (plFacts->uValue.phbtTree->plActionListRelPlan->uValue.poiOperInstance == poiOperInstanceA &&
	  plFacts->uValue.phbtTree->plActionListRelPlan->plNext==NULL)
	return TRUE;  // only action that added this fact is A
      
      b=TRUE;

      for (plAddingActions=plFacts->uValue.phbtTree->plActionListRelPlan;
	   plAddingActions;
	   plAddingActions=plAddingActions->plNext) {
	
	if (plAddingActions->uValue.poiOperInstance->nDepth<poiOperInstanceB->nDepth &&
	    !NeededBy(poiOperInstanceA,plAddingActions->uValue.poiOperInstance)) {
	  b=FALSE;
	  break;
	}

      }
      if (b) return TRUE; // all actions that add this precondition are need A
      
    }
  
    
    
  }
  return FALSE;
 } 

static BOOL AddedByNeeded
(
 HBTREEP phbtFact,
 OPERINSTANCEP poiOperInstance
)
{
  LISTP plAction;
  
  if (bVerboseFFShowRelaxedPlans) {
    fprintf(HandleToFileStream(3),"Candidate occluded fact: ");
    PrintHNode(HandleToFileStream(3),phbtFact);
    fprintf(HandleToFileStream(3),"-- action (%d,%d,%d)", poiOperInstance->nOperNumber,poiOperInstance->nOperInstance, poiOperInstance->nDepth);
    fprintf(HandleToFileStream(3),"\n");
    fflush(HandleToFileStream(3));
  }
  
  for (plAction=phbtFact->plActionListRelPlan; 
       plAction; 
       plAction=plAction->plNext) {
    
    if (NeededBy(plAction->uValue.poiOperInstance,poiOperInstance)) return TRUE;

  }
  return FALSE;
}

int OcclusionPenalties
(
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan,
 int nDepth
)
{
  int nOcclusions=0;
  int depth;
  LISTP plAction;
  LISTP plFacts;
  HBTREEP complement;

  for (depth=0;depth<nDepth;depth++) {
    
    plThisLevelActions=pplRelaxedPlan[depth];

    //    fprintf(HandleToFileStream(3),"-- Counting Occlusions\n ");
    for (plAction=plThisLevelActions; 
	 plAction; 
	 plAction=plAction->plNext) {
      
      //      fprintf(HandleToFileStream(3),"Looking at action: ");
      //      PrintFormula(HandleToFileStream(3),plAction->uValue.poiOperInstance->pcActionInstance,0);
      
      
      // compute occlusions
      
      for (plFacts=plAction->uValue.poiOperInstance->pcSupFacts;
	   plFacts;
	   plFacts=plFacts->plNext) {
	
	plFacts->uValue.phbtTree->nOccCounted=0; // marking as counted
	
	if ( (complement=complementNode(plpPositivePlan,plpNegativePlan,plFacts->uValue.phbtTree)) ) {
	  //	  fprintf(HandleToFileStream(3),"Looking at a complement ");
	  if (complement->nDepthAddedOcc > plFacts->uValue.phbtTree->nDepthAddedOcc
	      &&
	      AddedByNeeded(complement,plAction->uValue.poiOperInstance) )
	    {
	      nOcclusions++;
	      plFacts->uValue.phbtTree->nOccCounted=1; // marking as counted
	      plFacts->uValue.phbtTree->nDepthOccCounted=depth+1; // marking as counted
	      //complement->nDepthAddedOcc=depth+1; // un-occluding complement
	      plFacts->uValue.phbtTree->nDepthAddedOcc=depth+1; //un-occluding fact 

	      if (bVerboseFFShowRelaxedPlans) {
		fprintf(HandleToFileStream(3),"Occluded fact");
		PrintHNode(HandleToFileStream(3),plFacts->uValue.phbtTree);
		fprintf(HandleToFileStream(3),"\n");
	      }
	    }
	}
      }
    }
    //    fprintf(HandleToFileStream(3),"-- Now updating\n ");
    
    for (plAction=plThisLevelActions; 
	 plAction; 
	 plAction=plAction->plNext) {
      //      fprintf(HandleToFileStream(3),"Looking at action: ");
      //      PrintFormula(HandleToFileStream(3),plAction->uValue.poiOperInstance->pcActionInstance,0);
      // update additions 
      for (plFacts=plAction->uValue.poiOperInstance->plAddedFacts;
	   plFacts;
	   plFacts=plFacts->plNext) {
	//	fprintf(HandleToFileStream(3),"updating depth for %s",(plFacts->uValue.phbtTree->apbtOwner==plpPositivePlan->aphbtWorld)?"":"not-");
	//	PrintHNode(HandleToFileStream(3),plFacts->uValue.phbtTree);
	//	fprintf(HandleToFileStream(3),"\n");
	if (plFacts->uValue.phbtTree->nOccCounted==1 && plFacts->uValue.phbtTree->nDepthOccCounted==depth+1) {
	  // this has been counted, as occluded discounting!
	  nOcclusions--;
	  plFacts->uValue.phbtTree->nOccCounted=0; // avoid discounting again for this fact
	  if (bVerboseFFShowRelaxedPlans) {
	    fprintf(HandleToFileStream(3),"Discounted fact\n");
	    PrintHNode(HandleToFileStream(3),plFacts->uValue.phbtTree);
	    fprintf(HandleToFileStream(3),"\n");
	  }
	}
	plFacts->uValue.phbtTree->nDepthAddedOcc=depth+1;
	plFacts->uValue.phbtTree->plActionListRelPlan=InsertOperInstance(plAction->uValue.poiOperInstance,
									 plFacts->uValue.phbtTree->plActionListRelPlan);
      }
    }
    
  }
  if (bVerboseFFShowRelaxedPlans) {
    fprintf(HandleToFileStream(3),"Real Occlusion Penalty=%d\n",nOcclusions);
  }
  return dPrecondOcclusionMultiplier*nOcclusions;
}


int GoalOcclusionPenalties
(
 LINEARPLANP plpPositivePlan,
 LINEARPLANP plpNegativePlan
)
{
  int sum;
  LISTP pl;
  HBTREEP complement;
  
  sum=0;
  for (pl=plGoalFacts; pl; pl=pl->plNext) {
    //    CELLP pc;
    if ( (complement=complementNode(plpPositivePlan,plpNegativePlan,pl->uValue.phbtTree) ) ) {
      //      fprintf(HandleToFileStream(3),"Looking at node %p, with arguments:",pl->uValue.phbtTree);
      // for (pc=pl->uValue.phbtTree->pcKey; pc; pc=pc->pcNext) {
      //	fprintf(HandleToFileStream(3),"%s,",IdentName(pc->pfForm));
      // }
      //      fprintf(HandleToFileStream(3),"depth=%d, complementdepth=%d\n",pl->uValue.phbtTree->nDepthAddedByRelaxed,complement->nDepthAddedByRelaxed);
      if (complement->nDepthAddedByRelaxed>pl->uValue.phbtTree->nDepthAddedByRelaxed)
	++sum;
    } else {
      //      fprintf("no complement for node %p\n",pl->uValue.phbtTree);
    }
  }
  if (bVerboseFFShowRelaxedPlans) {
    fprintf(HandleToFileStream(3),"Goal occlusion penalty=%d\n",sum);
  }
  //  return sum;
  return dGoalOcclusionMultiplier*sum;
}





int njbEval,njbCallsEvalAdd;  // for debugging. consider deleting me.


CELLP ComputeGoalDistance_QualPreferences_FF_unreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOL bHeuristicForEachGoal
) 
{
    CELLP pc;
    CELLP pcMod;
    HBTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    int best_preference_value=-1;   /* best_metric now contains the best value... */
    /*    double discountedMetric=100000.0; */
    /* double metric; */
    int goals, preferences;
    double goalDistanceSum;
    double goal_cost=0.0;
    int a;
    BOOL bRealActionPerformed;
    BOOL bChange;
    int act,actual_adds; /* actual adds */
    int a1=0,a2=0;
    int njbTotalEval,njbTotalAdds;

    ZONE *saveZone = pzCurrent;
    int i;
    int nDepth;
    double occlusionPenalty,mutexPenalty;
    
    int nFixPointLimit=200; 


    ENTER("ComputeGoalDistance_CRC_Negations_Qualprefs",TRUE);

    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

   
    bUnderNot = 0;


    bUseHWorldFF = 1;

    aAddAction.pfEval = EvalAdd_FF;
    aDelAction.pfEval = EvalDel_FF;
    aAndAction.pfEval = EvalAndRelaxed_FF;
    aForAllAction.pfEval = EvalForAll_FF;
    aForAllActionNoReset.pfEval = EvalForAll_HNoReset;
    aExistsAction.pfEval = EvalExists_H;
    aExistsXAction.pfEval = EvalExistsX_H;


//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;

    if (bVerboseFFShowRelaxedPlans) {
      fprintf(HandleToFileStream(3),"Computing h for plan %p labeled by ",plpLinearPlan);
      PrintFormula(HandleToFileStream(3),plpLinearPlan->pcActionName,0);
    }

    if (firstRunGoalDistances) {
	InitGoalDistanceQual();
    }

    // initialize distances to preferences:
    plpLinearPlan->afDistancesToPrefValues = (double*)ZoneAlloc((1+nQualPreferenceNumber)*sizeof(double));

    // initialize heuristic vector
    if (bHeuristicForEachGoal) 
      plpLinearPlan->nHeuristicVectorSize=1+totalGoals;
    else
      plpLinearPlan->nHeuristicVectorSize=1;

    plpLinearPlan->afHeuristicVector=(double*)MemAlloc(plpLinearPlan->nHeuristicVectorSize*sizeof(double));
    
    /* create a new plan */
    SetZone(&zScratch);

    plpLevel0Plan=plpLinearPlan;

    /* build dummy plan */
    newWorld = CopyWorld_H(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction_H(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 


    aphbtNegWorld = MakeWorld_H(); /* create a new empty world for negative facts */
    
    
    plpNegPlan = MakeWorldAction_H(aphbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    //    WorldSignature(temporalPlan);WorldSignature(plpNegPlan);
    //    temporalPlan->nSignature+=plpNegPlan->nSignature;
    

    //    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
    //	pWorldSummary=ComputeAllDistances_Qual(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    //    }


    
    nDepth=0;
    a=0;

    bChange= bRealActionPerformed=FALSE;
    
    actual_adds=0;
    nFixPoints = 0;
    nRealFixPoints = 0;

    
    njbTotalEval=0; njbTotalAdds=0; njbCallsEvalAdd=0;

    while (1) {
      OPERATORP po;
      //      CELLP pcOldAddEnd,pcOldDelEnd; 

      if (bHeuristicForEachGoal) 
	goals = NumberOfGoalsSatisfied_H_costvector(temporalPlan,pbBindings,&goal_cost,plpLinearPlan->afHeuristicVector+1);
      else
	goals = NumberOfGoalsSatisfied_H(temporalPlan,pbBindings,&goal_cost);

      preferences = QualPreferenceValue(temporalPlan,pbBindings);

      /* updating nFixPoints and nRealFixPoints */

      if (nDepth>0&&a==0) nFixPoints++;  
      if (a>0) nFixPoints=0;

      if (nDepth>0&&actual_adds==0) nRealFixPoints++;
      if (actual_adds>0) nRealFixPoints=0;

      if (bVerboseFFShowRelaxedWorlds) {
	fprintf(HandleToFileStream(3),"World at depth %d (adds=%d/addsbyupdateseq=%d/actual_adds(including unrelaxed)=%d/number_of_unrelaxed_fixed_points=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));      
	fprintf(HandleToFileStream(3),"Evals=%d\n",njbEval);      
	PrintHWorld(3,temporalPlan,pbBindings);
      	fprintf(HandleToFileStream(3),"\nNegated World at depth %d:\n",nDepth);
      	PrintHWorld(3,plpNegPlan,pbBindings);
      }
      
      /* end loop if all goals are satisfied and preference value is the maximum */

      if (best_preference_value<preferences)
	best_preference_value = preferences;
      
    
      if (goals>=totalGoals&&preferences>=nQualPreferenceNumber) break;


      //      if (nDepth>0&&(actual_adds==0||(a==0&&nFixPoints>=nFixPointLimit))) break;

      if (nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))) break;

      //      if (nDepth>0&&(!bChange||(a==0&&bRealActionPerformed))) break;
     
      
      /* we compute the successor world */
      a=0;

      bChange=FALSE;
      bRealActionPerformed=FALSE;

      nFFDepth=nDepth+1;


      if (!poHOperators) { /* if there are no heuristic-specific operators 
			      use the domain operators */
	for(nFFOperNumber=0,po=poOperators;po;po=po->poNext,nFFOperNumber++) {
	  nFFOperInstance=0;
	  pcFFSupFacts=NULL;
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	}
      } else {
	for(nFFOperNumber=0,po=poHOperators;po;po=po->poNext,nFFOperNumber++) {
	  nFFOperInstance=0;
	  pcFFSupFacts=NULL;
	  dEvalTotalCost = 0;
	  njbEval=0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	  njbTotalEval+=njbEval;
	}
      }
      
      /* process the action costs where each action has cost 1 */

      //     fprintf(HandleToFileStream(3),"Processing operators -- \n");

      a1=a=AddsFromUpdateWorld_FF_unreleffs(1.0,&act);
      actual_adds=act;
      njbTotalAdds+=a1;
      aIfThenElseAction.pfEval = EvalIfThenElse_RESET; 
       
       
      /* probably is best to do this with the and, and reset the cost only
	 if the initial cost was also 0 */
      for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	/* this does not work if the post action sequence contains nested ands */
	BOOL aux_bool;
	dEvalTotalCost = 0;

	//	if (*pcMod->pfForm->paAction->pfEval == EvalAnd) {  /* this is an and, we need to be careful not to add up costs */
	// for (pcMod1=pcMod->pfForm->pcArgs; pcMod1; pcMod1=pcMod1->pcNext) {
	//   (*pcMod1->pfForm->paAction->pfEval)(pcMod1,temporalPlan,pbBindings);
	//   dEvalTotalCost = 0;
	// }
	//} else {
	  	
	  aux_bool = (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	  
	  //}

	  
      //  (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);

	  a2=AddsFromUpdateWorld_FF_unreleffs(0.0,&act);
	  a+=a2;
	  actual_adds+=act;
	  njbTotalAdds+=a2;

      }
      aIfThenElseAction.pfEval = EvalIfThenElse; 
      
      ++nDepth;
      
      //      printf("d=%d,a=%d\n",nDepth,a);

    }


    //  fprintf(HandleToFileStream(3),"Final depth %d(a1=%d/a2=%d/actual_adds=%d/nf=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));
    //fprintf(HandleToFileStream(3),"TotalEvals=%d\n",njbTotalEval);
    //fprintf(HandleToFileStream(3),"TotalAdds=%d\n",njbTotalAdds);
    //fprintf(HandleToFileStream(3),"CallsEvalAdds=%d\n",njbCallsEvalAdd);
    //    fprintf(HandleToFileStream(3),"World at final depth %d:\n",nDepth);
    //PrintHWorld(3,temporalPlan,pbBindings);

    
    //    fprintf(stderr,"!%d",nDepth);


    // trivially assigning infinity to everything
    for (i=0;i<=nQualPreferenceNumber; i++) 
      plpLinearPlan->afDistancesToPrefValues[i]=INFTY;
    
    plpLinearPlan->afDistancesToPrefValues[0]=0; /* to achieve nothing is immediate */
    
    // now compute the costs to the preferences

    for (i=nQualPreferenceNumber, pcMod=pcBDF; pcMod; pcMod=pcMod->pcNext,i--) {
      dEvalTotalCost=0; /* setting evaluation cost to 0 */
      if ((*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings)) /* if the preference is true in the final world */
	plpLinearPlan->afDistancesToPrefValues[i]=dEvalTotalCost; 
    }	

    // if (goals<totalGoals) goal_cost = INFTY; /* penalizing when goals are not achieved */
    /* goalDistanceSum = goal_cost; */
    
    if (goals==totalGoals) {
      goalDistanceSum=(double)RelaxedPlanLength(NULL,temporalPlan,pbBindings,nDepth,0);
      if (bOcclusions) 
	Penalties(plpLinearPlan,pbBindings,nDepth,&occlusionPenalty,&mutexPenalty);
      else {
	occlusionPenalty=0;
	mutexPenalty=0;
      }
      //      plpLinearPlan->afHeuristicVector[0]=goal_cost;
      plpLinearPlan->afHeuristicVector[0]=goal_cost;
      // attach the list of helpful actions
      if (pplRelaxedPlan)
	plpLinearPlan->plHelpfulActions=pplRelaxedPlan[0];
      else 
	plpLinearPlan->plHelpfulActions=NULL;
    } else {
      goalDistanceSum = INFTY;
      plpLinearPlan->plHelpfulActions=NULL;
    }
    if (bVerboseFFShowRelaxedPlans) 
      fprintf(HandleToFileStream(3),"RelPlan length: %lf\nHeuristic Value=%lf\n",goalDistanceSum,goalDistanceSum+occlusionPenalty+mutexPenalty);

    ZoneClear(&zScratch);
    SetZone(saveZone);

    /* set the best prefernce value */
    plpLinearPlan->dfBestPreferenceValue = best_preference_value;
   
    pc=MakeFloatForm(goalDistanceSum+occlusionPenalty+mutexPenalty);
    // pc=MakeFloatForm(goalDistanceSum);

    bRelaxedModeNeg = 0;


     /* All functions back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aAndAction.pfEval = EvalAnd;
    aAddAction.pfEval = EvalAdd;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    aForAllAction.pfEval = EvalForAll;
    aForAllActionNoReset.pfEval = EvalForAll;
    aExistsAction.pfEval = EvalExists;
    aExistsXAction.pfEval = EvalExistsX;


    bUseHWorldFF = 0;


    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
}




CELLP ComputeGoalDistance_QualPreferences_FF_unreleffs_preserve
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOL bHeuristicForEachGoal
) 
{
    CELLP pc;
    CELLP pcMod;
    HBTREEP *newWorld;
    LINEARPLAN *temporalPlan;
    int best_preference_value=-1;   /* best_metric now contains the best value... */
    /*    double discountedMetric=100000.0; */
    /* double metric; */
    int goals, preferences;
    double goalDistanceSum;
    double goal_cost=0.0;
    int a;
    BOOL bRealActionPerformed;
    BOOL bChange;
    int act,actual_adds; /* actual adds */
    int a1=0,a2=0;
    int njbTotalEval,njbTotalAdds;

    ZONE *saveZone = pzCurrent;
    int i;
    int nDepth;
    double occlusionPenalty,mutexPenalty;
    int nGoalSeen;
    
    int nFixPointLimit=200; 
    int LEVELS_BEYOND_GOAL=2; // how many levels beyond the goal we are willing to compute


    ENTER("ComputeGoalDistance_CRC_Negations_Qualprefs",TRUE);

    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

   
    bUnderNot = 0;


    bUseHWorldFF = 1;

    aAddAction.pfEval = EvalAdd_FF;
    aDelAction.pfEval = EvalDel_FF;
    aAndAction.pfEval = EvalAndRelaxed_FF;
    aForAllAction.pfEval = EvalForAll_FF;
    aForAllActionNoReset.pfEval = EvalForAll_HNoReset;
    aExistsAction.pfEval = EvalExists_H;
    aExistsXAction.pfEval = EvalExistsX_H;


//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;

    if (bVerboseFFShowRelaxedPlans) {
      fprintf(HandleToFileStream(3),"Computing h for plan %p labeled by ",plpLinearPlan);
      PrintFormula(HandleToFileStream(3),plpLinearPlan->pcActionName,0);
    }

    if (firstRunGoalDistances) {
	InitGoalDistanceQual();
    }

    // initialize distances to preferences:
    plpLinearPlan->afDistancesToPrefValues = (double*)ZoneAlloc((1+nQualPreferenceNumber)*sizeof(double));

    // initialize heuristic vector
    if (bHeuristicForEachGoal) 
      plpLinearPlan->nHeuristicVectorSize=1+totalGoals;
    else
      plpLinearPlan->nHeuristicVectorSize=1;

    plpLinearPlan->afHeuristicVector=(double*)MemAlloc(plpLinearPlan->nHeuristicVectorSize*sizeof(double));
    
    /* create a new plan */
    SetZone(&zScratch);

    plpLevel0Plan=plpLinearPlan;

    /* build dummy plan */
    newWorld = CopyWorld_H(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction_H(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 


    aphbtNegWorld = MakeWorld_H(); /* create a new empty world for negative facts */
    
    
    plpNegPlan = MakeWorldAction_H(aphbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    //    WorldSignature(temporalPlan);WorldSignature(plpNegPlan);
    //    temporalPlan->nSignature+=plpNegPlan->nSignature;
    

    //    if (! (pWorldSummary = RelCRCHashSearch(temporalPlan->nSignature))) {
    //	pWorldSummary=ComputeAllDistances_Qual(temporalPlan,pbBindings,0,DISTANCE_LIMIT);
    //    }


    
    nDepth=0;
    a=0;

    bChange= bRealActionPerformed=FALSE;
    
    actual_adds=0;
    nFixPoints = 0;
    nRealFixPoints = 0;
    nGoalSeen=0;
    
    njbTotalEval=0; njbTotalAdds=0; njbCallsEvalAdd=0;

    while (1) {
      OPERATORP po;
      //      CELLP pcOldAddEnd,pcOldDelEnd; 

      if (bHeuristicForEachGoal) 
	goals = NumberOfGoalsSatisfied_H_costvector(temporalPlan,pbBindings,&goal_cost,plpLinearPlan->afHeuristicVector+1);
      else
	goals = NumberOfGoalsSatisfied_H(temporalPlan,pbBindings,&goal_cost);

      preferences = QualPreferenceValue(temporalPlan,pbBindings);

      /* updating nFixPoints and nRealFixPoints */

      if (nDepth>0&&a==0) nFixPoints++;  
      if (a>0) nFixPoints=0;

      if (nDepth>0&&actual_adds==0) nRealFixPoints++;
      if (actual_adds>0) nRealFixPoints=0;

      if (goals>=totalGoals) nGoalSeen++;

      if (bVerboseFFShowRelaxedWorlds) {
	fprintf(HandleToFileStream(3),"World at depth %d (adds=%d/addsbyupdateseq=%d/actual_adds(including unrelaxed)=%d/number_of_unrelaxed_fixed_points=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));      
	fprintf(HandleToFileStream(3),"Evals=%d\n",njbEval);      
	PrintHWorld(3,temporalPlan,pbBindings);
      	fprintf(HandleToFileStream(3),"\nNegated World at depth %d:\n",nDepth);
      	PrintHWorld(3,plpNegPlan,pbBindings);
      }
      
      /* end loop if all goals are satisfied and preference value is the maximum */

      if (best_preference_value<preferences)
	best_preference_value = preferences;
      
    
      //      if (goals>=totalGoals&&preferences>=nQualPreferenceNumber) break;


      //      if (nDepth>0&&(actual_adds==0||(a==0&&nFixPoints>=nFixPointLimit))) break;
      
      if (nGoalSeen>=LEVELS_BEYOND_GOAL) break;
      if (nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))) break;

      //      if (nDepth>0&&(!bChange||(a==0&&bRealActionPerformed))) break;
     
      
      /* we compute the successor world */
      a=0;

      bChange=FALSE;
      bRealActionPerformed=FALSE;

      nFFDepth=nDepth+1;


      if (!poHOperators) { /* if there are no heuristic-specific operators 
			      use the domain operators */
	for(nFFOperNumber=0,po=poOperators;po;po=po->poNext,nFFOperNumber++) {
	  nFFOperInstance=0;
	  pcFFSupFacts=NULL;
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	}
      } else {
	for(nFFOperNumber=0,po=poHOperators;po;po=po->poNext,nFFOperNumber++) {
	  nFFOperInstance=0;
	  pcFFSupFacts=NULL;
	  dEvalTotalCost = 0;
	  njbEval=0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	  njbTotalEval+=njbEval;
	}
      }
      
      /* process the action costs where each action has cost 1 */

      //     fprintf(HandleToFileStream(3),"Processing operators -- \n");

      a1=a=AddsFromUpdateWorld_FF_unreleffs(1.0,&act);
      actual_adds=act;
      njbTotalAdds+=a1;
      aIfThenElseAction.pfEval = EvalIfThenElse_RESET; 
       
       
      /* probably is best to do this with the and, and reset the cost only
	 if the initial cost was also 0 */
      for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	/* this does not work if the post action sequence contains nested ands */
	BOOL aux_bool;
	dEvalTotalCost = 0;

	//	if (*pcMod->pfForm->paAction->pfEval == EvalAnd) {  /* this is an and, we need to be careful not to add up costs */
	// for (pcMod1=pcMod->pfForm->pcArgs; pcMod1; pcMod1=pcMod1->pcNext) {
	//   (*pcMod1->pfForm->paAction->pfEval)(pcMod1,temporalPlan,pbBindings);
	//   dEvalTotalCost = 0;
	// }
	//} else {
	  	
	  aux_bool = (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	  
	  //}

	  
      //  (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);

	  a2=AddsFromUpdateWorld_FF_unreleffs(0.0,&act);
	  a+=a2;
	  actual_adds+=act;
	  njbTotalAdds+=a2;

      }
      aIfThenElseAction.pfEval = EvalIfThenElse; 
      
      ++nDepth;
      
      //      printf("d=%d,a=%d\n",nDepth,a);

    }


    //  fprintf(HandleToFileStream(3),"Final depth %d(a1=%d/a2=%d/actual_adds=%d/nf=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));
    //fprintf(HandleToFileStream(3),"TotalEvals=%d\n",njbTotalEval);
    //fprintf(HandleToFileStream(3),"TotalAdds=%d\n",njbTotalAdds);
    //fprintf(HandleToFileStream(3),"CallsEvalAdds=%d\n",njbCallsEvalAdd);
    //    fprintf(HandleToFileStream(3),"World at final depth %d:\n",nDepth);
    //PrintHWorld(3,temporalPlan,pbBindings);

    
    //    fprintf(stderr,"!%d",nDepth);


    // trivially assigning infinity to everything
    for (i=0;i<=nQualPreferenceNumber; i++) 
      plpLinearPlan->afDistancesToPrefValues[i]=INFTY;
    
    plpLinearPlan->afDistancesToPrefValues[0]=0; /* to achieve nothing is immediate */
    
    // now compute the costs to the preferences

    for (i=nQualPreferenceNumber, pcMod=pcBDF; pcMod; pcMod=pcMod->pcNext,i--) {
      dEvalTotalCost=0; /* setting evaluation cost to 0 */
      if ((*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings)) /* if the preference is true in the final world */
	plpLinearPlan->afDistancesToPrefValues[i]=dEvalTotalCost; 
    }	

    // if (goals<totalGoals) goal_cost = INFTY; /* penalizing when goals are not achieved */
    /* goalDistanceSum = goal_cost; */
    
    if (goals==totalGoals) {
      occlusionPenalty=0;
      mutexPenalty=0;
      goalDistanceSum=(double)RelaxedPlanLength(NULL,temporalPlan,pbBindings,nDepth,1);
      if (bOcclusions) 
	Penalties(plpLinearPlan,pbBindings,nDepth,&occlusionPenalty,&mutexPenalty);
      if (bGoalOcclusions) {
	occlusionPenalty+=GoalOcclusionPenalties(temporalPlan,plpNegPlan);
	mutexPenalty=0;
      }
      if (bRealOcclusions) {
	occlusionPenalty+=OcclusionPenalties(temporalPlan,plpNegPlan,nDepth);
      }
      //      plpLinearPlan->afHeuristicVector[0]=goal_cost;
      plpLinearPlan->afHeuristicVector[0]=goal_cost;
      // attach the list of helpful actions
      if (pplRelaxedPlan)
	plpLinearPlan->plHelpfulActions=pplRelaxedPlan[0];
      else 
	plpLinearPlan->plHelpfulActions=NULL;
    } else {
      goalDistanceSum = INFTY;
      plpLinearPlan->plHelpfulActions=NULL;
    }
    if (bVerboseFFShowRelaxedPlans) 
      fprintf(HandleToFileStream(3),"RelPlan length: %lf\nHeuristic Value=%lf\n",goalDistanceSum,goalDistanceSum+occlusionPenalty+mutexPenalty);

    ZoneClear(&zScratch);
    SetZone(saveZone);

    /* set the best prefernce value */
    plpLinearPlan->dfBestPreferenceValue = best_preference_value;
   
    pc=MakeFloatForm(goalDistanceSum+occlusionPenalty+mutexPenalty);
    // pc=MakeFloatForm(goalDistanceSum);

    bRelaxedModeNeg = 0;


     /* All functions back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aAndAction.pfEval = EvalAnd;
    aAddAction.pfEval = EvalAdd;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    aForAllAction.pfEval = EvalForAll;
    aForAllActionNoReset.pfEval = EvalForAll;
    aExistsAction.pfEval = EvalExists;
    aExistsXAction.pfEval = EvalExistsX;


    bUseHWorldFF = 0;


    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
}



static void SetQuantifierEvalersFF(void) {
  aForAllAction.pfEval = EvalForAll_FF;
  aForAllActionNoReset.pfEval = EvalForAll_HNoReset;
  aExistsAction.pfEval = EvalExists_H;
  aExistsXAction.pfEval = EvalExistsX_H;
}


CELLP ComputeGoalDistance_PDDLPreferences_FF_unreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOL bHeuristicForEachGoal
) 
{
    CELLP pc;
    CELLP pcMod;
    HBTREEP *newWorld;
    LINEARPLANP temporalPlan;
    /*    double discountedMetric=100000.0; */
    /* double metric; */
    int goals, preferences,oldPreferences;
    double preferenceCost,oldPreferenceCost;
    double preferenceDistanceSum;
    double goalDistanceSum;
    double metric,oldMetric;
    double optimisticMetric;
    double discountedMetric;
    double currentMetric;
    double bestMetric=INFTY;
    double goal_cost=0.0;
    int a;
    BOOL bRealActionPerformed;
    BOOL bChange;
    BOOL bGoalOnly;
    int act,actual_adds; /* actual adds */
    int a1=0,a2=0;
    int njbTotalEval,njbTotalAdds;

    ZONE *saveZone = pzCurrent;
    int nDepth;

    int nFixPointLimit=200; 
    const int NUM_PREFERENCE_CRITERIA=5; /* Right now there are four different criteria:
					  - bestRelaxedMetric
					  - optimisticMetric
					  - discountedMetric
					  - preferenceDistance
					  - current metric */


    ENTER("ComputeGoalDistance_CRC_Negations_Qualprefs",TRUE);



    if (firstRunGoalDistances) {
      InitGoalDistance2(plpLinearPlan,pbBindings);
    }



    /* redefining modify world action and del action */

    aModifyWorldAction.pfEval = EvalModifyWorldRelaxed;
    aNotAction.pfEval = EvalNotRelaxedNeg;

   
    bUnderNot = 0;
    

    /* when are we planning only for a goal? */
    bGoalOnly = 
      (nOptimisticMetricPriority<0 || nOptimisticMetricPriority>=NUM_PREFERENCE_CRITERIA) &&
      (nDiscountedMetricPriority<0 || nDiscountedMetricPriority>=NUM_PREFERENCE_CRITERIA) &&
      (nBestRelaxedMetricPriority<0 || nBestRelaxedMetricPriority>=NUM_PREFERENCE_CRITERIA) &&
      (nMetricPriority<0 || nMetricPriority>=NUM_PREFERENCE_CRITERIA) &&
      (nPreferenceDistancePriority<0 || nPreferenceDistancePriority>=NUM_PREFERENCE_CRITERIA);
      
    bUseHWorldFF = 1;

    aAddAction.pfEval = EvalAdd_FF;
    aDelAction.pfEval = EvalDel_FF;
    aAndAction.pfEval = EvalAndRelaxed_FF;

    SetQuantifierEvalersFF();


//    aDelAction.pfEval = EvalDel;   /* we *are* considering deletes */
    //aNotAction.pfEval = EvalNotRelaxed;

    if (bVerboseFFShowRelaxedPlans || bVerboseFFShowRelaxedWorlds) {
      fprintf(HandleToFileStream(3),"Computing h for successor labeled by ");
      PrintFormula(HandleToFileStream(3),plpLinearPlan->pcActionName,0);
    }


    // initialize heuristic vector // we are adding at most 4 heuristics
    if (bHeuristicForEachGoal) 
      plpLinearPlan->nHeuristicVectorSize=1+4+totalGoals;
    else
      plpLinearPlan->nHeuristicVectorSize=1+4;

    plpLinearPlan->afHeuristicVector=(double*)MemAlloc(plpLinearPlan->nHeuristicVectorSize*sizeof(double));
    memset(plpLinearPlan->afHeuristicVector,0x0,plpLinearPlan->nHeuristicVectorSize*sizeof(double));
       
    /* create a new plan */
    SetZone(&zScratch);

    plpLevel0Plan=plpLinearPlan;

    /* build dummy plan */
    newWorld = CopyWorld_H(LinearPlanWorld(plpLinearPlan));
    temporalPlan = MakeWorldAction_H(newWorld,
				   LinearPlanActionName(plpLinearPlan),
				   LinearPlanActionDuration(plpLinearPlan), 
				   LinearPlanActionCost(plpLinearPlan), 
				   LinearPlanActionPriority(plpLinearPlan)); 


    aphbtNegWorld = MakeWorld_H(); /* create a new empty world for negative facts */
    
    
    plpNegPlan = MakeWorldAction_H(aphbtNegWorld,  /* dummy plan for the negated facts world */
			      LinearPlanActionName(plpLinearPlan),
			      LinearPlanActionDuration(plpLinearPlan), 
			      LinearPlanActionCost(plpLinearPlan), 
			      LinearPlanActionPriority(plpLinearPlan)); 

    nDepth=0;
    a=0;

    bChange= bRealActionPerformed=FALSE;
    
    actual_adds=0; nFixPoints = 0; nRealFixPoints = 0;

    njbTotalEval=0; njbTotalAdds=0; njbCallsEvalAdd=0; // debugging variabels

    oldPreferences=0;

    discountedMetric=currentMetric=oldPreferenceCost=oldMetric=0.0;
    
    preferenceDistanceSum=0;
    
    if (nOptimisticMetricPriority>=0 && nOptimisticMetricPriority<NUM_PREFERENCE_CRITERIA && pcOptimisticMetricFn) {
      FormulaToDouble(pcOptimisticMetricFn,temporalPlan,pbBindings,&optimisticMetric);
    }

    // main cycle (heuristic graph is computed)
    //    fprintf(HandleToFileStream(3),"--\n");
    while (1) {
      OPERATORP po;

      if (bHeuristicForEachGoal) 
	goals = NumberOfGoalsSatisfied_H_costvector(temporalPlan,pbBindings,&goal_cost,plpLinearPlan->afHeuristicVector+1);
      else
	goals = NumberOfGoalsSatisfied_H(temporalPlan,pbBindings,&goal_cost);

      preferences = NumberOfPreferencesSatisfiedWithCost(temporalPlan,pbBindings,&preferenceCost);
      
      if (nPreferenceDistancePriority>=0 && nPreferenceDistancePriority<NUM_PREFERENCE_CRITERIA) {
	preferenceDistanceSum+=pow(preferenceCost-oldPreferenceCost,dHeuristicExponent);
	oldPreferenceCost=preferenceCost;
	//preferenceDistanceSum+=pow(preferences-oldPreferences,dHeuristicExponent);
	//oldPreferences=preferences;
	//fprintf(HandleToFileStream(3),"pdsm=%lf--prefs=%d---cost=%lf\n",preferenceDistanceSum,preferences,preferenceCost);
      }
      
      /* compute metric always (we need to set plpLinearPlan->dfBestRelaxedMetric) */

      if (pcMetricFn) {
	FormulaToDouble(pcMetricFn,temporalPlan,pbBindings,&metric);
	if (nDepth==0) currentMetric=metric;
      }
      
      
      if (nDiscountedMetricPriority>=0&&nDiscountedMetricPriority<NUM_PREFERENCE_CRITERIA) {	

	//	fprintf(HandleToFileStream(3),"diff: %lf",metric-oldMetric);
       	discountedMetric+=(metric-oldMetric)*pow(dMetricDiscountFactor,(nDepth==0)?0:nDepth-1);
	// discountedMetric+=(metric-oldMetric)*pow(dMetricDiscountFactor,(nDepth==0)?0:nDepth);
	//fprintf(HandleToFileStream(3),"dm(%d): %lf\n",nDepth,discountedMetric);
	oldMetric=metric;
      }
      
      if (bestMetric > metric)
	bestMetric=metric;
     

      /* updating nFixPoints and nRealFixPoints */

      if (nDepth>0&&a==0) nFixPoints++;  
      if (a>0) nFixPoints=0;

      if (nDepth>0&&actual_adds==0) nRealFixPoints++;
      if (actual_adds>0) nRealFixPoints=0;

      if (bVerboseFFShowRelaxedWorlds) {
	fprintf(HandleToFileStream(3),"World at depth %d (adds=%d/addsbyupdateseq=%d/actual_adds(including unrelaxed)=%d/number_of_unrelaxed_fixed_points=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));      
	fprintf(HandleToFileStream(3),"Metric=%lf\n",metric);      
	fprintf(HandleToFileStream(3),"Preferences=%d/%d\n",preferences,totalPreferences);
	fprintf(HandleToFileStream(3),"Evals=%d\n",njbEval);      
	PrintHWorld(3,temporalPlan,pbBindings);
      	fprintf(HandleToFileStream(3),"\nNegated World at depth %d:\n",nDepth);
      	PrintHWorld(3,plpNegPlan,pbBindings);
      }
      
      /* end loop if all goals are satisfied and preference value is the maximum */

      if (bGoalOnly&&goals>=totalGoals) break;  // we are planning for a goal only, early stop

      if (!bGoalOnly&&goals>=totalGoals&&preferences>=totalPreferences) break;
      
      /* end loop if there is a fixed point */

      if (nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))) break;
      
      /* we compute the successor world */
      a=0;

      bChange=FALSE;
      bRealActionPerformed=FALSE;

      nFFDepth=nDepth+1;


      if (!poHOperators) { /* if there are no heuristic-specific operators 
			      use the domain operators */
	for(nFFOperNumber=0,po=poOperators;po;po=po->poNext,nFFOperNumber++) {
	  nFFOperInstance=0;
	  pcFFSupFacts=NULL;
	  dEvalTotalCost = 0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	}
      } else {
	for(nFFOperNumber=0,po=poHOperators;po;po=po->poNext,nFFOperNumber++) {
	  nFFOperInstance=0;
	  pcFFSupFacts=NULL;
	  dEvalTotalCost = 0;
	  njbEval=0;
	  bHSPIgnoreOperator=po->bHSPIgnore;
	  (*po->pcSuccessor->pfForm->paAction->pfEval)(po->pcSuccessor,temporalPlan,pbBindings);
	  njbTotalEval+=njbEval;
	}
      }
      
      /* process the action costs where each action has cost 1 */

      //     fprintf(HandleToFileStream(3),"Processing operators -- \n");

      a1=a=AddsFromUpdateWorld_FF_unreleffs(1.0,&act);
      actual_adds=act;
      njbTotalAdds+=a1;
      aIfThenElseAction.pfEval = EvalIfThenElse_RESET; 
       
       
      /* probably is best to do this with the and, and reset the cost only
	 if the initial cost was also 0 */
      for(pcMod=pcPostActionSequence;pcMod;pcMod=pcMod->pcNext) {
	/* this does not work if the post action sequence contains nested ands */
	BOOL aux_bool;
	dEvalTotalCost = 0;
	
	aux_bool = (*pcMod->pfForm->paAction->pfEval)(pcMod,temporalPlan,pbBindings);
	
	a2=AddsFromUpdateWorld_FF_unreleffs(0.0,&act);
	a+=a2;
	actual_adds+=act;
	njbTotalAdds+=a2;
	
      }
      aIfThenElseAction.pfEval = EvalIfThenElse; 
      
      ++nDepth;
      
      //      printf("d=%d,a=%d\n",nDepth,a);

    }


    //    fprintf(HandleToFileStream(3),"best metric=%lf\n",bestMetric);
    //  fprintf(HandleToFileStream(3),"Final depth %d(a1=%d/a2=%d/actual_adds=%d/nf=%d) goals=%d/%d boolean=%d\n",nDepth,a1,a2,actual_adds,nFixPoints,goals,totalGoals,(nDepth>0&&(nRealFixPoints>1||(a==0&&nFixPoints>=nFixPointLimit))));
    //fprintf(HandleToFileStream(3),"TotalEvals=%d\n",njbTotalEval);
    //fprintf(HandleToFileStream(3),"TotalAdds=%d\n",njbTotalAdds);
    //fprintf(HandleToFileStream(3),"CallsEvalAdds=%d\n",njbCallsEvalAdd);
    //    fprintf(HandleToFileStream(3),"World at final depth %d:\n",nDepth);
    //PrintHWorld(3,temporalPlan,pbBindings);

    
    //    fprintf(stderr,"!%d",nDepth);

    // if (goals<totalGoals) goal_cost = INFTY; /* penalizing when goals are not achieved */
    /* goalDistanceSum = goal_cost; */
    
    if (goals==totalGoals) {
      int base;
      
      //printf("HERE%d!",nDiscountedMetricPriority);

      goalDistanceSum=(double)RelaxedPlanLength(NULL,temporalPlan,pbBindings,nDepth,0);
      plpLinearPlan->afHeuristicVector[0]=goal_cost;
      if (bHeuristicForEachGoal) 
	base=totalGoals+1;
      else base=1;

      // assign preference criteria in order

      if (nDiscountedMetricPriority>=0&&nDiscountedMetricPriority<NUM_PREFERENCE_CRITERIA) {
	//	printf("HERE--! %d %lf",nBestRelaxedMetricPriority,discountedMetric);
	//	fflush(stdout);
	plpLinearPlan->afHeuristicVector[base+nDiscountedMetricPriority]=discountedMetric;
      }
      
      if (nOptimisticMetricPriority>=0&&nOptimisticMetricPriority<NUM_PREFERENCE_CRITERIA) {
	
	plpLinearPlan->afHeuristicVector[base+nOptimisticMetricPriority]=optimisticMetric;
      }

      if (nBestRelaxedMetricPriority>=0&&nBestRelaxedMetricPriority<NUM_PREFERENCE_CRITERIA) {
	plpLinearPlan->afHeuristicVector[base+nBestRelaxedMetricPriority]=bestMetric;
      }
      
      if (nPreferenceDistancePriority>=0&&nPreferenceDistancePriority<NUM_PREFERENCE_CRITERIA) {
	plpLinearPlan->afHeuristicVector[base+nPreferenceDistancePriority]=preferenceDistanceSum;
      }
      
      if (nMetricPriority>=0&&nMetricPriority<NUM_PREFERENCE_CRITERIA) {
	plpLinearPlan->afHeuristicVector[base+nMetricPriority]=currentMetric;
      }

      // set the best relaxed metric
      plpLinearPlan->dfBestRelMetric=bestMetric;
      // attach the list of helpful actions
      if (pplRelaxedPlan)
	plpLinearPlan->plHelpfulActions=pplRelaxedPlan[0];
      else 
	plpLinearPlan->plHelpfulActions=NULL;
    } else {
      goalDistanceSum = INFTY;
      plpLinearPlan->plHelpfulActions=NULL;
    }

    if (bVerboseFFShowRelaxedPlans) 
      fprintf(HandleToFileStream(3),"Heuristic Value=%lf\n",goalDistanceSum);



    ZoneClear(&zScratch);
    SetZone(saveZone);
   
    pc=MakeFloatForm(goalDistanceSum);

    bRelaxedModeNeg = 0;


     /* All functions back to normal */
    aModifyWorldAction.pfEval = EvalModifyWorld;
    aAndAction.pfEval = EvalAnd;
    aAddAction.pfEval = EvalAdd;
    aDelAction.pfEval = EvalDel;
    aNotAction.pfEval = EvalNot;
    aForAllAction.pfEval = EvalForAll;
    aForAllActionNoReset.pfEval = EvalForAll;
    aExistsAction.pfEval = EvalExists;
    aExistsXAction.pfEval = EvalExistsX;


    bUseHWorldFF = 0;


    EXIT("ComputeGoalDistance_CRC_Negations");
    return pc;
}





CELLP ComputeGoalDistanceQual
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
   return ComputeGoalDistance_CRC_Negations_Qualprefs(pcFormula,plpLinearPlan,pbBindings);
   // return ComputeGoalDistance_CRC_Negations_Qualprefs_HSP_unreleffs(pcFormula,plpLinearPlan,pbBindings);
}



CELLP ComputeGoalDistanceHspUnreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
  return ComputeGoalDistance_CRC_Negations_Qualprefs_HSP_unreleffs(pcFormula,plpLinearPlan,pbBindings);
}


CELLP ComputeGoalDistanceFFUnreleffs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
   return ComputeGoalDistance_QualPreferences_FF_unreleffs_preserve(pcFormula,plpLinearPlan,pbBindings,1);
  //  return ComputeGoalDistance_QualPreferences_FF_unreleffs(pcFormula,plpLinearPlan,pbBindings,1);
}


CELLP ComputeGoalDistanceFFPDDLPrefs
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
  return ComputeGoalDistance_PDDLPreferences_FF_unreleffs(pcFormula,plpLinearPlan,pbBindings,0);
}
