/* strips.c

Copyright C, 1996 - 99  F. Bacchus

Notes:
	1.  For now we only accept costs, durations and priorities which are
	simple integer constants.  (This is simple to change.)

STRIPS Operators ---------------------------------------------------------------

Implementation of STRIPS style operators.
We use the firstOrder evaluator to implement STRIPS operators.
To test if an action is applicable in a world we evalutate its
preconditions on the world, and use the formula evaluator to
generate all the correct bindings.
A STRIPS operator consists of

	pre			--- Preconditions, list of literals that may have variables.
	add			--- Add list
	del			--- del list

We compile the preconditions into a quantified formula
every instantiation that satisfies the conjunction of the precondition
literals calls a function that returns a new world where the
add and delete lists (using the current satisfying instantiation)
have been used to modify the old world.

Because of simplicity of STRIPS operators we can add some extra
operator functionality.

Example:

(def-strips-operator (pickup ?x)
	(pre	(handempty) (clear ?x) (ontable ?x))
	(add	(holding ?x))
	(del	(handempty) (clear ?x) (ontable ?x)))

*/

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "makeform.h"
#include "oper.h"
#include "strips.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "var.h"
#include "world.h"

/* local definitions and structures */

/* local function prototypes */

static BOOL NewStripsParse
(
	CELLP pcFormula,					/* input formula */
	CELLP *ppcName,						/* output name */
	CELLP *ppcPre,						/* output preconditions */
	CELLP *ppcAdd,						/* output addition formula */
	CELLP *ppcDel,						/* output delete formula */
	CELLP *ppcDuration,					/* output delta time */
	CELLP *ppcCost,						/* output delta cost */
	CELLP *ppcPriority					/* output priority */
);
static CELLP CompileSuccessor
(
	CELLP pcPre,						/* precondition generator(s) */
	CELLP pcMod							/* modifier formula */
);
static CELLP MakeForAll
(
	CELLP pcVarsRemaining,				/* variables */
	CELLP pcLits,						/* list of literals (generators) */
	CELLP pcMod							/* modifier formula */
);

/* -----------------------------------------------------------------------------
	Instantiate the actionInterface for stripsOperators

	The DefStripsOperator parses and sets up a strips operator.
	It generates a formula which when evaluated, generates (as a side effect)
	a successor world for each possible binding of the operator's arguments.
*/

/* EvalDefStripsOperator (Interface Command)

Description:
	Compile the operator into a function that can be applied to a world,
	then place it on the list of operators.
Scheme:

(define (def-strips-operator . keyword-args)
	(let ((op '())
			(successors '())
			(update-formula '())
			(preconds '())
			(name '()) (pre '((TRUE))) (add '()) (del '())
			(duration 1) (cost 1) (priority 0))
		(do ()
			((null? keyword-args))
			(case (first keyword-args)
				(:name (set! name (cadr keyword-args)))
				(:pre (set! pre (cadr keyword-args)))
				(:add (set! add (cadr keyword-args)))
				(:del (set! del (cadr keyword-args)))
				(:duration (set! duration (cadr keyword-args)))
				(:cost (set! cost (cadr keyword-args)))
				(:priority (set! priority (cadr keyword-args)))
				(else (error "Illegal keyword: ~A" (first keyword-args))))
			(set! keyword-args (cddr keyword-args)))
		(set! op (make-operator :name name :pre pre :add add :del del
				:duration duration :cost cost :priority priority))
		(set! preconds (operator-pre op))
		(let ((vars-remaining (variables-in preconds))
				(without-variables '())
				(with-variables '())
				(modify-world '()))
			(do ((lit '()))
				((null? preconds))
				(set! lit (first preconds))
				(set! preconds (rest preconds))
				(if (variable-free? lit)
					(set! without-variables (cons lit without-variables))
					(set! with-variables (cons lit with-variables))))
			(set! with-variables (reverse! with-variables))

			(set! modify-world
				(lambda (world/action bindings)
					;;(format #t "~%modify world called, bindings:~S" bindings)
					(let ((new-world (copy-world (world/action-world world/action))))
						(do ((literals del)
								(del-lit '()))
							((null? literals))
							(set! del-lit (first literals))
							(set! literals (rest literals))
							;;(format #t "~%del-lit~S" del-lit)
							(delete-world-lit (get-operator del-lit)
								(eval-terms (get-args del-lit)
									world/action bindings)
								new-world))
						(do ((literals add)
								(add-lit '()))
							((null? literals))
							(set! add-lit (first literals))
							(set! literals (rest literals))
							;;(format #t "~%Add-lit~S" add-lit)
							(add-world-lit (get-operator add-lit)
								(eval-terms (get-args add-lit)
									world/action bindings)
								new-world))
						;;(format #t "~%name~S  ~S" name bindings)

						(set! successors
							(cons (make-world/action
									new-world
									(cons (get-operator name)
										(eval-terms (get-args name)
											world/action bindings))
									(eval-term duration world/action bindings)
									(eval-term cost world/action bindings)
									priority)
								successors)))))

			(letrec ((make-forall
						(lambda (lits)
							(cond
								((null? vars-remaining)
									(if (null? lits)
										`(CALL ,modify-world)
										(list 'IMPLIES
											(if (length=1 lits)
												(first lits)
												`(AND ,@lits))
											`(CALL ,modify-world))))
								(else
									(let ((new-vars
												(intersection vars-remaining
													(variables-in (first lits)))))
										(set! vars-remaining
											(set-difference vars-remaining new-vars))
										(if (not (null? new-vars))
										`(FORALL ,new-vars ,(first lits) ,(make-forall (rest lits)))
										`(AND ,(first lits) ,(make-forall (rest lits))))))))))

				(set! update-formula
					(if without-variables
						`(AND ,@without-variables
							,(make-forall with-variables))
						(make-forall with-variables)))))

		(if *verbose*
			(begin
				(format *trace-stream*
					"~%STRIPS operator ~S compiled into formula:" name)
				(format #t "~%~S~%~%" update-formula)))
		(set-operator-eval-object! op
			(lambda (world/action)
				(set! successors '())
				(eval-formula update-formula world/action '())
				successors))
		(add-op op)))
*/

BOOL EvalDefStripsOperator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcSuccessor;					/* successor formula */
	CELLP pcName;						/* operator name */
	CELLP pcPre;						/* preconditions */
	CELLP pcAdd,pcDel;					/* add and delete formulas */
	CELLP pcMod;						/* modifier formula */
	CELLP pcDuration;					/* delta time */
	CELLP pcCost;						/* delta cost */
	CELLP pcPriority;					/* priority */
	CELLP pcModifyWorld;				/* modify world formula */
	OPERATORP po;						/* operator pointer */
	char ac[40];						/* string buffer */
	BOOL bHSPIgnore=FALSE;
	BOOL bUnrelEff=FALSE;
	/* convert the command string into an operator */

	ENTER("EvalDefStripsOperator",FALSE);
	if(!NewStripsParse(pcFormula,&pcName,&pcPre,&pcAdd,&pcDel,
		&pcDuration,&pcCost,&pcPriority))
	{
		EXIT("EvalDefStripsOperator");
		return FALSE;
	}

	/* convert adds and dels to modify world formula */

	pcMod=AppendFormula(FALSE,pcDel,pcAdd);	/* combine */
	if(pcMod&&pcMod->pcNext)
		pcMod=MakeAndForm(FALSE,pcMod);
	pcModifyWorld=MakeModifyWorldForm(FALSE,NULL,pcMod);

	/* convert preconditions to successor formula */

	pcSuccessor=CompileSuccessor(pcPre?pcPre->pfForm->pcArgs:NULL,pcModifyWorld);

	/* create the operator and thread it back into modify-world */

	po=MakeOperator(pcName,pcSuccessor,pcDuration,pcCost,pcPriority,bHSPIgnore,bUnrelEff);
	pcModifyWorld->pfForm->pcArgs->pfForm->uValue.poOperator=po;

	if(bVerbose)
	{
		CommandPrintf(pfTraceStream,"\"%s\" operator successor formula:\n",
			GetName(pcName->pfForm,ac));
		PrintFormula(pfTraceStream,pcSuccessor,0);
		CommandPrintf(pfTraceStream,"\n");
	}
	EXIT("EvalDefStripsOperator");
	return TRUE;
}

/* NewStripsParse

Description:
	Convert a strips operator expressed as a list, into a formula.
*/

static BOOL NewStripsParse
(
	CELLP pcFormula,				/* input formula */
	CELLP *ppcName,					/* output name */
	CELLP *ppcPre,					/* output preconditions */
	CELLP *ppcAdd,					/* output addition formula */
	CELLP *ppcDel,					/* output delete formula */
	CELLP *ppcDuration,				/* output delta time */
	CELLP *ppcCost,					/* output delta cost */
	CELLP *ppcPriority				/* output priority */
)
{
	CELLP pcName;					/* output name */
	CELLP pcPre;					/* output preconditions */
	CELLP pcAdd;					/* output addition formula */
	CELLP pcDel;					/* output delete formula */
	CELLP pcDuration;				/* output delta time */
	CELLP pcCost;					/* output delta cost */
	CELLP pcPriority;				/* output priority */
	CELLP pc,pc1;
	char ac[40];					/* string buffer */

	/* set defaults */

	ENTER("StripsParse",TRUE);
	pcName=NULL;
	pcPre=NULL;
	pcAdd=NULL;
	pcDel=NULL;
	pcDuration=NULL;
	pcCost=NULL;
	pcPriority=NULL;

	/* get the name and argument list */

	pcName=pcFormula->pfForm->pcArgs;
	if(!pcName)
	{
		ErrorMessage("def-strips-operator:  No name or argument list specified.\n");
		return FALSE;
	}

	/* scan through argument list */

	for(pc=pcName->pcNext;pc;pc=pc1)
	{
		pc1=pc->pcNext;
		pc->pcNext=NULL;
		if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_DURATION]))
		{
			if(pcDuration)
			{
				ErrorMessage("def-strips-operator:  %s has more than one duration clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcDuration=pc->pfForm->pcArgs;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_COST]))
		{
			if(pcCost)
			{
				ErrorMessage("def-strips-operator:  %s has more than one cost clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcCost=pc->pfForm->pcArgs;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_PRIORITY]))
		{
			if(pcPriority)
			{
				ErrorMessage("def-strips-operator:  %s has more than one priority clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcPriority=pc->pfForm->pcArgs;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_PRE]))
		{
			if(pcPre)
			{
				ErrorMessage("def-strips-operator:  %s has more than one preconditions clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcPre=pc;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_ADD]))
		{
			if(pcAdd)
			{
				ErrorMessage("def-strips-operator:  %s has more than one add clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcAdd=pc;
		}
		else if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_DEL]))
		{
			if(pcDel)
			{
				ErrorMessage("def-strips-operator:  %s has more than one delete clause\n",
					GetName(pcName->pfForm,ac));
				return NULL;
			}
			pcDel=pc;
		}
		else
		{
			ErrorMessage("def-strips-operator:  Unknown keyword \"%s\"\n",
				GetName(pc->pfForm,ac));
			EXIT("StripsParse");
			return FALSE;
		}
	}
	*ppcAdd=pcAdd;
	*ppcDel=pcDel;
	*ppcPre=pcPre;
	*ppcDuration=pcDuration?pcDuration:MakeFloatForm(1.0);
	*ppcCost=pcCost?pcCost:MakeFloatForm(1.0);
	*ppcPriority=pcPriority?pcPriority:MakeFloatForm(0.0);
	*ppcName=pcName;
	EXIT("StripsParse");
	return TRUE;
}

/* Formula compile routines ---------------------------------------------------- */

/* CompileSuccessor

Description:
	Convert a STRIPS "pre" formula which may contain multiple
	generators into a nested forall formula.
*/

static CELLP CompileSuccessor
(
	CELLP pcPre,						/* precondition generator(s) */
	CELLP pcMod							/* modifier formula */
)
{
	CELLP pcWithoutVariables;			/* list of preconditions without variables */
	CELLP pcWithoutEnd;
	CELLP pcWithVariables;				/* list of preconditions with variables */
	CELLP pcWithEnd;
	CELLP pcVars;
	CELLP pcSuccessor;					/* successor formula */
	CELLP pc,pc1;						/* formula pointers */

	/* split precondition formulas with and without variables */
	/* formulas without variables do not have generators */

	pcWithoutVariables=NULL;			/* set up listheads */
	pcWithoutEnd=(CELLP)&pcWithoutVariables;
	pcWithVariables=NULL;
	pcWithEnd=(CELLP)&pcWithVariables;
	for(pc=pcPre;pc;pc=pc->pcNext)
	{
		pc1=CopyCell(pc);
		if(VariableFreeQ(pc1))
			pcWithoutEnd=pcWithoutEnd->pcNext=pc1;
		else
			pcWithEnd=pcWithEnd->pcNext=pc1;
	}

	/* generate the successor (update) formula */

	pcVars=VariablesIn(pcWithVariables);
	pcSuccessor=MakeForAll(pcVars,pcWithVariables,pcMod);	/* set up bindings for formulas with variables */
	if(pcWithoutVariables)
	{
		pc=AppendFormula(FALSE,pcWithoutVariables,pcSuccessor);	/* append formulas with variables */
		pcSuccessor=MakeAndForm(FALSE,pc);
	}
	return pcSuccessor;
}

/* MakeForAll

Description:
	Make a forall formula which will generate all possible successor states,
	for those generators with variables.
*/

static CELLP MakeForAll
(
	CELLP pcVarsRemaining,				/* variables */
	CELLP pcLits,						/* list of generator literals */
	CELLP pcMod							/* modifier formula */
)
{
	CELLP pc,pc1;
	CELLP pcNewVars;
	CELLP pcRest;

	ENTER("MakeForAll",FALSE);
	if(!pcVarsRemaining)
	{
		if(!pcLits)
			pc=pcMod;
		else
		{
			pc=CopyCellList(pcLits);	/* hey! don't mess with this! */
			pc=MakeImpliesForm(FALSE,pcLits->pcNext?MakeAndForm(FALSE,pc):pc,
				pcMod);
		}
		EXIT("MakeForAll");
		return pc;
	}

	pcRest=pcLits->pcNext;				/* split the list of literals */
	pcLits->pcNext=NULL;
	pcNewVars=FormulaIntersection(pcVarsRemaining,VariablesIn(pcLits));
	pcVarsRemaining=FormulaDifference(pcVarsRemaining,pcNewVars);
	if(pcNewVars)
	{
		pc=CopyCell(pcLits);
		pc=MakeForAllForm(FALSE,pcNewVars,pc,
			MakeForAll(pcVarsRemaining,pcRest,pcMod));
	}
	else
	{
		pc=MakeForAll(pcVarsRemaining,pcRest,pcMod);
		pc1=CopyCell(pcLits);
		pc=MakeImpliesForm(FALSE,pc1,pc);
	}
	EXIT("MakeForAll");
	return pc;
}

