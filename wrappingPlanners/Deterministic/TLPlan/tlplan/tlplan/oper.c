/* oper.c -- Operators

Copyright C, 1996 - 99  F. Bacchus

Operators

	The interface between actions and the planner.
	Structures for holding the various components of an operator.
*/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tlplan.h"
#include "formula.h"
#include "oper.h"
#include "tl_tab.h"
#include "util.h"
#include "zone.h"

/* global data */

OPERATORP poOperators;					/* list of domain's operators */
MACROOPERATORP pmoMacroOperators;		/* list of domain's macro operators */
MACROOPERATORP pmoMacroOperEnd=(MACROOPERATORP)&pmoMacroOperators;	/* pointer to end of macro operator list */
ELIDEDOPERATORP peoElidedOperators;		/* list of domain's elided operators */
ELIDEDOPERATORP peoElidedOperEnd=(ELIDEDOPERATORP)&peoElidedOperators;	/* pointer to end of elided operator list */

/* local data */

static OPERATORP poOperEnd=(OPERATORP)&poOperators;	/* pointer to end of operator list */

/* MakeOperator

Description:
	Constructor
Note:
	pfSuccessor is not copied (so its pointer doesn't change on the caller)!
Scheme:

(define (make-operator . keyword-args)
  (let ((name '()) (pre '()) (add '()) (del '())
	   (duration 1) (cost 1) (priority 0) (update '()) (eval-object '()))
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
	(:update (set! update (cadr keyword-args)))
	(:eval-object (set! eval-object (cadr keyword-args)))
	(else (error "Illegal keyword: ~A" (first keyword-args))))
	  (set! keyword-args (cddr keyword-args)))
	(vector 'operator name pre add del update duration cost priority
		eval-object)))
*/

OPERATORP MakeOperator
(
	CELLP pcName,						/* operator name */
	CELLP pcSuccessor,					/* successor formula */
	CELLP pcDuration,					/* duration */
	CELLP pcCost,						/* cost */
	CELLP pcPriority					/* priority */
)
{
	OPERATORP po;

	/* create the operator and fill it in. */

	ENTER("MakeOperator",FALSE);
	po=(OPERATORP)MemAlloc(sizeof(struct Operator));
	po->pcName=(CELLP)MemAlloc(sizeof(CELL));
	po->pcName->pfForm=pcName->pfForm;
	po->pcSuccessor=pcSuccessor;
	po->pcDuration=pcDuration;
	po->pcCost=pcCost;
	po->pcPriority=pcPriority;

	/* link the operator into the global list (fifo) */

	poOperEnd=poOperEnd->poNext=po;
	EXIT("MakeOperator");
	return po;
}

/* MarkOperator

Description:
	Mark an operator (for garbage collection)
*/

void MarkOperator
(
	OPERATORP po
)
{
	ENTER("MarkOperator",FALSE);
	if(po->pcName)
		MarkFormula(po->pcName);
	if(po->pcSuccessor)
		MarkFormula(po->pcSuccessor);
	if(po->pcDuration)
		MarkFormula(po->pcDuration);
	if(po->pcCost)
		MarkFormula(po->pcCost);
	if(po->pcPriority)
		MarkFormula(po->pcPriority);
	ZoneMark(po);
	EXIT("MarkOperator");
}

/* ClearOps

Description:
	ReInitialize list of operators.
Scheme:

(define (clear-ops )
  (set! *operators* '()))
*/

void ClearOps(void)
{
	ENTER("ClearOps",FALSE);
	poOperators=NULL;
	poOperEnd=(OPERATORP)&poOperators;
	EXIT("ClearOps");
}

/* AddMacroOperator

Description:
	Add a new entry to the macro operator declaration table.
*/

MACROOPERATORP AddMacroOperator
(
	char *psName,						/* operator name */
	char *psDomain,						/* domain file path */
	CELLP pcGoal						/* macro goal formula */
)
{
	MACROOPERATORP pmo;

	/* create the operator and fill it in. */

	ENTER("AddMacroOperator",FALSE);
	pmo=(MACROOPERATORP)MemAlloc(sizeof(struct MacroOperator));
	pmo->psName=IdentAlloc(psName);
	pmo->psDomain=StrAlloc(psDomain);
	pmo->pcGoal=CopyCell(pcGoal);

	/* link the macro operator into the global list (fifo) */

	pmoMacroOperEnd=pmoMacroOperEnd->pmoNext=pmo;
	EXIT("AddMacroOperator");
	return pmo;
}

/* MarkMacroOperator

Description:
	Mark an macro operator table entry (for garbage collection)
*/

void MarkMacroOperator
(
	MACROOPERATORP pmo
)
{
	ENTER("MarkMacroOperator",FALSE);
	if(pmo->pcGoal)
		MarkFormula(pmo->pcGoal);
	ZoneMark(pmo);
	EXIT("MarkMacroOperator");
}

/* FreeMacroOperators

Description:
	Free the macro operators list.
*/

void FreeMacroOperators(void)
{
	pmoMacroOperators=NULL;
	pmoMacroOperEnd=(MACROOPERATORP)&pmoMacroOperators;
}

/* AddElidedOperator

Description:
	Add a new entry to the Elided operator declaration table.
*/

ELIDEDOPERATORP AddElidedOperator
(
	char *psName						/* operator name */
)
{
	ELIDEDOPERATORP peo;

	/* create the operator and fill it in. */

	ENTER("AddElidedOperator",FALSE);
	peo=(ELIDEDOPERATORP)MemAlloc(sizeof(struct ElidedOperator));
	peo->psName=IdentAlloc(psName);

	/* link the Elided operator into the global list (fifo) */

	peoElidedOperEnd=peoElidedOperEnd->peoNext=peo;
	EXIT("AddElidedOperator");
	return peo;
}

/* MarkElidedOperator

Description:
	Mark an Elided operator table entry (for garbage collection)
*/

void MarkElidedOperator
(
	ELIDEDOPERATORP peo
)
{
	ENTER("MarkElidedOperator",FALSE);
	ZoneMark(peo);
	EXIT("MarkElidedOperator");
}

/* FreeElidedOperators

Description:
	Free the Elided operators list.
*/

void FreeElidedOperators(void)
{
	peoElidedOperators=NULL;
	peoElidedOperEnd=(ELIDEDOPERATORP)&peoElidedOperators;
}

