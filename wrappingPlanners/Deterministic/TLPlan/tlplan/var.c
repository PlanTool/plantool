/* var.c

Copyright C, 1996 - 2001	F. Bacchus

Variables

	Routines to handle variables.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tlplan.h"
#include "eval.h"
#include "formula.h"
#include "util.h"
#include "var.h"

/* local function prototypes */

static BOOL NotVarOccursQ
(
	CELLP plVar,							/* atomic formula */
	CELLP plList							/* list of formulas */
);


/* VarQ

Description:
	Test if symbol is a variable.  x is a variable if it begins with '?'.
Scheme:

(define (var? x)
  (and (symbol? x) (char-ci=? (string-ref (symbol->string x) 0) #\?)))
*/

BOOL VarQ
(
	CELLP pcVar
)
{
	char ac[40];

	ENTER("VarQ",TRUE);
	EXIT("VarQ");
	return !pcVar->pfForm->pcArgs&&*GetName(pcVar->pfForm,ac)=='?';
}

/* VariablesIn

Description:
	Return list of all variables in a list of formulas.
Note:
	This routine is recursive.
	Don't pass in anything containing quantifiers.
	It ignores both pfVars and pfGenLit fields in quantifiers!
Scheme:

(define (variables-in exp)
  (cond ((var? exp) (list exp))
	((atom? exp) '())
	(else (union (variables-in (first exp))
			 (variables-in (rest exp))))))
*/

CELLP VariablesIn
(
	CELLP pcList						/* list of formulas */
)
{
	CELLP pcResult;						/* variable list */
	CELLP pc1,pc2;

	/* fill in variable list */

	ENTER("VariablesIn",TRUE);
	pcResult=NULL;						/* initialize result */

	for(pc1=pcList;pc1;pc1=pc1->pcNext)
	{
		if(VarQ(pc1))
		{
			if(NotVarOccursQ(pc1,pcResult))
			{
				pc2=CopyCell(pc1);
				pc2->pcNext=pcResult;
				pcResult=pc2;
			}
		}
		else if(pc1->pfForm->pcArgs)
			pcResult=FormulaUnion(VariablesIn(pc1->pfForm->pcArgs),pcResult);
	}
	EXIT("VariablesIn");
	return pcResult;
}

/* NotVarOccursQ

	Return TRUE if var does not occur in a list (of variables).
Scheme:

(define (not-occurs? var exp)
  (cond ((eq? var exp) '())
	((pair? exp)
	 (and (not-occurs? var (first exp))
		  (not-occurs? var (rest exp))))
	(else #t)))
*/

static BOOL NotVarOccursQ
(
	CELLP pcVar,						/* atomic formula */
	CELLP pcList						/* list of variables */
)
{
	CELLP pc;

	ENTER("NotVarOccursQ",TRUE);
	for(pc=pcList;pc;pc=pc->pcNext)
	{
		if(StringEqQ(IdentName(pcVar->pfForm),IdentName(pc->pfForm)))
		{
			EXIT("NotVarOccursQ");
			return FALSE;
		}
	}
	EXIT("NotVarOccursQ");
	return TRUE;
}

/* VariableFreeQ

Description:
	Return TRUE if expression has no variables.
Scheme:

(define (variable-free? exp)
  (cond ((null? exp) #t)
	((var? exp) #f)
	((atom? exp) #t)
	(else (and (variable-free? (first exp))
		   (variable-free? (rest exp))))))
*/

BOOL VariableFreeQ
(
	CELLP pcFormula
)
{
	CELLP pc;

	ENTER("VariableFreeQ",TRUE);
	for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
	{
		if(VarQ(pc))
		{
			EXIT("VariableFreeQ");
			return FALSE;
		}
		if(pc->pfForm->pcArgs)
			if(!VariableFreeQ(pc))
			{
				EXIT("VariableFreeQ");
				return FALSE;
			}
	}
	EXIT("VariableFreeQ");
	return TRUE;
}
