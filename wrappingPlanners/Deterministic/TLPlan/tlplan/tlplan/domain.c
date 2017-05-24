/* Notes:
	2.  Use of indexes and runtime symbolinfo lookup needs to be removed and
	replaced by pointers.
*/

/* domain.c

Copyright C, 1996 - 2001,  F. Bacchus

Define Domain

Functions that interface with the internal data structures that
maintain the world state (i.e., a database describing the current world).

The interface is defined in terms of list descriptions of worlds.
Worlds are specified as fully bound predicates and function
definitions.  Each literal is a formula with the following form:

	literal	        = (predicateName arg1 arg2 ... argn)
	predicateName   = symbol.
	argi			= symbol | number

Each function definition has the form:

	functionDefn	= (= (functionName arg1 arg2 ... argn) value)
	functionName	= symbol.
	argi			= symbol | number
	value			= symbol | number

Notes
1.  Nested function applications are not allowed in world descriptions.
	They are allowed in goals, temporal goals, and actions.

To distinguish between the different types of predicates
and functions the user must supply information about the symbols
used in the current domain. This is accomplished via various functions.

There are 7 types of domain symbols that the user has to inform
the system about:

1.  Described predicates:  These are predicates for which we
	maintain a list of all positive instances.
2.  Defined predicates:  These are predicates for which the user
	provides a definition by way of a first-order formula. This
	formula is interpreted in a manner that allows for recursive
	definitions.
3.  External predicates:  These are predicates whose truth is
	evaluated by calling a user defined library routine.

4.  Described functions:  These are functions for which we maintain
	a list of all function values. Functions values not on this
	list do not have a value.
5.  Defined functions:  These are functions for which the user
	provides a definition by way of a first-order formula. This
	formula is interpreted in a manner that allows for recursive
	definitions.
6.  External functions:  These are functions that are computed by
	a user provided library routine.

7.  External generators:  These are generators that are defined in
	a user provided library routine.

*/

#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "cc.h"
#include "compute.h"
#include "current.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "makegen.h"
#include "iface.h"
#include "makeform.h"
#include "oper.h"
#include "progress.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "user.h"
#include "util.h"
#include "world.h"
#include "zone.h"

/* local function prototypes */


/* global data */

/* The "symbolInfo" structure holds information about the domain's
described and defined symbols.

	name		symbol name
	type		predicate, function, generator, unknown
	arity		arity of predicate or function.
	evalType	unknown, described, defined, external
	eval		evaluation routine.
	formula		formula to evaluate
*/

SYMBOLINFO asiWorldSymbols[MAXSYMBOLS];	/* one symbol info structure for each domain symbol */
int nNumberOfWorldSymbols;				/* total number of described and defined symbols */
int nNumberOfDescribedSymbols;			/* number of described symbols */
int nNumberOfCheckedSymbols;			/* number of cycle checked described symbols */

BINDINGP pbGlobalVariables;				/* global variable bindings list */
CELLP pcSearchGlobalInit;				/* search-global initialization list */

/* MarkSymbolInfo

Description:
	Mark a symbol info entry (for garbage collection)
*/

void MarkSymbolInfo
(
	SYMBOLINFOP psi
)
{
	ENTER("MarkSymbolInfo",FALSE);
	if(psi->pcFormula)
		MarkFormulaList(psi->pcFormula);
	if(psi->paAction)
		ZoneMark(psi->paAction);
	EXIT("MarkSymbolInfo");
}

/* GetSymbolName

Description:
	Return the symbol name given symbol index.
Scheme:

(define (get-symbol-name index)
  (symbol-info-name (vector-ref *world-symbols* index)))
*/

char *GetSymbolName
(
	int nIndex
)
{
	char *ps;

	ENTER("GetSymbolName",TRUE);
	ps=SymbolInfoName(asiWorldSymbols+nIndex);
	EXIT("GetSymbolName");
	return ps;
}

/* GetSymbolInfoPtr

Description:
	Return a pointer to the symbol information for a symbol.
*/

SYMBOLINFOP GetSymbolInfoPtr
(
	char *psName
)
{
	SYMBOLINFOP psi;
	SYMBOLP psSymbol;

	ENTER("GetSymbolInfoPtr",FALSE);
	if(!SymbolLookup(psName,&psSymbol))
	{
		ErrorMessage("Symbol not found %s\n",psName);
		psi=NULL;
	}
	else if(psSymbol->nType!=ATOM_SYMBOLINFOP)
	{
		ErrorMessage("Symbol is invalid in this context: %s\n",psName);
		psi=NULL;
	}
	else
		psi=psSymbol->uValue.psiSymbolInfo;
	EXIT("GetSymbolInfoPtr");
	return psi;
}

/* Functions for setting up the domain symbol information ------------------- */

/* InsertSymbolInfo

Description:
	Add a new entry to the symbol info table.
Scheme:

(define (insert-symbol-info symbol type arity eval-type . optional-args)
  (let ((eval-object (if (null? optional-args)
			 '()
			 (first optional-args))))
	(put-symbol-index symbol *number-of-world-symbols*)
	(set! *world-symbols*
	  (apply vector
		 (append! (vector->list *world-symbols*)
			  (list (make-symbol-info :name symbol
						  :type  type
						  :arity arity
						  :eval-type eval-type
						  :eval-object eval-object)))))
	(set! *number-of-world-symbols* (+ 1 *number-of-world-symbols*))
	(list (get-symbol-index symbol)
	  (vector-ref *world-symbols* (get-symbol-index symbol)))))
*/

void InsertSymbolInfo
(
	char *psName,						/* symbol name */
	int nSymbolType,
	int nArity,							/* number of arguments */
	int nEvalType,
	void (*pfFunction)(void),			/* function pointer (optional) */
	CELLP pcFormula,					/* formula to evaluate (optional) */
	BOOL bRewritable					/* predicate rewritable flag (optional) */
)
{
	int nIndex;
	ACTIONP pa;
	SYMBOLINFOP psiSymbolInfo;

	ENTER("InsertSymbolInfo",FALSE);
	if(nNumberOfWorldSymbols>=MAXSYMBOLS)
		ErrorExit("Too many symbols: Recompile with larger MAXSYMBOLS\n");

	nIndex=nNumberOfWorldSymbols++;
	psiSymbolInfo=asiWorldSymbols+nIndex;
	if(!AddToSymbolTable(psName,ATOM_SYMBOLINFOP,(void *)(psiSymbolInfo)))
	{
		EXIT("InsertSymbolInfo");
		return;							/* symbol already defined or hash table full */
	}
	psiSymbolInfo->psName=IdentAlloc(psName);
	psiSymbolInfo->nSymbolType=nSymbolType;
	psiSymbolInfo->nArity=nArity;
	psiSymbolInfo->nEvalType=nEvalType;
	psiSymbolInfo->pfFunction=pfFunction;
	psiSymbolInfo->pcFormula=pcFormula;
	psiSymbolInfo->bRewritable=bRewritable;

	/* we always allocate the prototype action from the heap, 
	so we can garbage collect with impunity. */
	
	pa=psiSymbolInfo->paAction=(ACTIONP)MemAlloc(sizeof(ACTION));
	switch(nEvalType)
	{
		case EVAL_DESCRIBED:
			switch(nSymbolType)
			{
				case SYMBOL_PREDICATE:
					pa->pfEval=EvalDescPredicate;
					pa->pfIdle=EvalDescPredicate;
					pa->pfProgress=ProgressAtomic;
					pa->pfCurrent=CurrentAtomic;
					pa->pfGenerator=GenerateDescPredicate;
					pa->pfCompute=NoComputor;
					pa->pfMakeCCForm=MakeCCAtomicForm;
					break;
				case SYMBOL_FUNCTION:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
//					pa->pfGenerator=NoGenerator;
					pa->pfGenerator=GenerateDescPredicate;
					pa->pfCompute=ComputeDescFunction;
					pa->pfMakeCCForm=NoCCForm;
					break;
				case SYMBOL_MACRO:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=NoGenerator;
					pa->pfCompute=ComputeDescMacro;
					pa->pfMakeCCForm=NoCCForm;
					break;
				default:
					ErrorMessage("Unsupported described type, %s\n",psName);
			}
			break;
		case EVAL_DEFINED:
			switch(nSymbolType)
			{
				case SYMBOL_PREDICATE:
					pa->pfEval=EvalDefPredicate;
					pa->pfIdle=EvalDefPredicate;
					pa->pfProgress=ProgressAtomic;
					pa->pfCurrent=CurrentAtomic;
					pa->pfGenerator=NoGenerator;
					pa->pfCompute=NoComputor;
					pa->pfMakeCCForm=MakeCCAtomicForm;
					break;
				case SYMBOL_FUNCTION:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=NoGenerator;
					pa->pfCompute=ComputeDefFunction;
					pa->pfMakeCCForm=NoCCForm;
					break;
				case SYMBOL_GENERATOR:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=GenerateDefGenerator;
					pa->pfCompute=NoComputor;
					pa->pfMakeCCForm=NoCCForm;
					break;
				case SYMBOL_MACRO:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=NoGenerator;
//					pa->pfCompute=ComputeDefMacro;
					pa->pfCompute=ComputeDefFunction;
					pa->pfMakeCCForm=NoCCForm;
					break;
				default:
					ErrorMessage("Unsupported defined type, %s\n",psName);
			}
			break;
		case EVAL_EXTERNAL:
			switch(nSymbolType)
			{
				case SYMBOL_PREDICATE:
					pa->pfEval=(EVALP)SymbolInfoFunction(psiSymbolInfo);
					pa->pfIdle=(EVALP)SymbolInfoFunction(psiSymbolInfo);
					pa->pfProgress=ProgressAtomic;
					pa->pfCurrent=CurrentAtomic;
					pa->pfGenerator=NoGenerator;
					pa->pfCompute=NoComputor;
					pa->pfMakeCCForm=MakeCCAtomicForm;
					break;
				case SYMBOL_FUNCTION:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=NoGenerator;
					pa->pfCompute=(COMPUTEP)SymbolInfoFunction(psiSymbolInfo);
					pa->pfMakeCCForm=NoCCForm;
					break;
				case SYMBOL_GENERATOR:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=(GENERATORP)SymbolInfoFunction(psiSymbolInfo);
					pa->pfCompute=NoComputor;
					pa->pfMakeCCForm=NoCCForm;
					break;
				case SYMBOL_MACRO:
					pa->pfEval=NoEvaluator;
					pa->pfIdle=NoIdler;
					pa->pfProgress=NoProgressor;
					pa->pfCurrent=NoCurrentor;
					pa->pfGenerator=NoGenerator;
					pa->pfCompute=(COMPUTEP)SymbolInfoFunction(psiSymbolInfo);
					pa->pfMakeCCForm=NoCCForm;
					break;
				default:
					ErrorMessage("Unsupported external type, %s\n",psName);
			}
			break;
		default:
			ErrorMessage("Unsupported symbol type, %s\n",psName);
	}
	EXIT("InsertSymbolInfo");
}

/* Symbol type predicates ------------------------------------------------------ */

BOOL PredicateQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
)
{
	BOOL b;

	ENTER("PredicateQ",FALSE);
	if(psiSymbolInfo)
		b=SymbolInfoSymbolType(psiSymbolInfo)==SYMBOL_PREDICATE;
	else
		b=FALSE;
	EXIT("PredicateQ");
	return b;
}

BOOL FunctionQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
)
{
	BOOL b;

	ENTER("FunctionQ",FALSE);
	b=SymbolInfoSymbolType(psiSymbolInfo)==SYMBOL_FUNCTION;
	EXIT("FunctionQ");
	return b;
}

BOOL GeneratorQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
)
{
	BOOL b;

	ENTER("GeneratorQ",FALSE);
	b=SymbolInfoSymbolType(psiSymbolInfo)==SYMBOL_GENERATOR;
	EXIT("GeneratorQ");
	return b;
}

/* Symbol class predicates ----------------------------------------------------- */

/* DescribedQ

Description:
	This symbol is part of the world database.
*/

BOOL DescribedQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
)
{
	BOOL b;

	ENTER("DescribedQ",FALSE);
	b=SymbolInfoEvalType(psiSymbolInfo)==EVAL_DESCRIBED;
	EXIT("DescribedQ");
	return b;
}

/* DefinedQ

Description:
	This symbol is defined by a first order formula.
*/

BOOL DefinedQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
)
{
	BOOL b;

	ENTER("DefinedQ",FALSE);
	b=SymbolInfoEvalType(psiSymbolInfo)==EVAL_DEFINED;
	EXIT("DefinedQ");
	return b;
}

/* ExternalQ

Description:
	This symbol is defined in a user supplied library.
*/

BOOL ExternalQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
)
{
	BOOL b;

	ENTER("ComputedQ",FALSE);
	b=SymbolInfoEvalType(psiSymbolInfo)==EVAL_EXTERNAL;
	EXIT("ComputedQ");
	return b;
}

/* Miscellaneous --------------------------------------------------------------- */

/* StringToSymbolType

Description:
	Convert a symbol type string to a symbol type constant.
*/

int StringToSymbolType
(
	char *psString
)
{
	int n;

	ENTER("StringToSymbolType",TRUE);
	if(StringEqQ(psString,apsStringTab[STRING_UNKNOWN]))
		n=SYMBOL_UNKNOWN;
	else if(StringEqQ(psString,apsStringTab[STRING_PREDICATE]))
		n=SYMBOL_PREDICATE;
	else if(StringEqQ(psString,apsStringTab[STRING_FUNCTION]))
		n=SYMBOL_FUNCTION;
	else if(StringEqQ(psString,apsStringTab[STRING_GENERATOR]))
		n=SYMBOL_GENERATOR;
	else if(StringEqQ(psString,apsStringTab[STRING_MACRO]))
		n=SYMBOL_MACRO;
	else
	{
		ErrorMessage("StringToSymbolType:  Unknown type: %s\n",psString);
		n=SYMBOL_UNKNOWN;
	}
	EXIT("StringToSymbolType");
	return n;
}

/* StringToVariableType

Description:
	Convert a variable type string to a variable type constant.
*/

//int StringToVariableType
//(
//	char *psString
//)
//{
//	int n;
//
//	ENTER("StringToVariableType",TRUE);
//	if(StringEqQ(psString,apsStringTab[STRING_DOMAIN_GLOBAL]))
//		n=DOMAIN_GLOBAL;
//	else if(StringEqQ(psString,apsStringTab[STRING_SEARCH_GLOBAL]))
//		n=SEARCH_GLOBAL;
//	else if(StringEqQ(psString,apsStringTab[STRING_WORLD_GLOBAL]))
//		n=WORLD_GLOBAL;
//	else
//	{
//		ErrorMessage("StringToVariableType:  Unknown variable type: %s\n",psString);
//		n=VARIABLE_UNKNOWN;
//	}
//	EXIT("StringToVariableType");
//	return n;
//}

/* Accessors to defined predicate eval */

CELLP GetDefParameters
(
	SYMBOLINFOP psiSymbolInfo
)
{
	ENTER("GetDefParameters",TRUE);
	EXIT("GetDefParameters");
	return psiSymbolInfo->pcFormula->pfForm->pcVars;
}

CELLP GetDefLocals					/* for defined functions only */
(
	SYMBOLINFOP psiSymbolInfo
)
{
	ENTER("GetDefLocals",TRUE);
	EXIT("GetDefLocals");
	return psiSymbolInfo->pcFormula->pfForm->pcGenLit;
}

CELLP GetDefFormula
(
	SYMBOLINFOP psiSymbolInfo
)
{
	ENTER("GetDefFormula",TRUE);
	EXIT("GetDefFormula");
	return psiSymbolInfo->pcFormula->pfForm->pcArgs;
}


