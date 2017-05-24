/* color.c -- ColorFormula support routines

Copyright C, 1997 - 2001, Fahiem Bacchus

*/

#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "color.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "list.h"
#include "progress.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"

// local function prototypes

static int PColorTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* term to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel,							/* recursion level (initialize to 0) */
	int nCol							/* current column */
);
static int EColorTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* term to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel,							/* recursion level (initialize to 0) */
	int nCol							/* current column */
);

static char *psBlanks="                                                                                ";

/* PColorFormulaList

Description:
	Print a list of formulas in lisp format, marking sub-formulas that progress to false.
*/

DECLSPEC void PColorFormulaList
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* (list of) formulas to display */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;							/* formula pointer */

	ENTER("PColorFormulaList",TRUE);
	if(pcFormula->pcNext)
	{
		CommandPuts("(",pfStream);
		for(pc=pcFormula;pc;pc=pc->pcNext)
		{
			PColorFormula(pfStream,pc,plpLinearPlan,pbBindings,1);
			if(pc->pcNext)
				CommandPrintf(pfStream," ");
		}
		CommandPuts(")\n",pfStream);
	}
	else
		PrintFormula(pfStream,pcFormula,0);
	EXIT("PColorFormulaList");
}

/* PColorFormula

Description:
	Print a formula in lisp format, marking sub-formulas that progress to false
*/

DECLSPEC void PColorFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel							/* recursion level (initialize to 0) */
)
{
	CELLP pc;							/* formula pointer */
	int nCol;							/* current column location */
	BINDINGP pb;						/* bindings pointer */

	ENTER("PColorFormula",TRUE);
	pb=pbBindings;
	if(nLevel)
		CommandPuts("\n",pfStream);
	CommandPrintf(pfStream,"%.*s(",2*nLevel,psBlanks);
	nCol=2*nLevel+1;
	if(pcFormula)
	{
		PColorTerm(pfStream,pcFormula,plpLinearPlan,pbBindings,nLevel,nCol);
		nCol=2*(nLevel+1);
		if(pcFormula->pfForm->pcVars)
		{
			CommandPrintf(pfStream,"\n%.*s(",2*(nLevel+1),psBlanks);
			nCol++;
			for(pc=pcFormula->pfForm->pcVars;pc;pc=pc->pcNext)
			{
				nCol=PColorTerm(pfStream,pc,plpLinearPlan,pbBindings,nLevel+1,nCol);
				if(pc->pcNext)
				{
					CommandPrintf(pfStream," ");
					nCol++;
				}
			}
			CommandPrintf(pfStream,")");
		}
		nCol=2*(nLevel+1);
		if(ExistsFormQ(pcFormula)||ForAllFormQ(pcFormula))
		{
			for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PrintFormula(pfStream,pc,nLevel+1);
				else
				{
					CommandPrintf(pfStream,"(");
					nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
					CommandPrintf(pfStream,")");
				}
			}
			nCol=2*(nLevel+1);
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PrintFormula(pfStream,pc,nLevel+1);
				else
					nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
			}
		}
		else
		{
			for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PrintFormula(pfStream,pc,nLevel+1);
				else
				{
					CommandPrintf(pfStream,"(");
					nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
					CommandPrintf(pfStream,")");
				}
			}

			// special processing for binding formula
		
			if(BindingFormQ(pcFormula))
				pb=ExtendBindings(GetBindingVars(pcFormula),GetBindingVals(pcFormula),pbBindings);

			nCol=2*(nLevel+1);
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PColorFormula(pfStream,pc,plpLinearPlan,pb,nLevel+1);
				else
					nCol=PColorTerm(pfStream,pc,plpLinearPlan,pb,nLevel+1,nCol);
			}
		}
	}
	CommandPrintf(pfStream,")");
	if(!nLevel)
		CommandPrintf(pfStream,"\n");
	EXIT("PColorFormula");
}

/* PColorTerm

Description:
	Print a single term of a formula, marking false terms
*/

static int PColorTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* term to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel,							/* recursion level (initialize to 0) */
	int nCol							/* current column */
)
{
	int nLength;						/* string length */
	char ac[40];						/* string buffer */
	int immutable;
	CELLP pc;							/* formula pointer */
	char *ps;							/* string pointer */

	ENTER("PColorTerm",TRUE);
	if(pcFormula->pfForm->paAction->pfProgress!=NoProgressor)
	{
		pc=(*pcFormula->pfForm->paAction->pfProgress)(pcFormula,plpLinearPlan,pbBindings,&immutable);
		if(FalseFormQ(pc))
			CommandPuts("~",pfStream);
	}
	switch(pcFormula->pfForm->nType)
	{
		case ATOM_STRING:
			ps=GetName(pcFormula->pfForm,ac);
			nLength=strlen(ps)+2;
			if(nCol>2*(nLevel+1)&&nCol+nLength>80)
			{
				CommandPrintf(pfStream,"\n%.*s",2*(nLevel+1),psBlanks);
				nCol=2*(nLevel+1);
			}
			CommandPrintf(pfStream,"\"%s\"",ps);	// notice quotes!!!
			nCol+=nLength;
			break;
		case ATOM_IDENT:
		case ATOM_INFINITY:
		case ATOM_ISPECP:
		case ATOM_FUNCTIONP:
		case ATOM_SYMBOLINFOP:
		case ATOM_INTEGER:
		case ATOM_FLOAT:
			ps=GetName(pcFormula->pfForm,ac);
			nLength=strlen(ps);
			if(nCol>2*(nLevel+1)&&nCol+nLength>80)
			{
				CommandPrintf(pfStream,"\n%.*s",2*(nLevel+1),psBlanks);
				nCol=2*(nLevel+1);
			}
			CommandPrintf(pfStream,"%s",ps);
			nCol+=nLength;
			break;
		case ATOM_OPERATORP:
			{
				OPERATORP po;
				po=pcFormula->pfForm->uValue.poOperator;
				PrintFormula(pfStream,po->pcName,nLevel);
			}
			break;
		case ATOM_LISTP:
			PrintList(pfStream,pcFormula->pfForm->uValue.plList,nLevel+1);
			break;
		default:
			ErrorMessage("PColorTerm:  Unsupported atom type %d\n",
				pcFormula->pfForm->nType);
	}
	EXIT("PColorTerm");
	return nCol;
}

// --------------------

/* EColorFormulaList

Description:
	Print a list of formulas in lisp format, marking false sub-formulas.
*/

DECLSPEC void EColorFormulaList
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* (list of) formulas to display */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;							/* formula pointer */

	ENTER("EColorFormulaList",TRUE);
	if(pcFormula->pcNext)
	{
		CommandPuts("(",pfStream);
		for(pc=pcFormula;pc;pc=pc->pcNext)
		{
			EColorFormula(pfStream,pc,plpLinearPlan,pbBindings,1);
			if(pc->pcNext)
				CommandPrintf(pfStream," ");
		}
		CommandPuts(")\n",pfStream);
	}
	else
		PrintFormula(pfStream,pcFormula,0);
	EXIT("EColorFormulaList");
}

/* EColorFormula

Description:
	Print a formula in lisp format, marking false sub-formulas.
*/

DECLSPEC void EColorFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel							/* recursion level (initialize to 0) */
)
{
	CELLP pc;							/* formula pointer */
	int nCol;							/* current column location */
	BINDINGP pb;						/* bindings pointer */

	ENTER("EColorFormula",TRUE);
	pb=pbBindings;
	if(nLevel)
		CommandPuts("\n",pfStream);
	CommandPrintf(pfStream,"%.*s(",2*nLevel,psBlanks);
	nCol=2*nLevel+1;
	if(pcFormula)
	{
		EColorTerm(pfStream,pcFormula,plpLinearPlan,pbBindings,nLevel,nCol);
		nCol=2*(nLevel+1);
		if(pcFormula->pfForm->pcVars)
		{
			CommandPrintf(pfStream,"\n%.*s(",2*(nLevel+1),psBlanks);
			nCol++;
			for(pc=pcFormula->pfForm->pcVars;pc;pc=pc->pcNext)
			{
				nCol=EColorTerm(pfStream,pc,plpLinearPlan,pbBindings,nLevel+1,nCol);
				if(pc->pcNext)
				{
					CommandPrintf(pfStream," ");
					nCol++;
				}
			}
			CommandPrintf(pfStream,")");
		}
		nCol=2*(nLevel+1);
		if(ExistsFormQ(pcFormula)||ForAllFormQ(pcFormula))
		{
			for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PrintFormula(pfStream,pc,nLevel+1);
				else
				{
					CommandPrintf(pfStream,"(");
					nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
					CommandPrintf(pfStream,")");
				}
			}
			nCol=2*(nLevel+1);
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PrintFormula(pfStream,pc,nLevel+1);
				else
					nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
			}
		}
		else
		{
			for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					PrintFormula(pfStream,pc,nLevel+1);
				else
				{
					CommandPrintf(pfStream,"(");
					nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
					CommandPrintf(pfStream,")");
				}
			}

			// special processing for binding formula
		
			if(BindingFormQ(pcFormula))
				pb=ExtendBindings(GetBindingVars(pcFormula),GetBindingVals(pcFormula),pbBindings);

			nCol=2*(nLevel+1);
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				CommandPrintf(pfStream," ");
				nCol++;
				if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
					EColorFormula(pfStream,pc,plpLinearPlan,pb,nLevel+1);
				else
					nCol=EColorTerm(pfStream,pc,plpLinearPlan,pb,nLevel+1,nCol);
			}
		}
	}
	CommandPrintf(pfStream,")");
	if(!nLevel)
		CommandPrintf(pfStream,"\n");
	EXIT("EColorFormula");
}

/* EColorTerm

Description:
	Print a single term of a formula, marking false terms
*/

static int EColorTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* term to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel,							/* recursion level (initialize to 0) */
	int nCol							/* current column */
)
{
	int nLength;						/* string length */
	char ac[40];						/* string buffer */
	char *ps;							/* string pointer */

	ENTER("EColorTerm",TRUE);
	if(pcFormula->pfForm->paAction->pfEval!=NoEvaluator)
	{
		if(!(*pcFormula->pfForm->paAction->pfEval)(pcFormula,plpLinearPlan,pbBindings))
			CommandPuts("~",pfStream);
	}
	switch(pcFormula->pfForm->nType)
	{
		case ATOM_STRING:
			ps=GetName(pcFormula->pfForm,ac);
			nLength=strlen(ps)+2;
			if(nCol>2*(nLevel+1)&&nCol+nLength>80)
			{
				CommandPrintf(pfStream,"\n%.*s",2*(nLevel+1),psBlanks);
				nCol=2*(nLevel+1);
			}
			CommandPrintf(pfStream,"\"%s\"",ps);	// notice quotes!!!
			nCol+=nLength;
			break;
		case ATOM_IDENT:
		case ATOM_INFINITY:
		case ATOM_ISPECP:
		case ATOM_FUNCTIONP:
		case ATOM_SYMBOLINFOP:
		case ATOM_INTEGER:
		case ATOM_FLOAT:
			ps=GetName(pcFormula->pfForm,ac);
			nLength=strlen(ps);
			if(nCol>2*(nLevel+1)&&nCol+nLength>80)
			{
				CommandPrintf(pfStream,"\n%.*s",2*(nLevel+1),psBlanks);
				nCol=2*(nLevel+1);
			}
			CommandPrintf(pfStream,"%s",ps);
			nCol+=nLength;
			break;
		case ATOM_OPERATORP:
			{
				OPERATORP po;
				po=pcFormula->pfForm->uValue.poOperator;
				PrintFormula(pfStream,po->pcName,nLevel);
			}
			break;
		case ATOM_LISTP:
			PrintList(pfStream,pcFormula->pfForm->uValue.plList,nLevel+1);
			break;
		default:
			ErrorMessage("EColorTerm:  Unsupported atom type %d\n",
				pcFormula->pfForm->nType);
	}
	EXIT("EColorTerm");
	return nCol;
}

