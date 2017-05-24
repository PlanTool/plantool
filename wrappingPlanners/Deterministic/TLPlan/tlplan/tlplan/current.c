/* current.c -- Temporal Evaluator

Copyright C, 1999  F. Bacchus

NOTA BENE!  

This module contains 3 value logic...  
Don't assume that !FALSE == TRUE, or vice-versa!

This module contains functions to evaluate the current value of temporal 
formulas expressed in LTL or MTL.

The current algorithm, is based directly on progression.  However, instead
of generating formulas as a result, it treats progression as a 3 value logic
and generates TRUE, FALSE and NOT_FALSE instead.  The intent is to avoid
generating any garbage whenever progression would generate the result (false).

Here, we can avoid many of the simplifications performed during progression,
and cut short many computations on the first sighting of NOT_FALSE.

Nomenclature for algorithm descriptions:
	c() current
	s() shift
	?() evaluate, with TRUE or FALSE result
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
#include "compute.h"
#include "current.h"
#include "eval.h"
#include "formula.h"
#include "idle.h"
#include "iface.h"
#include "interval.h"
#include "makeform.h"
#include "makegen.h"
#include "tlparse.h"
#include "util.h"
#include "world.h"

/* local structures and definitions */


/* local function prototypes */


/* Boolean Formulas ------------------------------------------------------------ */

/* CurrentFalse

Description:
	Current a (false) formula... return FALSE.
Algorithm:
	c(false) => FALSE
*/

int CurrentFalse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("CurrentFalse",TRUE);
	EXIT("CurrentFalse");
	return FALSE;
}

/* CurrentTrue

Description:
	Current a (true) formula... return TRUE.
Algorithm:
	c(true) => TRUE
*/

int CurrentTrue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("CurrentTrue",TRUE);
	EXIT("CurrentTrue");
	return TRUE;
}

/* CurrentNot

Description:
	Current a not Formula checking for true/false.
Algorithm:
	c(not form) => (not c(form))
*/

int CurrentNot
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArg1;					/* pointer to sub formula */
	int nArg1;						/* current value of sub formula */

	ENTER("CurrentNot",TRUE);
	pcArg1=pcFormula->pfForm->pcArgs;
	nArg1=(*pcArg1->pfForm->paAction->pfCurrent)(pcArg1,plpLinearPlan,pbBindings);
	if(nArg1==FALSE)
		nArg1=TRUE;
	else if(nArg1==TRUE)
		nArg1=FALSE;
	EXIT("CurrentNot");
	return nArg1;
}

/* CurrentAnd

Description:
	Current an (and... ) formula checking for true/false.
Algorithm:
	c(and form ...) => (and c(form) ...)
*/

int CurrentAnd
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcSubForm;					/* pointer to sub formula */
	BOOL bSubForms;						/* sub formula seen */
	int nSubForm;						/* current value of sub formula */

	ENTER("CurrentAnd",TRUE);
	bSubForms=FALSE;
	nSubForm=TRUE;

	for(pcSubForm=pcFormula->pfForm->pcArgs;pcSubForm;pcSubForm=pcSubForm->pcNext)
	{
		nSubForm=(*pcSubForm->pfForm->paAction->pfCurrent)(pcSubForm,plpLinearPlan,pbBindings);
		if(nSubForm==FALSE)			
		{
			bSubForms=FALSE;
			break;
		}
		else if(nSubForm==NOT_FALSE)
			bSubForms=TRUE;
	}

	if(bSubForms)
		nSubForm=NOT_FALSE;
	EXIT("CurrentAnd");
	return nSubForm;
}

/* CurrentOr

Description:
	Current an (or... ) formula checking for true/false.
Algorithm:
	c(or form ...) => (or c(form) ...)
*/

int CurrentOr
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcSubForm;					/* pointer to sub formula */
	BOOL bSubForms;						/* sub formulas seen */
	int nSubForm;						/* current value of sub formula */

	ENTER("CurrentOr",TRUE);
	bSubForms=FALSE;
	nSubForm=FALSE;

	for(pcSubForm=pcFormula->pfForm->pcArgs;pcSubForm;pcSubForm=pcSubForm->pcNext)
	{
		nSubForm=(*pcSubForm->pfForm->paAction->pfCurrent)(pcSubForm,plpLinearPlan,pbBindings);
		if(nSubForm==TRUE)
		{
			bSubForms=FALSE;
			break;
		}
		else if(nSubForm==NOT_FALSE)
			bSubForms=TRUE;
	}
	if(bSubForms)
		nSubForm=NOT_FALSE;
	EXIT("CurrentOr");
	return nSubForm;
}

/* CurrentXor

Description:
	Current an (xor... ) formula checking for true/false.
Algorithm:
	c(xor form ...) => (xor c(form) ...)
*/

int CurrentXor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcSubForm;					/* pointer to sub formula */
	BOOL bFoundOne;						/* found a TRUE term */
	BOOL bFoundTwo;						/* found atleast two TRUE terms */
	BOOL bSubForms;						/* found sub formulas */
	int nSubForm;						/* current value of sub formula */

	ENTER("CurrentXor",TRUE);
	bFoundOne=FALSE;
	bFoundTwo=FALSE;
	bSubForms=FALSE;

	for(pcSubForm=pcFormula->pfForm->pcArgs;pcSubForm;pcSubForm=pcSubForm->pcNext)
	{
		nSubForm=(*pcSubForm->pfForm->paAction->pfCurrent)(pcSubForm,plpLinearPlan,pbBindings);
		if(nSubForm==TRUE)
		{
			if(bFoundOne)				/* if atleast two true terms */
			{
				bFoundTwo=TRUE;
				break;
			}
			else						/* first true term */
				bFoundOne=TRUE;
		}
		else if(nSubForm==NOT_FALSE)	/* found at least one sub formula */
			bSubForms=TRUE;
	}

	if(bFoundTwo)
		nSubForm=FALSE;
	else if(bSubForms)
		nSubForm=NOT_FALSE;
	else
		nSubForm=bFoundOne;
	EXIT("CurrentXor");
	return nSubForm;
}

/* CurrentImplies

Description:
	Current an (implies... ) formula checking for true/false.
Algorithm:
	c(implies a c) => (implies c(a) c(c))
*/

int CurrentImplies
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArg1;
	CELLP pcArg2;
	int nArg1;
	int nArg2;

	ENTER("CurrentImplies",TRUE);
	pcArg1=pcFormula->pfForm->pcArgs;
	nArg1=(*pcArg1->pfForm->paAction->pfCurrent)(pcArg1,plpLinearPlan,pbBindings);
	if(nArg1==FALSE)
		nArg1=TRUE;
	else
	{
		pcArg2=pcArg1->pcNext;
		nArg2=(*pcArg2->pfForm->paAction->pfCurrent)(pcArg2,plpLinearPlan,pbBindings);
		if(nArg2==TRUE)
			nArg1=TRUE;
		else if(nArg1==TRUE)
			nArg1=nArg2;
	}
	EXIT("CurrentImplies");
	return nArg1;
}

/* CurrentIfThenElse

Description:
	Current an (if-then-else... ) formula checking for true/false.
Algorithm:
	c(if-then-else a t f) => (if-then-else c(a) c(t) c(f))
*/

int CurrentIfThenElse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArg1;
	CELLP pcArg2;
	CELLP pcArg3;
	int nArg1;
	int nArg2;
	int nArg3;

	ENTER("CurrentIfThenElse",TRUE);
	pcArg1=pcFormula->pfForm->pcArgs;
	pcArg2=pcArg1->pcNext;
	pcArg3=pcArg2->pcNext;

	nArg1=(*pcArg1->pfForm->paAction->pfCurrent)(pcArg1,plpLinearPlan,pbBindings);
	if(nArg1==TRUE)
	{
		nArg2=(*pcArg2->pfForm->paAction->pfCurrent)(pcArg2,plpLinearPlan,pbBindings);
		EXIT("CurrentIfThenElse");
		return nArg2;
	}
	if(nArg1==FALSE)
	{
		nArg3=(*pcArg3->pfForm->paAction->pfCurrent)(pcArg3,plpLinearPlan,pbBindings);
		EXIT("CurrentIfThenElse");
		return nArg3;
	}

	nArg2=(*pcArg2->pfForm->paAction->pfCurrent)(pcArg2,plpLinearPlan,pbBindings);
	nArg3=(*pcArg3->pfForm->paAction->pfCurrent)(pcArg3,plpLinearPlan,pbBindings);
	if(nArg2==TRUE)
	{
		if(nArg3==TRUE)
			nArg1=TRUE;
		else if(nArg3!=FALSE)
			nArg1=NOT_FALSE;
		EXIT("CurrentIfThenElse");
		return nArg1;
	}
	if(nArg2==FALSE)
	{
		if(nArg3==FALSE)
			nArg1=FALSE;
		EXIT("CurrentIfThenElse");
		return nArg1;
	}
	EXIT("CurrentIfThenElse");
	return nArg1;
}

/* Quantified Formulas --------------------------------------------------------- */

/* CurrentForAll (Quantified And)

Description:
	Current a (forall... ) quantifier, by reducing to instances.
Algorithm:
	c(forall vars gen form) => (and (binding vars vals c(form)) ...)
*/

int CurrentForAll
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArgs;
	void *pvContext;
	int nForm;							/* current value of formula */
	BOOL bSubForms;						/* sub formula seen */
	int nBaseSP;						// saved base btree stack index

	ENTER("CurrentForAll",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* make room */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	if(!pcArgs)
	{
		nForm=FALSE;
		EXIT("CurrentForAll");
		return nForm;
	}
	
	nForm=TRUE;
	bSubForms=FALSE;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		nForm=(*pcArgs->pfForm->paAction->pfCurrent)(pcArgs,plpLinearPlan,pbBindings);
		if(nForm==FALSE)
		{
			bSubForms=FALSE;
			SetBTreeSP(nBaseSP);		// restore btree stack pointer
			break;
		}
		else if(nForm==NOT_FALSE)
			bSubForms=TRUE;
	}
	if(bSubForms)
		nForm=NOT_FALSE;
	EXIT("CurrentForAll");
	return nForm;
}

/* CurrentExists (Quantified Or)

Description:
	Current an (exists... ) quantifier, by reducing to instances.
Algorithm:
	c(exists vars gen form) => (or (binding vars vals c(form)) ...)
*/

int CurrentExists
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArgs;
	void *pvContext;
	BOOL bSubForms;
	int nForm;
	int nBaseSP;						// saved base btree stack index

	ENTER("CurrentExists",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* make room */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	bSubForms=FALSE;
	nForm=FALSE;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!pcArgs)
			nForm=TRUE;
		else
			nForm=(*pcArgs->pfForm->paAction->pfCurrent)(pcArgs,plpLinearPlan,pbBindings);
		if(nForm==TRUE)
		{
			bSubForms=FALSE;
			SetBTreeSP(nBaseSP);		// restore btree stack pointer
			break;
		}
		else if(nForm==NOT_FALSE)
			bSubForms=TRUE;
	}

	if(bSubForms)
		nForm=NOT_FALSE;
	EXIT("CurrentExists");
	return nForm;
}

/* CurrentExistsX (Quantified Xor)

Description:
	Current an exists! quantifier, by reducing to instances.
Algorithm:
	c(exists! vars gen form) => (xor (binding vars vals c(form)) ...)
*/

int CurrentExistsX
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArgs;
	void *pvContext;
	BOOL bFoundOne;
	BOOL bFoundTwo;
	BOOL bSubForms;
	int nForm;
	int nBaseSP;						// saved base btree stack index

	ENTER("CurrentExistsX",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	/* make room */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();

	bFoundOne=FALSE;
	bFoundTwo=FALSE;
	bSubForms=FALSE;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!pcArgs)
			nForm=TRUE;
		else
			nForm=(*pcArgs->pfForm->paAction->pfCurrent)(pcArgs,plpLinearPlan,pbBindings);
		if(nForm==TRUE)
		{
			if(bFoundOne)
			{
				bFoundTwo=TRUE;
				bSubForms=FALSE;
				SetBTreeSP(nBaseSP);	// restore btree stack pointer
				break;
			}
			else
				bFoundOne=TRUE;
		}
		else if(nForm==NOT_FALSE)
			bSubForms=TRUE;
	}

	if(bFoundTwo)
		nForm=FALSE;
	else if(bSubForms)
		nForm=NOT_FALSE;
	else
		nForm=bFoundOne;
	EXIT("CurrentExistsX");
	return nForm;
}

/* Temporal Formulas ----------------------------------------------------------- */

/* CurrentAlways (Temporal And)

Description:
	Current an (always... ) modality.
Algorithm:
	c(always form) => (and c(form) (always form))
*/

int CurrentAlways
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArg1;
	int nArg1;

	ENTER("CurrentAlways",TRUE);
	pcArg1=pcFormula->pfForm->pcArgs;
	nArg1=(*pcArg1->pfForm->paAction->pfCurrent)(pcArg1,plpLinearPlan,pbBindings);
	if(nArg1!=FALSE)
		nArg1=NOT_FALSE;
	EXIT("CurrentAlways");
	return nArg1;
}

/* CurrentEventually (Temporal Or)

Description:
	Current an (eventually ...) modality.
Algorithm:
	c(eventually form) => (or c(form) (eventually form))
*/

int CurrentEventually
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArg1;
	int nArg1;

	ENTER("CurrentEventually",TRUE);
	pcArg1=pcFormula->pfForm->pcArgs;
	nArg1=(*pcArg1->pfForm->paAction->pfCurrent)(pcArg1,plpLinearPlan,pbBindings);
	if(nArg1!=TRUE)
		nArg1=NOT_FALSE;
	EXIT("CurrentEventually");
	return nArg1;
}

/* CurrentNext

Description:
	Current a (next ...) modality.
Algorithm:
	c(next form) => form
*/

int CurrentNext
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ENTER("CurrentNext",TRUE);
	EXIT("CurrentNext");
	return NOT_FALSE;
}

/* CurrentUntil

Description:
	Current an (until ...) modality of the form (condition UNTIL achievement).
Algorithm:
	c(until c a) => (or c(a) (and c(c) (until c a)))
*/

int CurrentUntil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcAchievement;
	CELLP pcCondition;
	int nAchievement;
	int nCondition;

	ENTER("CurrentUntil",TRUE);
	pcCondition=pcFormula->pfForm->pcArgs;
	pcAchievement=pcCondition->pcNext;
	nAchievement=(*pcAchievement->pfForm->paAction->pfCurrent)(pcAchievement,plpLinearPlan,pbBindings);
	if(nAchievement==TRUE)
	{
		EXIT("CurrentUntil");
		return TRUE;
	}
	nCondition=(*pcCondition->pfForm->paAction->pfCurrent)(pcCondition,plpLinearPlan,pbBindings);
	if(nCondition==TRUE)
		nCondition=NOT_FALSE;

	if(nAchievement!=FALSE)
	{
		if(nCondition==FALSE)
			nCondition=nAchievement;
		else 
			nCondition=NOT_FALSE;
	}
	EXIT("CurrentUntil");
	return nCondition;
}

/* MTL Timed Temporal Formulas ------------------------------------------------- */

/* CurrentTAlways (Timed Temporal And)

Description:
	Current a (talways... ) modality.
*/

int CurrentTAlways
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ISPECP pisISpec;
	CELLP pcForm;
	int nForm;

	ENTER("CurrentTAlways",TRUE);
	pisISpec=GetBFISpec(pcFormula)->pfForm->uValue.piISpec;
	if(ISpecLt0(pisISpec))
	{
		nForm=TRUE;							/* timed out */
		EXIT("CurrentTAlways");
		return nForm;
	}
	if(ISpec0In(pisISpec))
	{
		pcForm=GetBFFormulas(pcFormula);
		nForm=(*pcForm->pfForm->paAction->pfCurrent)(pcForm,plpLinearPlan,pbBindings);
		if(nForm==TRUE)
			nForm=NOT_FALSE;
	}
	else
		nForm=NOT_FALSE;
	EXIT("CurrentTAlways");
	return nForm;
}

/* CurrentDelta

Description:
	Current a (delta... ) formula.
	Copy the formula, shift it and then current that.
Algorithm:
	c(delta form) => c(s(form))
*/

int CurrentDelta
(
	CELLP pcFormula,					/* calling formula */
	LINEARPLANP plpLinearPlan,			/* current world/action */
	BINDINGP pbBindings					/* current variables bindings */
)
{
	CELLP pcISpec;						/* interval to shift */
	CELLP pcTail;						/* trailing arguments */
	CELLP pcShiftedForm;				/* shifted formula */
	int nForm;							/* current value of form */

	ENTER("CurrentDelta",TRUE);

	/* copy the formula */
	
	pcShiftedForm=CopyCell(pcFormula->pfForm->pcArgs);
	pcShiftedForm->pfForm=CopyAlloc(pcShiftedForm->pfForm,sizeof(FORMULA));
	pcISpec=pcShiftedForm->pfForm->pcArgs;
	pcTail=pcISpec->pcNext;

	/* shift the ispec and reassemble */
	
	pcISpec=LShiftForm(pcISpec,LinearPlanActionDuration(plpLinearPlan));
	pcISpec->pcNext=pcTail;
	pcShiftedForm->pfForm->pcArgs=pcISpec;

	/* current the result */
	
	nForm=(*pcShiftedForm->pfForm->paAction->pfCurrent)(pcShiftedForm,plpLinearPlan,pbBindings);
	EXIT("CurrentDelta");
	return nForm;
}

/* CurrentTEventually

Description:
	Current a (teventually... ) modality.
*/

int CurrentTEventually
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ISPECP pisISpec;
	CELLP pcForm;
	int nForm;

	ENTER("CurrentTEventually",TRUE);
	pisISpec=GetBFISpec(pcFormula)->pfForm->uValue.piISpec;
	if(ISpecLt0(pisISpec))
		nForm=FALSE;					/* timed out */
	else
	{
		if(ISpec0In(pisISpec))
		{
			pcForm=GetBFFormulas(pcFormula);
			nForm=(*pcForm->pfForm->paAction->pfCurrent)(pcForm,plpLinearPlan,pbBindings);
			if(nForm==FALSE)
				nForm=NOT_FALSE;
		}
		else
			nForm=NOT_FALSE;
	}
	EXIT("CurrentTEventually");
	return nForm;
}

/* CurrentTUntil

Description:
	Current a (tuntil... ) modality of the form (condition until achievement).
*/

int CurrentTUntil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	ISPECP piISpec;
	CELLP pcForm;
	int nCondition;
	int nAchievement;

	ENTER("CurrentTUntil",TRUE);
	piISpec=GetBFISpec(pcFormula)->pfForm->uValue.piISpec;
	if(ISpecLt0(piISpec))
	{
		nCondition=FALSE;				/* timed out */
		EXIT("CurrentTUntil");
		return nCondition;
	}
	pcForm=GetBFFormulas(pcFormula);
	if(ISpec0In(piISpec))
	{
		nAchievement=(*pcForm->pcNext->pfForm->paAction->pfCurrent)(pcForm->pcNext,plpLinearPlan,pbBindings);
		if(nAchievement==TRUE)
		{
			nCondition=TRUE;
			EXIT("CurrentTUntil");
			return nCondition;
		}
		nCondition=(*pcForm->pfForm->paAction->pfCurrent)(pcForm,plpLinearPlan,pbBindings);
		if(nCondition==TRUE)
			nCondition=NOT_FALSE;

		if(nAchievement!=FALSE)
		{
			if(nCondition==FALSE)
				nCondition=nAchievement;
			else 
				nCondition=NOT_FALSE;
		}
		EXIT("CurrentTUntil");
		return nCondition;
	}
	nCondition=(*pcForm->pfForm->paAction->pfCurrent)(pcForm,plpLinearPlan,pbBindings);
	if(nCondition==TRUE)
		nCondition=NOT_FALSE;
	EXIT("CurrentTUntil");
	return nCondition;
}

/* Auxiliary Formulas ---------------------------------------------------------- */

/* CurrentBinding

Description:
	Current a binding node, by extending the current bindings. Also if
	internal formula is not TRUE or FALSE preserve the bindings but reset
	the formula to be the currented formula.
Algorithm:
	c(binding vars vals form) => (binding vars vals c(form))
*/

int CurrentBinding
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcForm;
	int nForm;

	ENTER("CurrentBinding",TRUE);
	pcForm=GetBindingFormula(pcFormula);
	nForm=(*pcForm->pfForm->paAction->pfCurrent)(pcForm,plpLinearPlan,
		ExtendBindings(GetBindingVars(pcFormula),GetBindingVals(pcFormula),pbBindings));
	EXIT("CurrentBinding");
	return nForm;
}

/* Modalities ----------------------------------------------------------------- */

/* CurrentGoal

Description:
	Current a goal formula. Should not have modalities inside.
Algorithm:
	c(goal form) => ?(goal form)
*/

int CurrentGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;
	int n;

	ENTER("CurrentGoal",TRUE);
	pcArgs=pcFormula->pfForm->pcArgs;
	n=(*pcArgs->pfForm->paAction->pfEval)(pcArgs,GetGoalWorld(TRUE),pbBindings);
	EXIT("CurrentGoal");
	return n;
}

/* CurrentCurrent

Description:
	Current a current formula. Should not have modalities inside.
Algorithm:
	c(current form) => ?(current form)
*/

int CurrentCurrent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcArgs;
	int n;

	ENTER("CurrentCurrent",TRUE);
	pcArgs=pcFormula->pfForm->pcArgs;
	n=(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plpCurrentPlan,pbBindings);
	EXIT("CurrentCurrent");
	return n;
}

/* CurrentPrevious

Description:
	Current a previous formula. Should not have modalities inside.
Algorithm:
	c(previous form) => ?(previous form)
*/

int CurrentPrevious
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	LINEARPLANP plp;
	CELLP pcArgs;
	int n;

	ENTER("CurrentPrevious",TRUE);

	/* if the current plan is not null, get the previous plan */
	
	plp=plpLinearPlan;
	if(plp)
		plp=plp->plpParent;

	/* if the previous plan exists, evaluate the passed formula, 
	otherwise return FALSE */
	
	pcArgs=pcFormula->pfForm->pcArgs;
	if(plp)
		n=(*pcArgs->pfForm->paAction->pfEval)(pcArgs,plp,pbBindings);
	else
		n=FALSE;
	EXIT("CurrentPrevious");
	return n;
}

/* CurrentAtomic

Description:
	Current an atomic formula.  
Notes:
	This routine works for any predicate that can be immediately reduced to
	true or false (any atomic formula).
Algorithm:
	c(predicate form) => ?(predicate form)
*/

int CurrentAtomic
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	int n;

	ENTER("CurrentAtomic",TRUE);
	n=(*pcFormula->pfForm->paAction->pfEval)(pcFormula,plpLinearPlan,pbBindings);
	EXIT("CurrentAtomic");
	return n;
}
