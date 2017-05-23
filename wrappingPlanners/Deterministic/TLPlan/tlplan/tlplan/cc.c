// cc.c -- Current Condition Algorithm

// Description:
//	Compute the current condition formula from a temporal formula.
//	Each temporal formula generates two current formulas, TForm and FForm,
//	with the following properties:
//		a) Both formulas are atemporal.
//		b) For every world W:
//
//			if Tform != NIL then 
//				(eval Tform W '()) = #T
//					==> (progress <TLFORM> W '()) = (TRUE)
//
//			if Fform != NIL then
//				(eval Fform W '()) = #F
//					==> (progress <TLFORM> W '()) = (FALSE)
//
//	While in general we need to be able to generate both the TForm and FForm of
//	each formula, we only generate the FForm of the outer control formula, since
//	in general, our control formulas are never true, and we are only interested
//	in knowing if they are false.

// The derivation of the true forms is straight forward.  We check all the cases
// that can make the formula true.  We fetch the tform or fform of each subformula
// based on whether we need to check for truth or false (one sided logic).  When 
// we don't have a particular tform or fform we need to check, then we fall back 
// and do our best with what we have.  We only fail to return a particular form 
// as a last resort.

// The derivation of the false forms requires an additional step.  We work as we
// do for the true form, but check all the cases that can make the formula false.
// The additional step arises because we require the false form to return false.
// So we must negate (and simplify) each case.

#include <stdio.h>

#include "tlplan.h"
#include "cc.h"
#include "eval.h"
#include "formula.h"
#include "makeform.h"

// MakeCCTrueForm

CELLP MakeCCTrueForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	*pbImmutable=TRUE;
	return pcForm;
}

// MakeCCFalseForm

CELLP MakeCCFalseForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	*pbImmutable=TRUE;
	return pcForm;
}

// MakeCCAtomicForm

CELLP MakeCCAtomicForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	*pbImmutable=TRUE;
	return pcForm;
}

// Booleans ---------------------------------------------------------------------

// MakeCCNotForm

// Description:
//	Return current condition formulas for a not formula

// Scheme
//	(define (cc-not form)
//		(let*
//			;; 1. Compute all cc's 
//			(
//				(cc-subforms (map cc (get-args form)))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//	
//			;; test for special case where we can reuse form.
//			;; note should be sufficient to test only one of tforms or fforms.
//	
//			(cond
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//				(else
//					(let 
//						(
//							(ccarg (first cc-subforms)))
//	
//						(if (not (null? (get-fform ccarg)))
//							(set! tform (make-not-form (get-fform ccarg))))
//						(if (not (null? (get-tform ccarg)))
//							(set! fform (make-not-form (get-tform ccarg)))))))
//	
//			(make-cc-return tform fform)))

CELLP MakeCCNotForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg;
	CELLP pcSubForm;
	BOOL bImmutable;

	pcArg=pcForm->pfForm->pcArgs;
	if(!pcArg)							// do we need to check this?
		return NULL;
	pcSubForm=(*pcArg->pfForm->paAction->pfMakeCCForm)(!bType,pcArg,&bImmutable);
	if(pcSubForm)
	{
		if(FormulaEqQ(pcArg,pcSubForm))	// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		if(NotFormQ(pcSubForm))			// remove double negation
		{
			*pbImmutable=TRUE;
			return pcSubForm->pfForm->pcArgs;
		}
		*pbImmutable=FALSE;
		return MakeNotForm(pcSubForm->pcNext&&bImmutable,pcSubForm);
	}
	return NULL;
}

// CCAnd

// Description:
//	Return current condition formulas for an and formula

// Scheme
//	(define (cc-and form)
//		(let*
//			;;1. Compute all cc's of the and's arguments.
//			((cc-subforms (map cc (get-args form)))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//			
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms or fforms.
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//	
//				(else
//					;; if every argument has a tform then we can make a tform
//					(if (every (lambda (cc) (not (null? (get-tform cc)))) cc-subforms)
//						(set! tform (make-and-form tsubforms)))
//	
//					;; the fform however are simply the conjunction of the ones that exist.
//					(let ((len (length fsubforms)))
//						(cond 
//							((> len 1)
//								(set! fform (make-and-form fsubforms)))
//							((= len 1)
//								(set! fform (first fsubforms)))))))
//			(make-cc-return tform fform)))

CELLP MakeCCAndForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArgs;
	CELLP pcSubForm;
	CELLP pcSubForms,pcEnd;				// list head
	CELLP pc;
	BOOL bImmutable;

	pcArgs=pcForm->pfForm->pcArgs;
	if(!pcArgs)							// do we need to check this?
		return NULL;

	pcSubForms=NULL;
	pcEnd=(CELLP)&pcSubForms;

	if(bType)							// handle true form
	{
		// if every argument has a tform, we can make a tform

		for(pc=pcArgs;pc;pc=pc->pcNext)
		{
			pcSubForm=(*pc->pfForm->paAction->pfMakeCCForm)(TRUE,pc,&bImmutable);
			if(!pcSubForm)
				return NULL;
			pcEnd=pcEnd->pcNext=bImmutable?CopyCell(pcSubForm):pcSubForm;
		}
		pcEnd->pcNext=NULL;
	}
	else								// handle false form
	{
		// if any argument has an fform, we can make an fform

		for(pc=pcArgs;pc;pc=pc->pcNext)
		{
			pcSubForm=(*pc->pfForm->paAction->pfMakeCCForm)(FALSE,pc,&bImmutable);
			if(pcSubForm)
				pcEnd=pcEnd->pcNext=bImmutable?CopyCell(pcSubForm):pcSubForm;
		}
		pcEnd->pcNext=NULL;
	}
	if(pcSubForms)
	{
		if(FormulaListEqQ(pcArgs,pcSubForms))	// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		if(pcSubForms->pcNext)
		{
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcSubForms);
		}
		*pbImmutable=TRUE;
		return pcSubForms;
	}
	return NULL;
}

// CCOr

// Description:
//	Return current condition formulas for an or formula

// Scheme
//	(define (cc-or form)
//		(let*
//			;;1. Compute all cc's of the and's arguments.
//			((cc-subforms (map cc (get-args form)))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//	
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//	
//				(else
//					;; if every argument has an fform then we can make a fform
//					(if (every (lambda (cc) (not (null? (get-fform cc)))) cc-subforms)
//						(set! fform (make-or-form fsubforms)))
//	
//					;; the tform however are simply the conjunction of the ones that exist.
//					(let ((len (length tsubforms)))
//						(cond 
//							((> len 1)
//								(set! tform (make-or-form tsubforms)))
//							((= len 1)
//								(set! tform (first tsubforms)))))))
//			(make-cc-return tform fform)))

CELLP MakeCCOrForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArgs;
	CELLP pcSubForm;
	CELLP pcSubForms,pcEnd;				// list head
	CELLP pc;
	BOOL bImmutable;

	pcArgs=pcForm->pfForm->pcArgs;
	if(!pcArgs)							// do we need to check this?
		return NULL;

	pcSubForms=NULL;
	pcEnd=(CELLP)&pcSubForms;

	if(bType)							// handle true form
	{
		// if any argument has a tform, we can make a tform

		for(pc=pcArgs;pc;pc=pc->pcNext)
		{
			pcSubForm=(*pc->pfForm->paAction->pfMakeCCForm)(TRUE,pc,&bImmutable);
			if(pcSubForm)
				pcEnd=pcEnd->pcNext=bImmutable?CopyCell(pcSubForm):pcSubForm;
		}
		pcEnd->pcNext=NULL;
	}
	else								// handle false form
	{
		// if every argument has an fform, we can make an fform

		for(pc=pcArgs;pc;pc=pc->pcNext)
		{
			pcSubForm=(*pc->pfForm->paAction->pfMakeCCForm)(FALSE,pc,&bImmutable);
			if(!pcSubForm)
				return NULL;
			pcEnd=pcEnd->pcNext=bImmutable?CopyCell(pcSubForm):pcSubForm;
		}
		pcEnd->pcNext=NULL;
	}
	if(pcSubForms)
	{
		if(FormulaListEqQ(pcArgs,pcSubForms))	// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		if(pcSubForms->pcNext)
		{
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcSubForms);
		}
		*pbImmutable=TRUE;
		return pcSubForms;
	}
	return NULL;
}

// CCXor

// Description:
//	Return current condition formulas for an xor formula

// Scheme
//	(define (cc-xor form)
//		(let*
//			;;1. Compute all cc's of the and's arguments.
//			((cc-subforms (map cc (get-args form)))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//	
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//	
//				(else
//					;;The xor can be tested for true if every subform has a Tform and
//					;;also a Fform. In which case it will be true if
//					;;(and (xor tforms) (xor fforms))---one and only one *must* be
//					;;true, and all but one *must* be false. Note these formulas
//					;;are one-sided, i.e., if one of the tforms==False it does not
//					;;mean its subform will not progress to true!
//					(cond
//						(
//							(and 
//								(every (lambda (cc) (not (null? (get-tform cc)))) cc-subforms)
//								(every (lambda (cc) (not (null? (get-fform cc)))) cc-subforms))
//							(set! tform
//								(make-and-form
//									(list (make-xor-form tsubforms)
//										(make-xor-form fsubforms))))))
//	
//					;;The xor is false if more than one of the tforms is
//					;;true. Note that if all of the tforms are false this does not
//					;;mean anything. 
//					;;Also if all of the subforms has an fform and they all
//					;;evaluate to false, then this will be a failure as well.
//	
//					(let
//						(
//							(more-than-one-has-tform (> 1 (count get-tform cc-subforms)))
//							(all-have-fforms 
//								(every (lambda (cc) (not (null? (get-fform cc)))) cc-subforms)))
//	
//						(cond 
//							(
//								(and more-than-one-has-tform all-have-fforms)
//								(set! fform 
//									(make-and-form
//										(list 
//											(make-or-form
//												(make-xor-form tsubforms)
//												(make-not-form (make-or-form tsubforms)))
//											(make-or-form fsubforms)))))
//							(
//								more-than-one-has-tform
//								(set! fform 
//									(make-or-form
//										(make-xor-form tsubforms)
//										(make-not-form (make-or-form tsubforms)))))
//	
//							;;
//							(
//								all-have-fforms
//								(set! fform (make-or-form fsubforms)))))))
//			(make-cc-return tform fform)))

CELLP MakeCCXorForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArgs;
	CELLP pcTrueForm;
	CELLP pcTrueForms,pcTrueEnd;		// list head
	CELLP pcFalseForm;
	CELLP pcFalseForms,pcFalseEnd;		// list head
	CELLP pc;
	BOOL bNull;							// null flag
	BOOL bImmutable;

	pcArgs=pcForm->pfForm->pcArgs;
	if(!pcArgs)							// do we need to check this?
		return NULL;

	pcTrueForms=NULL;
	pcTrueEnd=(CELLP)&pcTrueForms;
	pcFalseForms=NULL;
	pcFalseEnd=(CELLP)&pcFalseForms;

	if(bType)							// handle true form
	{
		// The xor can be tested for true if every subform has a Tform and
		// also a Fform. In which case it will be true if
		// (and (xor tforms) (xor fforms))---one and only one *must* be
		// true, and all but one *must* be false. Note these formulas
		// are one-sided, i.e., if one of the tforms==False it does not
		// mean its subform will not progress to true!

		for(pc=pcArgs;pc;pc=pc->pcNext)
		{
			pcTrueForm=(*pc->pfForm->paAction->pfMakeCCForm)(TRUE,pc,&bImmutable);
			if(!pcTrueForm)
				return NULL;
			pcTrueEnd=pcTrueEnd->pcNext=bImmutable?CopyCell(pcTrueForm):pcTrueForm;
			pcFalseForm=(*pc->pfForm->paAction->pfMakeCCForm)(FALSE,pc,&bImmutable);
			if(!pcFalseForm)
				return NULL;
			pcFalseEnd=pcFalseEnd->pcNext=bImmutable?CopyCell(pcFalseForm):pcFalseForm;
		}
		pcTrueEnd->pcNext=NULL;
		pcFalseEnd->pcNext=NULL;

		if(FormulaListEqQ(pcArgs,pcTrueForms))	// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		if(pcTrueForms->pcNext)
			pcTrueForms=MakeXorForm(FALSE,pcTrueForms);
		if(pcFalseForms->pcNext)
			pcFalseForms=MakeXorForm(FALSE,pcFalseForms);
		pcTrueForms->pcNext=pcFalseForms;
		*pbImmutable=FALSE;
		return MakeAndForm(FALSE,pcTrueForms);
	}
	else								// handle false form
	{
		// The xor is false if more than one of the tforms is true. 
		// Note that if all of the tforms are false this does not mean anything. 
		// Also if all of the subforms has an fform and they all
		// evaluate to false, then this will be a failure as well.

		bNull=FALSE;					// assume no null false forms
		for(pc=pcArgs;pc;pc=pc->pcNext)
		{
			pcTrueForm=(*pc->pfForm->paAction->pfMakeCCForm)(TRUE,pc,&bImmutable);
			if(pcTrueForm)
				pcTrueEnd=pcTrueEnd->pcNext=bImmutable?CopyCell(pcTrueForm):pcTrueForm;
			pcFalseForm=(*pc->pfForm->paAction->pfMakeCCForm)(FALSE,pc,&bImmutable);
			if(pcFalseForm)
				pcFalseEnd=pcFalseEnd->pcNext=bImmutable?CopyCell(pcFalseForm):pcFalseForm;
			else
				bNull=TRUE;				// at least one null false form
		}
		pcTrueEnd->pcNext=NULL;
		pcFalseEnd->pcNext=NULL;

		if(FormulaListEqQ(pcArgs,pcTrueForms))	// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		if(pcTrueForms)
		{
			if(pcTrueForms->pcNext)		// if more than one true form
			{
				if(!bNull)				// if no null false forms
				{
					pcTrueForm=MakeOrForm(FALSE,pcTrueForms);
					pcTrueForm=MakeNotForm(FALSE,pcTrueForm);
					pcTrueForms=MakeXorForm(FALSE,pcTrueForms);
					pcTrueForms->pcNext=pcTrueForm;
					pcTrueForms=MakeOrForm(FALSE,pcTrueForms);
					pcFalseForms=MakeOrForm(FALSE,pcFalseForms);
					pcTrueForms->pcNext=pcFalseForms;
					*pbImmutable=FALSE;
					return MakeAndForm(FALSE,pcTrueForms);
				}
				pcTrueForm=MakeOrForm(FALSE,pcTrueForms);
				pcTrueForm=MakeNotForm(FALSE,pcTrueForm);
				pcTrueForms=MakeXorForm(FALSE,pcTrueForms);
				pcTrueForms->pcNext=pcTrueForm;
				*pbImmutable=FALSE;
				return MakeOrForm(FALSE,pcTrueForms);
			}
		}
		if(!bNull)
		{
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcFalseForms);
		}
	}
	return NULL;
}

// CCImplies

// Description:
//	Return current condition formulas for an implies formula

// Scheme
//	(define (cc-implies form)
//		(let*
//			;;1. Compute all cc's 
//			((cc-subforms (map cc (get-args form)))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//			
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//	
//				(else
//					(let
//						(
//							(A-cc (first cc-subforms))
//							(C-cc (second cc-subforms)))
//	
//						;;there is a tform if antecedent has fform or consequent has tform.
//						(cond
//							(
//								(and 
//									(not (null? (get-tform C-cc))) 
//									(not (null? (get-fform A-cc))))
//								(set! tform 
//									(make-or-form 
//										(list 
//											(make-not-form (get-fform A-cc))
//											(get-tform C-cc)))))
//							;;
//							((not (null? (get-tform C-cc)))
//								(set! tform (get-tform C-cc)))
//							;;
//							((not (null? (get-fform A-cc)))
//								(set! tform (make-not-form (get-fform A-cc)))))
//						(if
//							;;there is a fform if the antecedent has a tform and the
//							;;consequent has a fform.
//							(and (not (null? (get-fform C-cc))) (not (null? (get-tform A-cc))))
//							(set! fform 
//								(make-or-form 
//									(list 
//										(make-not-form (get-tform A-cc))
//										(get-fform C-cc))))))))
//			(make-cc-return tform fform)))

CELLP MakeCCImpliesForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg1,pcArg2;
	CELLP pcAntecedent;
	CELLP pcConsequent;
	BOOL bAntecedentImmutable;
	BOOL bConsequentImmutable;

	pcArg1=pcForm->pfForm->pcArgs;
	if(!pcArg1)							// do we need to check this?
		return NULL;
	pcArg2=pcArg1->pcNext;

	if(bType)							// handle true form
	{
		// there is a tform if antecedent has fform or consequent has tform.

		pcAntecedent=(*pcArg1->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg1,&bAntecedentImmutable);
		pcConsequent=(*pcArg2->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg2,&bConsequentImmutable);
		if(pcAntecedent&&pcConsequent)
		{
			if(FormulaEqQ(pcAntecedent,pcArg1)&&FormulaEqQ(pcConsequent,pcArg2))	// if nothing changed, reuse original formula
			{
				*pbImmutable=TRUE;
				return pcForm;
			}
			*pbImmutable=FALSE;
			return MakeImpliesForm(FALSE,
				bAntecedentImmutable?CopyCell(pcAntecedent):pcAntecedent,
				(bConsequentImmutable&&pcConsequent->pcNext)?CopyCell(pcConsequent):pcConsequent);
		}
		if(pcConsequent)
		{
			*pbImmutable=bConsequentImmutable;
			return pcConsequent;
		}
		if(pcAntecedent)
		{
			*pbImmutable=FALSE;
			return MakeNotForm(FALSE,
				(bAntecedentImmutable&&pcAntecedent->pcNext)?CopyCell(pcAntecedent):pcAntecedent);
		}
	}
	else								// handle false form
	{
		// there is a fform if the antecedent has a tform and the consequent has a fform.
	
		pcAntecedent=(*pcArg1->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg1,&bAntecedentImmutable);
		pcConsequent=(*pcArg2->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg2,&bConsequentImmutable);
		if(pcAntecedent&&pcConsequent)
		{
			if(FormulaEqQ(pcAntecedent,pcArg1)&&FormulaEqQ(pcConsequent,pcArg2))	// if nothing changed, reuse original formula
			{
				*pbImmutable=TRUE;
				return pcForm;
			}
			*pbImmutable=FALSE;
			return MakeImpliesForm(FALSE,
				bAntecedentImmutable?CopyCell(pcAntecedent):pcAntecedent,
				(bConsequentImmutable&&pcConsequent->pcNext)?CopyCell(pcConsequent):pcConsequent);
		}
	}
	return NULL;
}

// CCIfThenElse

// Description:
//	Return current condition formulas for an if-then-else formula

// Scheme
//	(define (cc-if-then-else form)
//		(let*
//			;;1. Compute all cc's 
//			((cc-subforms (map cc (get-args form)))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//	
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//				(else
//					(let 
//						(
//							(ccarg1 (first cc-subforms))
//							(ccarg2 (second cc-subforms))
//							(ccarg3 (third cc-subforms)))
//						;;
//						;; four cases where we can get a true form
//						(cond
//							(
//								(and 
//									(not (null? (get-tform ccarg1))) 
//									(not (null? (get-fform ccarg1))) 
//									(not (null? (get-tform ccarg2)))
//									(not (null? (get-tform ccarg3))))
//								(set! tform
//									(make-or-form
//										(list 
//											(make-and-form 
//												(list 
//													(get-tform ccarg1)
//													(get-tform ccarg2)))
//											(make-and-form 
//												(list 
//													(make-not-form (get-fform ccarg1))
//													(get-tform ccarg2)))))))
//							;;
//							(
//								(and 
//									(not (null? (get-tform ccarg1)))
//									(not (null? (get-tform ccarg2))))
//								(set! tform 
//									(make-and-form 
//										(list 
//											(get-tform ccarg1)
//											(get-tform ccarg2)))))
//							;;
//							(
//								(and 
//									(not (null? (get-fform ccarg1)))
//									(not (null? (get-tform ccarg3))))
//								(set! tform 
//									(make-and-form 
//										(list 
//											(make-not-form (get-fform ccarg1))
//											(get-tform ccarg3)))))
//							;;
//							(
//								(and 
//									(not (null? (get-tform ccarg2)))
//									(not (null? (get-tform ccarg3))))
//								(set! tform 
//									(make-and-form 
//										(list 
//											(get-tform ccarg2)
//											(get-tform ccarg3))))))
//	
//						;; cases where we get a false form. Rember the logic
//						;; that fform==false ==> progression is false!
//	
//						(cond
//							(
//								(and 
//									(not (null? (get-tform ccarg1)))
//									(not (null? (get-fform ccarg1)))
//									(not (null? (get-fform ccarg2)))
//									(not (null? (get-fform ccarg3))))
//								(set! fform
//									(make-and-form
//										(list 
//											(make-or-form 
//												(list 
//													(make-not-form (get-tform ccarg1))
//													(get-fform ccarg2)))
//											(make-or-form 
//												(list 
//													(get-fform ccarg1)
//													(get-fform ccarg2)))))))
//							;;
//							(
//								(and 
//									(not (null? (get-tform ccarg1)))
//									(not (null? (get-fform ccarg2))))
//								(set! fform 
//									(make-or-form 
//										(list 
//											(make-not-form (get-tform ccarg1))
//											(get-fform ccarg2)))))
//							;;
//							((and (not (null? (get-fform ccarg1)))
//									(not (null? (get-fform ccarg3))))
//								(set! tform 
//									(make-or-form 
//										(list 
//											(get-fform ccarg1)
//											(get-fform ccarg3)))))
//							;;
//							((and (not (null? (get-fform ccarg2)))
//									(not (null? (get-fform ccarg3))))
//								(set! tform 
//									(make-or-form 
//										(list 
//											(get-fform ccarg2)
//											(get-fform ccarg3)))))))))
//			(make-cc-return tform fform)))

CELLP MakeCCIfThenElseForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg1,pcArg2,pcArg3;
	CELLP pcTAntecedent,pcFAntecedent;
	CELLP pcConsequent;
	CELLP pcAlternate;
	BOOL bTAntecedentImmutable;
	BOOL bFAntecedentImmutable;
	BOOL bConsequentImmutable;
	BOOL bAlternateImmutable;

	pcArg1=pcForm->pfForm->pcArgs;
	if(!pcArg1)							// do we need to check this?
		return NULL;
	pcArg2=pcArg1->pcNext;
	pcArg3=pcArg2->pcNext;

	if(bType)							// handle true form
	{
		// cases where we get a true form

		pcTAntecedent=(*pcArg1->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg1,&bTAntecedentImmutable);
		pcFAntecedent=(*pcArg1->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg1,&bFAntecedentImmutable);
		pcConsequent=(*pcArg2->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg2,&bConsequentImmutable);
		pcAlternate=(*pcArg3->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg3,&bAlternateImmutable);
		if(pcTAntecedent&&pcFAntecedent&&pcConsequent&&pcAlternate)
		{
			if(FormulaEqQ(pcTAntecedent,pcArg1)&&	// if nothing changed, reuse original formula
				FormulaEqQ(pcConsequent,pcArg2)&&FormulaEqQ(pcAlternate,pcArg3))
			{
				*pbImmutable=TRUE;
				return pcForm;
			}

			if(bTAntecedentImmutable)
				pcTAntecedent=CopyCell(pcTAntecedent);
			if(bFAntecedentImmutable&&pcFAntecedent->pcNext)
				pcFAntecedent=CopyCell(pcFAntecedent);
			if(bConsequentImmutable&&pcConsequent->pcNext)
				pcConsequent=CopyCell(pcConsequent);
			if(bAlternateImmutable&&pcAlternate->pcNext)
				pcAlternate=CopyCell(pcAlternate);

			pcTAntecedent->pcNext=pcConsequent;
			pcTAntecedent=MakeAndForm(FALSE,pcTAntecedent);
			pcFAntecedent=MakeNotForm(FALSE,pcFAntecedent);
			pcFAntecedent->pcNext=pcAlternate;
			pcFAntecedent=MakeAndForm(FALSE,pcFAntecedent);
			pcTAntecedent->pcNext=pcFAntecedent;
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcTAntecedent);
		}
		if(pcTAntecedent&&pcConsequent)
		{
			if(bTAntecedentImmutable)
				pcTAntecedent=CopyCell(pcTAntecedent);
			if(bConsequentImmutable&&pcConsequent->pcNext)
				pcConsequent=CopyCell(pcConsequent);

			pcTAntecedent->pcNext=pcConsequent;
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcTAntecedent);
		}
		if(pcFAntecedent&&pcAlternate)
		{
			if(bFAntecedentImmutable&&pcFAntecedent->pcNext)
				pcFAntecedent=CopyCell(pcFAntecedent);
			if(bAlternateImmutable&&pcAlternate->pcNext)
				pcAlternate=CopyCell(pcAlternate);

			pcFAntecedent=MakeNotForm(FALSE,pcFAntecedent);
			pcFAntecedent->pcNext=pcAlternate;
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcFAntecedent);
		}
		if(pcConsequent&&pcAlternate)
		{
			if(bConsequentImmutable)
				pcConsequent=CopyCell(pcConsequent);
			if(bAlternateImmutable&&pcAlternate->pcNext)
				pcAlternate=CopyCell(pcAlternate);

			pcConsequent->pcNext=pcAlternate;
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcConsequent);
		}
	}
	else								// handle false form
	{
		// cases where we get a false form

		pcTAntecedent=(*pcArg1->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg1,&bTAntecedentImmutable);
		pcFAntecedent=(*pcArg1->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg1,&bFAntecedentImmutable);
		pcConsequent=(*pcArg2->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg2,&bConsequentImmutable);
		pcAlternate=(*pcArg3->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg3,&bAlternateImmutable);
		if(pcTAntecedent&&pcFAntecedent&&pcConsequent&&pcAlternate)
		{
			if(FormulaEqQ(pcTAntecedent,pcArg1)&&	// if nothing changed, reuse original formula
				FormulaEqQ(pcConsequent,pcArg2)&&FormulaEqQ(pcAlternate,pcArg3))
			{
				*pbImmutable=TRUE;
				return pcForm;
			}

			if(bTAntecedentImmutable&&pcTAntecedent->pcNext)
				pcTAntecedent=CopyCell(pcTAntecedent);
			if(bFAntecedentImmutable)
				pcFAntecedent=CopyCell(pcFAntecedent);
			if(bConsequentImmutable&&pcConsequent->pcNext)
				pcConsequent=CopyCell(pcConsequent);
			if(bAlternateImmutable&&pcAlternate->pcNext)
				pcAlternate=CopyCell(pcAlternate);

			pcTAntecedent=MakeNotForm(FALSE,pcTAntecedent);
			pcTAntecedent->pcNext=pcConsequent;
			pcTAntecedent=MakeOrForm(FALSE,pcTAntecedent);
			pcFAntecedent->pcNext=pcAlternate;
			pcFAntecedent=MakeOrForm(FALSE,pcFAntecedent);
			pcTAntecedent->pcNext=pcFAntecedent;
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcTAntecedent);
		}
		if(pcTAntecedent&&pcConsequent)
		{
			if(bTAntecedentImmutable&&pcTAntecedent->pcNext)
				pcTAntecedent=CopyCell(pcTAntecedent);
			if(bConsequentImmutable&&pcConsequent->pcNext)
				pcConsequent=CopyCell(pcConsequent);

			pcTAntecedent=MakeNotForm(FALSE,pcTAntecedent);
			pcTAntecedent->pcNext=pcConsequent;
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcTAntecedent);
		}
		if(pcFAntecedent&&pcAlternate)
		{
			if(bFAntecedentImmutable)
				pcFAntecedent=CopyCell(pcFAntecedent);
			if(bAlternateImmutable&&pcAlternate->pcNext)
				pcAlternate=CopyCell(pcAlternate);

			pcFAntecedent->pcNext=pcAlternate;
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcFAntecedent);
		}
		if(pcConsequent&&pcAlternate)
		{
			if(bConsequentImmutable)
				pcConsequent=CopyCell(pcConsequent);
			if(bAlternateImmutable&&pcAlternate->pcNext)
				pcAlternate=CopyCell(pcAlternate);

			pcConsequent->pcNext=pcAlternate;
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcConsequent);
		}
	}
	return NULL;
}

// Quantifiers -----------------------------------------------------------------

// MakeCCForAllForm

// Description:
//	Return the current condition formulas for a forall.

// Scheme
//	(define (cc-forall form)
//		(let* 
//			(
//				(args (get-args form))
//				(qf-vars (get-qf-variables args))
//				(gen-lit (get-qf-generator args))
//				(qf-form (list (get-qf-formula args)))
//				(cc-subforms (map cc qf-form))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//	
//			;; test for special case where we can reuse form.
//			;; note should be sufficient to test only one of tforms or fforms.
//	
//			(cond
//				((equal-list? tsubforms (first qf-form))
//					(set! tform form)
//					(set! fform form))
//				(else
//					(if 
//						(not (null? tsubforms))
//						(set! tform (list 'FORALL qf-vars gen-lit (first tsubforms))))
//					(if (not (null? fsubforms))
//						(set! fform (list 'FORALL qf-vars gen-lit (first fsubforms))))))
//			(make-cc-return tform fform)))

CELLP MakeCCForAllForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArg;
	CELLP pcSubForm;
	BOOL bImmutable;

	pcArg=pcForm->pfForm->pcArgs;
	if(!pcArg)							// null formula can't change, reuse original formula
	{
		*pbImmutable=TRUE;
		return pcForm;
	}

	pcSubForm=(*pcArg->pfForm->paAction->pfMakeCCForm)(bType,pcArg,&bImmutable);
	if(pcSubForm)
	{
		if(FormulaEqQ(pcSubForm,pcArg))		// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		pcVars=pcForm->pfForm->pcVars;
		pcGenLit=pcForm->pfForm->pcGenLit;
		*pbImmutable=FALSE;
		return MakeForAllForm(FALSE,pcVars,pcGenLit,
			(bImmutable&&pcSubForm->pcNext)?CopyCell(pcSubForm):pcSubForm);
	}
	return NULL;
}

// MakeCCExistsForm

// Description:
//	Return a current condition formula for an exists.

// Scheme
//	(define (cc-exists form)
//		(let* 
//			(
//				(args (get-args form))
//				(qf-vars (get-qf-variables args))
//				(gen-lit (get-qf-generator args))
//				(qf-form (list (get-qf-formula args)))
//				(cc-subforms (map cc qf-form))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//	
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (first qf-form))
//					(set! tform form)
//					(set! fform form))
//				(else
//					(if 
//						(not (null? tsubforms))
//						(set! tform (list 'EXISTS qf-vars gen-lit (first tsubforms))))
//					(if (not (null? fsubforms))
//						(set! fform (list 'EXISTS qf-vars gen-lit (first fsubforms))))))
//			(make-cc-return tform fform)))

CELLP MakeCCExistsForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArg;
	CELLP pcSubForm;
	BOOL bImmutable;

	pcArg=pcForm->pfForm->pcArgs;
	if(!pcArg)							// null formula can't change, reuse original formula
	{
		*pbImmutable=TRUE;
		return pcForm;
	}

	pcSubForm=(*pcArg->pfForm->paAction->pfMakeCCForm)(bType,pcArg,&bImmutable);
	if(pcSubForm)
	{
		if(FormulaEqQ(pcSubForm,pcArg))		// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		pcVars=pcForm->pfForm->pcVars;
		pcGenLit=pcForm->pfForm->pcGenLit;
		*pbImmutable=FALSE;
		return MakeExistsForm(FALSE,pcVars,pcGenLit,
			(bImmutable&&pcSubForm->pcNext)?CopyCell(pcSubForm):pcSubForm);
	}
	return NULL;
}

// MakeCCExistsXForm

// Description:
//	Return a current condition formula for an exists!

// Scheme
//	(define (cc-exists! form)
//		(let* 
//			(
//				(args (get-args form))
//				(qf-vars (get-qf-variables args))
//				(gen-lit (get-qf-generator args))
//				(qf-form (list (get-qf-formula args)))
//				(cc-subforms (map cc qf-form))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (first qf-form))
//					(set! tform form)
//					(set! fform form))
//				(else
//					;;Only tricky one
//					;;exist! will be true if at one and only binding is certain
//					;;to generate a true progression and all but one binding
//					;;is certain to generate false progressions
//					(if 
//						(and 
//							(not (null? tsubforms))
//							(not (null? fsubforms)))
//						(set! tform
//							(make-and-form
//								(list
//									(list 'EXISTS! qf-vars gen-lit (first tsubforms))
//									(list 'EXISTS! qf-vars gen-lit (first fsubforms))))))
//	
//					;;it will be false if more than one binding is certain to be true
//					;;(accounting for the case where all are true tests fail)
//					(cond
//						(
//							(and 
//								(not (null? tsubforms))
//								(not (null? fsubforms)))
//							(set! fform
//								(make-and-form
//									(list 
//										(make-or-form
//											(list
//												(list 'EXISTS! qf-vars gen-lit (first tsubforms))
//												(list 'FORALL qf-vars gen-lit (make-not-form (first tsubforms)))))
//										(list 'EXISTS qf-vars gen-lit fsubforms)))))
//						;;
//						(
//							(not (null? tsubforms))
//							(set! fform
//								(make-or-form
//									(list
//										(list 'EXISTS! qf-vars gen-lit (first tsubforms))
//										(list 'FORALL qf-vars gen-lit (make-not-form (first tsubforms)))))))
//						(
//							(not (null? fsubforms))
//							(set! fform (list 'EXISTS qf-vars gen-lit fsubforms))))))
//			(make-cc-return tform fform)))

CELLP MakeCCExistsXForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArg;
	CELLP pcTForm,pcFForm;
	CELLP pcTemp;
	BOOL bTImmutable;
	BOOL bFImmutable;

	pcArg=pcForm->pfForm->pcArgs;
	if(!pcArg)							// null formula can't change, reuse original formula
	{
		*pbImmutable=TRUE;
		return pcForm;
	}

	pcTForm=(*pcArg->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg,&bTImmutable);
	pcFForm=(*pcArg->pfForm->paAction->pfMakeCCForm)(FALSE,pcArg,&bFImmutable);
	if(pcTForm)
	{
		if(FormulaEqQ(pcTForm,pcArg))	// if nothing changed, reuse original formula
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
	}

	if(bType)							// handle true form
	{
		// exist! will be true if at one and only binding is certain
		// to generate a true progression and all but one binding
		// is certain to generate false progressions

		if(pcTForm&&pcFForm)
		{
			pcVars=pcForm->pfForm->pcVars;
			pcGenLit=pcForm->pfForm->pcGenLit;
			pcTForm=MakeExistsXForm(FALSE,pcVars,pcGenLit,
				(bTImmutable&&pcTForm->pcNext)?CopyCell(pcTForm):pcTForm);
			pcFForm=MakeExistsXForm(FALSE,pcVars,pcGenLit,
				(bFImmutable&&pcFForm->pcNext)?CopyCell(pcFForm):pcFForm);
			pcTForm->pcNext=pcFForm;
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcTForm);
		}
	}
	else								// handle false form
	{
		// it will be false if more than one binding is certain to be true
		// (accounting for the case where all are true tests fail)

		if(pcTForm&&pcFForm)
		{
			pcVars=pcForm->pfForm->pcVars;
			pcGenLit=pcForm->pfForm->pcGenLit;
			pcTemp=MakeNotForm(FALSE,
				(bTImmutable&&pcTForm->pcNext)?CopyCell(pcTForm):pcTForm);
			pcTemp=MakeForAllForm(FALSE,pcVars,pcGenLit,pcTemp);
			pcTForm=MakeExistsXForm(FALSE,pcVars,pcGenLit,
				(bTImmutable&&pcTForm->pcNext)?CopyCell(pcTForm):pcTForm);
			pcTForm->pcNext=pcTemp;
			pcTForm=MakeOrForm(FALSE,pcTForm);
			pcFForm=MakeExistsForm(FALSE,pcVars,pcGenLit,
				(bFImmutable&&pcFForm->pcNext)?CopyCell(pcFForm):pcFForm);
			pcTForm->pcNext=pcFForm;
			*pbImmutable=FALSE;
			return MakeAndForm(FALSE,pcTForm);
		}
		if(pcTForm)
		{
			pcVars=pcForm->pfForm->pcVars;
			pcGenLit=pcForm->pfForm->pcGenLit;
			pcTemp=MakeNotForm(FALSE,
				(bTImmutable&&pcTForm->pcNext)?CopyCell(pcTForm):pcTForm);
			pcTemp=MakeForAllForm(FALSE,pcVars,pcGenLit,pcTemp);
			pcTForm=MakeExistsXForm(FALSE,pcVars,pcGenLit,
				(bTImmutable&&pcTForm->pcNext)?CopyCell(pcTForm):pcTForm);
			pcTForm->pcNext=pcTemp;
			*pbImmutable=FALSE;
			return MakeOrForm(FALSE,pcTForm);
		}
		if(pcFForm)
		{
			pcVars=pcForm->pfForm->pcVars;
			pcGenLit=pcForm->pfForm->pcGenLit;
			*pbImmutable=FALSE;
			return MakeExistsForm(FALSE,pcVars,pcGenLit,
				(bFImmutable&&pcFForm->pcNext)?CopyCell(pcFForm):pcFForm);
		}
	}
	return NULL;
}

// MakeCCBindingForm

// Description:
//	Return current condition formulas for a binding formula.

// Scheme
//	(define (cc-binding form)
//		(let* 
//			(
//				(b-forms (list (get-binding-formula form)))
//				(b-vars (get-binding-vars form))
//				(b-vals (get-binding-vals form))
//				(cc-subforms (map cc b-forms))
//				(tsubforms (remove-nulls (map get-tform cc-subforms)))
//				(fsubforms (remove-nulls (map get-fform cc-subforms)))
//				(tform '())
//				(fform '()))
//			(cond
//				;; test for special case where we can reuse form.
//				;; note should be sufficient to test only one of tforms
//				;; or fforms.
//				((equal-list? tsubforms (get-args form))
//					(set! tform form)
//					(set! fform form))
//				(else
//					(let ((ccarg (first cc-subforms)))
//						(if (not (null? (get-tform ccarg)))
//							(set! tform (make-binding-form b-vars b-vals (get-tform ccarg))))
//						(if (not (null? (get-fform ccarg)))
//							(set! fform (make-binding-form b-vars b-vals (get-fform ccarg)))))))
//			(make-cc-return tform fform)))

CELLP MakeCCBindingForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg;
	CELLP pcBForm;
	CELLP pcVars;
	CELLP pcVals;
	BOOL bImmutable;

	pcArg=GetBindingFormula(pcForm);
	if(!pcArg)							// do we need to check this?
		return NULL;

	pcBForm=(*pcArg->pfForm->paAction->pfMakeCCForm)(bType,pcArg,&bImmutable);
	if(pcBForm)
	{
		if(FormulaEqQ(pcBForm,pcArg))
		{
			*pbImmutable=TRUE;
			return pcForm;
		}
		pcVars=GetBindingVars(pcForm);
		pcVals=GetBindingVals(pcForm);
		*pbImmutable=FALSE;
		return MakeBindingForm(FALSE,pcVars,pcVals,
			(bImmutable&&pcBForm->pcNext)?CopyCell(pcBForm):pcBForm);
	}
	return NULL;
}

// Temporal modalities ---------------------------------------------------------

// CCNext

// Description:
//	Return current component of a next formula

// Scheme
//	(define (cc-next form)
//		(make-cc-return '() '()))

CELLP MakeCCNextForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	return NULL;
}

// CCEventually

// Description:
//	Return current component of an eventually modality

// Scheme
//	(define (cc-eventually form)
//		(let ((cc-subform (cc (first (get-args form)))))
//			(make-cc-return (get-tform cc-subform) '())))

CELLP MakeCCEventuallyForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg;

	pcArg=pcForm->pfForm->pcArgs;
	if(!pcArg)							// do we need to check this?
		return NULL;

	if(bType)							// handle true form
		return (*pcArg->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg,pbImmutable);
	return NULL;
}

// CCAlways

// Description:
//	Return current component of an always modality

// Scheme
//	(define (cc-always form)
//		(let ((cc-subform (cc (first (get-args form)))))
//			(make-cc-return
//				(get-tform cc-subform)
//				(get-fform cc-subform))))

CELLP MakeCCAlwaysForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg;

	pcArg=pcForm->pfForm->pcArgs;
	if(!pcArg)							// do we need to check this?
		return NULL;

	return (*pcArg->pfForm->paAction->pfMakeCCForm)(bType,pcArg,pbImmutable);
}

// CCUntil

// Description:
//	Return current component of an until modality

// Scheme
//	(define (cc-until form)
//		(let* 
//			(
//				(cc-subforms (map cc (get-args form)))
//				(cc-subform1 (first cc-subforms))
//				(cc-subform2 (second cc-subforms))
//				(tform '())
//				(fform '()))
//			;;
//			;; (Until A B) is true if B is true
//			;;
//			(set! tform (get-tform cc-subform2))
//			;;
//			;; (until a b) is false if A and B progress to false
//			(if 
//				(and 
//					(not (null? (get-fform cc-subform1)))
//					(not (null? (get-fform cc-subform2))))
//				(set! fform 
//					(make-or-form
//						(list
//							(get-fform cc-subform1)
//							(get-fform cc-subform2)))))
//			(make-cc-return tform fform)))

CELLP MakeCCUntilForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
)
{
	CELLP pcArg1;
	CELLP pcArg2;
	BOOL bImmutable1;
	BOOL bImmutable2;

	pcArg1=pcForm->pfForm->pcArgs;
	if(!pcArg1)							// do we need to check this?
		return NULL;
	pcArg2=pcArg1->pcNext;

	if(bType)							// handle true form
	{
		// (Until A B) is true if B is true

		return (*pcArg2->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg2,pbImmutable);
	}
	else								// handle false form
	{
		// (until a b) is false if A and B progress to false

		pcArg1=(*pcArg1->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg1,&bImmutable1);
		if(!pcArg1)
			return NULL;
		pcArg2=(*pcArg2->pfForm->paAction->pfMakeCCForm)(TRUE,pcArg2,&bImmutable2);
		if(!pcArg2)
			return NULL;
		if(bImmutable1)
			pcArg1=CopyCell(pcArg1);
		if(bImmutable2&&pcArg2->pcNext)
			pcArg2=CopyCell(pcArg2);
		pcArg1->pcNext=pcArg2;
		*pbImmutable=FALSE;
		return MakeOrForm(FALSE,pcArg1);
	}
}

// MTL timed modalities --------------------------------------------------------

// We'll fill these in later?
