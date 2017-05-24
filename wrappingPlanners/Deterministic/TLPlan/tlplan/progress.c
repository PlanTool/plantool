/* progress.c

Copyright C, 1996 - 2001  F. Bacchus

Temporal Progressor -- Functions that manage temporal formulas expressed in LTL.

The temporal progressor is the heart of the TLPlan technique.
Progression simply involves progressing all of the terms of a formula, and
then simplifying the result if possible.
Typically simplification involves checking for
one or more terms which have themselves been reduced to true or false, and
then substituting a simpler equivalent formula.

Nomenclature for algorithm descriptions:
        p() progress
        s() shift
        ?() evaluate, with (true) or (false) result
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
#include "eval.h"
#include "formula.h"
#include "idle.h"
#include "iface.h"
#include "interval.h"
#include "makeform.h"
#include "makegen.h"
#include "progress.h"
#include "search.h"
#include "tlparse.h"
#include "util.h"
#include "world.h"

/* local structures and definitions */

/*FB Jan 2001
  1. Modified true/false so that same formula is always used.
  2. Modified code so that it uses equality testing and
     structure sharing more like the original lisp code.
     These modifications are based on the modification of Makeformulas
     and AppendFormula to make them hygienic.
     
*/

#define USE_BINDING_FORMULAS	0

/* local function prototypes */

static void AddInstance
(
	CELLP pcNewForm,                    /* formula to add */
	CELLP *ppcNewSubForms,              /* pointer to list of formulas */
	CELLP (*pfMake)(BOOL, CELLP, CELLP, CELLP),	/* make form routine */
	CELLP pcVars,                       /* variables to instantiate */
	LINEARPLANP plpLinearPlan,          /* current world */
	BINDINGP pbBindings,
	BOOL immutable
);

/* Run Time Routines ----------------------------------------------------------- */

/* ProgressFormula

Description:
        Top level progress evaluator, dispatch on nonAtomic formulas.
Notes:
        ProgressFormula takes advantage of the fact that progression of
        atomic formulas is identical to evaluating them. So it calls the
        evaluator.

Scheme:

(define (progress-formula form world/action bindings)
        (if (or (eq? form #t) (eq? form #f))
                form
        (let ((dispatch-fn (lookup-progress-eval (get-operator form))))
                (if dispatch-fn
                        (dispatch-fn form world/action bindings)
                        (eval-atomic (get-operator form)
                                (eval-terms (get-args form) world/action bindings)
                                world/action)))))
*/

//CELLP ProgressFormula
//(
//      CELLP pcFormula,
//      LINEARPLANP plpLinearPlan,
//      BINDINGP pbBindings
//)
//{
//      CELLP pc;
//
//      ENTER("ProgressFormula",TRUE);
//      pc=(*pcFormula->pfForm->paAction->pfProgress)(pcFormula,plpLinearPlan,pbBindings);
//      EXIT("ProgressFormula");
//      return pc;
//}

/* Boolean Formulas ------------------------------------------------------------ */

/* ProgressFalse

Description:
        Progress a FALSE formula... nothing changes.
Algorithm:
        p(false) => (false)
Scheme:

(define (progress-false form world/action bindings)
  #f)
*/

CELLP ProgressFalse
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  ENTER("ProgressFalse",TRUE);
  *pImmutable=TRUE;
  EXIT("ProgressFalse");
  return pcFormula;
}

/* ProgressTrue

Description:
        Progress a TRUE formula... nothing changes.
Algorithm:
        p(true) => (true)
Scheme:

(define (progress-true form world/action bindings)
  #t)
*/

CELLP ProgressTrue
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  ENTER("ProgressTrue",TRUE);
  *pImmutable=TRUE;
  EXIT("ProgressTrue");
  return pcFormula;
}

/* ProgressNot

Description:
        Progress a not Formula checking for true/false.
Algorithm:
        p(not form) => (not p(form))
Scheme:

(define (progress-not form world/action bindings)
        (let* ((subform (first (get-args form)))
                        (new-subform (progress-formula subform world/action bindings)))
                (cond
                        ((not new-subform) #t)
                        ((eq? #t new-subform) #f)
                        ((eq? new-subform subform) form)
                        (else (make-not-form new-subform)))))
*/

CELLP ProgressNot
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  CELLP pcNewArg1;
  CELLP pcArg1;
  BOOL immutable;

  ENTER("ProgressNot",TRUE);
  pcArg1=pcFormula->pfForm->pcArgs;
  pcNewArg1=(*pcArg1->pfForm->paAction->pfProgress)
                (pcArg1,plpLinearPlan,pbBindings,&immutable);
  if(FalseFormQ(pcNewArg1)) {
    pc=MakeTrueForm();
    *pImmutable=TRUE;
  }
  else if(TrueFormQ(pcNewArg1)) {
    pc=MakeFalseForm();
    *pImmutable=TRUE;
  }
  else if(pcNewArg1->pfForm==pcArg1->pfForm) {
    //else if(FormulaEqQ(pcNewArg1,pcArg1)) {
    pc=pcFormula;
    *pImmutable=TRUE;
  }
  else {
    *pImmutable=FALSE;
    pc=MakeNotForm(FALSE,(pcNewArg1->pcNext&&immutable)?CopyCell(pcNewArg1):pcNewArg1);
  }
  EXIT("ProgressNot");
  return pc;
}

/* ProgressAnd

Description:
        Progress an AND formula checking for true/false and compressing same
        level ands.
Algorithm:
        p(and form ...) => (and p(form) ...)
Scheme:

(define (progress-and form world/action bindings)
        (let ((new-subforms '())
                        (new-subform #f)
                        (make-new #f)
                        (false? #f))
                (do ((subforms (get-args form))
                                (subform '()))
                        ((or (null? subforms) false?))
                        (set! subform (first subforms))
                        (set! subforms (rest subforms))
                        (set! new-subform (progress-formula subform world/action bindings))
                        (cond
                                ((not new-subform)
                                        (set! new-subforms '())
                                        (set! false? #t))
                                ((eq? new-subform #t)
                                        (set! make-new #t))
                                ((eq? new-subform subform)
                                        (set! new-subforms (cons subform new-subforms)))
                                ((and-formp new-subform)
                                        (set! make-new #t)
                                        (set! new-subforms (append (get-args new-subform) new-subforms)))
                                (else
                                        (set! make-new #t)
                                        (set! new-subforms (cons new-subform new-subforms)))))
                (cond
                        (false? #f)
                        ((null? new-subforms) #t)
                        ((not make-new) form)
                        ((length=1 new-subforms) (first new-subforms))
                        (else (make-and-form new-subforms)))))
*/

CELLP ProgressAnd
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pcNewSubForms;                                    /* list of new subforms */
  CELLP pcNewSubFormsEnd;                                 /* list of new subforms */
  CELLP pcNewSubForm;                                     /* current new subform */
  CELLP pcSubForm;                                        /* current old subform */
  BOOL immutable;

  ENTER("ProgressAnd",TRUE);
  pcNewSubForms=pcNewSubFormsEnd=NULL;

  /* process prefix that agrees */
  
	pcNewSubForm=NULL;
	for(pcSubForm=pcFormula->pfForm->pcArgs;pcSubForm;pcSubForm=pcSubForm->pcNext)
	{
		pcNewSubForm=(*pcSubForm->pfForm->paAction->pfProgress)
			(pcSubForm,plpLinearPlan,pbBindings,&immutable);
		if(pcNewSubForm->pfForm!=pcSubForm->pfForm)
			break;
	}
	
  /* Check for termination prior to copying */
  
  if(!pcSubForm) /*all of formula agrees*/
    {
      *pImmutable=TRUE;
      EXIT("ProgressAnd");
      return pcFormula;
    }
  else if(FalseFormQ(pcNewSubForm)) /*Failure*/
    {
      *pImmutable=TRUE;
      EXIT("ProgressAnd");
      return MakeFalseForm();
    }
	
  /*copy agreeing prefix*/
  
  if(pcSubForm!=pcFormula->pfForm->pcArgs)
    pcNewSubForms=CopyPrefixCellList(pcFormula->pfForm->pcArgs,pcSubForm,&pcNewSubFormsEnd);
  else
    pcNewSubFormsEnd=(CELLP)&pcNewSubForms;

  /* process remaining arguments */
  
  while(pcSubForm)
    {
      /* We already have the next newsubform on entry into loop */
      if(FalseFormQ(pcNewSubForm))    /* Terminate on FALSE term */
	{
	  pcNewSubForms=NULL; /* collect garbage later */
	  *pImmutable=TRUE;
	  EXIT("ProgressAnd");
	  return MakeFalseForm();
	}
	    
      else if(TrueFormQ(pcNewSubForm))        /* discard TRUE terms */
	;
	
      //FB this seems to give a slight improvement. But would also mean
      //occupying more memory after a GC?
      //else if(AndFormQ(pcNewSubForm)) /* raise lower level AND */
      //{
      //  pcNewSubFormsEnd->pcNext=CopyCellListReturnEndCell(pcNewSubForm->pfForm->pcArgs,&pcTemp);
      //  pcNewSubFormsEnd=pcTemp;
      //}
      else /* Otherwise we always tack the newsubform on to the end*/
	{
	  if(immutable)
	    pcNewSubForm=CopyCell(pcNewSubForm);
	  pcNewSubFormsEnd=pcNewSubFormsEnd->pcNext=pcNewSubForm;
	  pcNewSubFormsEnd->pcNext=NULL;
	}

      /* Now progress next argument */
      pcSubForm=pcSubForm->pcNext;
      if(pcSubForm)
	pcNewSubForm=(*pcSubForm->pfForm->paAction->pfProgress)(pcSubForm,plpLinearPlan,pbBindings,&immutable);
    }

  if(!pcNewSubForms) {
    *pImmutable=TRUE;
    EXIT("ProgressAnd");
    return MakeTrueForm();
  }
  else if(!pcNewSubForms->pcNext) {
    *pImmutable=FALSE;
    EXIT("ProgressAnd");
    return pcNewSubForms;
  }
  else {
    *pImmutable=FALSE;
    EXIT("ProgressAnd");
    return MakeAndForm(FALSE,pcNewSubForms);
  }
}

/* ProgressOr

Description:
        Progress an or-formula checking for true/false and compressing same
        level ors.
Algorithm:
        p(or form ...) => (or p(form) ...)
Scheme:

(define (progress-or form world/action bindings)
  (let ((new-subforms '())
                (new-subform '())
                (make-new '())
        (true? #f))
        (do ((subforms (get-args form))
         (subform '()))
        ((or (null? subforms) true?))
          (set! subform (first subforms))
          (set! subforms (rest subforms))
          (set! new-subform (progress-formula subform world/action bindings))
          (cond ((eq? #t new-subform)
                         (set! new-subforms '())
                         (set! true? #t))
                        ((not new-subform)
                         (set! make-new #t))
                        ((eq? new-subform subform)
                         (set! new-subforms (cons subform new-subforms)))
                        ((or-formp new-subform)
                         (set! make-new #t)
                        (set! new-subforms (append (get-args new-subform) new-subforms)))
                        (else
                         (set! make-new #t)
                         (set! new-subforms (cons new-subform new-subforms)))))

        (cond (true? #t)
          ((null? new-subforms) #f)
          ((not make-new) form)
          ((length=1 new-subforms) (first new-subforms))
          (else (make-or-form new-subforms)))))
*/

CELLP ProgressOr
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pcNewSubForms;                            /* list of new subforms */
  CELLP pcNewSubFormsEnd;                                 /* list of new subforms */
  CELLP pcNewSubForm;                                     /* current new subform */
  CELLP pcSubForm;                                        /* current old subform */
  BOOL immutable;

  ENTER("ProgressOr",TRUE);
  pcNewSubForms=pcNewSubFormsEnd=NULL;

  /* process prefix that agrees */

	pcNewSubForm=NULL;
	for(pcSubForm=pcFormula->pfForm->pcArgs;pcSubForm;pcSubForm=pcSubForm->pcNext)
	{
		pcNewSubForm=(*pcSubForm->pfForm->paAction->pfProgress)
			(pcSubForm,plpLinearPlan,pbBindings,&immutable);
		if(pcNewSubForm->pfForm!=pcSubForm->pfForm)
			break;
	}

  /* Check for termination prior to copying */
  if(!pcSubForm) /*all of formula agrees*/
    {
      *pImmutable=TRUE;
      EXIT("ProgressAnd");
      return pcFormula;
    }
  else if(TrueFormQ(pcNewSubForm)) /*Success*/
    {
      *pImmutable=TRUE;
      EXIT("ProgressAnd");
      return MakeTrueForm();
    }

  /*copy agreeing prefix*/
  if(pcSubForm!=pcFormula->pfForm->pcArgs)
    pcNewSubForms=CopyPrefixCellList(pcFormula->pfForm->pcArgs,pcSubForm,&pcNewSubFormsEnd);
  else
    pcNewSubFormsEnd=(CELLP)&pcNewSubForms;

  /* process remaining arguments */
  while(pcSubForm)
    {
      /* We already have the next newsubform on entry into loop */
      if(TrueFormQ(pcNewSubForm))             /* if TRUE term */
	{
	  pcNewSubForms=NULL; /* collect garbage later */
	  *pImmutable=TRUE;
	  EXIT("ProgressOr");
	  return MakeTrueForm();
	}
      else if(FalseFormQ(pcNewSubForm))       /* discard FALSE terms */
	;

      //FB this seems to give a slight improvement. But would also mean
      //occupying more memory after a GC.
      //else if(OrFormQ(pcNewSubForm))  /* raise lower level OR */
      //{
      //  pcNewSubFormsEnd->pcNext=CopyCellListReturnEndCell(pcNewSubForm->pfForm->pcArgs,&pcTemp);
      //  pcNewSubFormsEnd=pcTemp;
      //}
      else                                                    /* prefix new terms */
	{
	  if(immutable)
	    pcNewSubForm=CopyCell(pcNewSubForm);
	  pcNewSubFormsEnd=pcNewSubFormsEnd->pcNext=pcNewSubForm;
	  pcNewSubFormsEnd->pcNext=NULL;
	}
      /* Now progress next argument */
      pcSubForm=pcSubForm->pcNext;
      if(pcSubForm)
	pcNewSubForm=(*pcSubForm->pfForm->paAction->pfProgress)(pcSubForm,plpLinearPlan,pbBindings,&immutable);
    }

  if(!pcNewSubForms) {
    *pImmutable=TRUE;
    EXIT("ProgressOr");
    return MakeFalseForm();
  }
  else if(!pcNewSubForms->pcNext) {
    *pImmutable=FALSE;
    EXIT("ProgressOr");
    return pcNewSubForms;
  }
  else {
    *pImmutable=FALSE;
    EXIT("ProgressOr");
    return MakeOrForm(FALSE,pcNewSubForms);
  }
}

/* ProgressXor

Description:
        Progress an XorFormula checking for true/false and compressing same
        level xors.
Algorithm:
        p(xor form ...) => (xor p(form) ...)
Scheme:

(define (progress-xor form world/action bindings)
        (let ((new-subforms '())
                        (new-subform '())
                        (make-new '())
                        (found-one? '())
                        (false? #f))
                (do ((subforms (get-args form))
                                (subform '()))
                        ((or (null? subforms) false?))
                        (set! subform (first subforms))
                        (set! subforms (rest subforms))
                        (set! new-subform (progress-formula subform world/action bindings))
                        (cond
                                ((and (eq? #t new-subform) found-one?)
                                        (set! new-subforms '())
                                        (set! false? #t))
                                ((eq? #t new-subform)
                                        (set! found-one? #t))
                                ((not new-subform)
                                        (set! make-new #t))
                                ((eq? new-subform subform)
                                        (set! new-subforms (cons subform new-subforms)))
                                ((xor-formp new-subform)
                                        (set! make-new #t)
                                        (set! new-subforms (append (get-args new-subform) new-subforms)))
                                (else
                                        (set! make-new #t)
                                        (set! new-subforms (cons new-subform new-subforms)))))
        (cond
                        ((or (and (null? new-subforms) (not found-one?)) false?)
                                #f)
                        ((and (null? new-subforms) found-one?)
                                #t)
                        ((and found-one? (length=1 new-subforms))
                                (make-not-form new-subforms))
                        (found-one?
                                (make-not-form (make-or-form new-subforms)))
                        ((not make-new)
                                form)
                        ((length=1 new-subforms)
                                (first new-subforms))
                        (else
                                (make-xor-form new-subforms)))))
*/

CELLP ProgressXor
(
   CELLP pcFormula,
   LINEARPLANP plpLinearPlan,
   BINDINGP pbBindings,
   BOOLP pImmutable
)
{
  CELLP pc;
  CELLP pcNewSubForms;                  /* list of new subforms */
  CELLP pcNewSubFormsEnd;               /* list of new subforms */
  CELLP pcNewSubForm;                   /* current new subform */
  CELLP pcSubForm;                      /* current old subform */
  BOOL immutable;
  BOOL bFoundOne;                       /* found a true term */

  ENTER("ProgressXor",TRUE);
  pcNewSubForms=pcNewSubFormsEnd=NULL;
  bFoundOne=FALSE;

  /* process prefix that agrees */

	pcNewSubForm=NULL;
	for(pcSubForm=pcFormula->pfForm->pcArgs;pcSubForm;pcSubForm=pcSubForm->pcNext)
	{
		pcNewSubForm=(*pcSubForm->pfForm->paAction->pfProgress)
			(pcSubForm,plpLinearPlan,pbBindings,&immutable);
		if(pcNewSubForm->pfForm!=pcSubForm->pfForm)
			break;
	}

  /* Check for termination prior to copying */
  if(!pcSubForm) /*all of formula agrees*/
    {
      *pImmutable=TRUE;
      EXIT("ProgressXor");
      return pcFormula;
    }

  /*copy agreeing prefix*/
  if(pcSubForm!=pcFormula->pfForm->pcArgs)
    pcNewSubForms=CopyPrefixCellList(pcFormula->pfForm->pcArgs,pcSubForm,&pcNewSubFormsEnd);
  else
    pcNewSubFormsEnd=(CELLP)&pcNewSubForms;

  /* process remaining arguments */
  while(pcSubForm)
    {
      /* We already have the next newsubform on entry into loop */
      if(TrueFormQ(pcNewSubForm))
	{
	  if(bFoundOne)                           /* if atleast two true terms */
	    {
	      pcNewSubForms=NULL;
	      *pImmutable=TRUE;
	      EXIT("ProgressXOr");
	      return MakeFalseForm();
	    }
	  else                                            /* first true term */
	    bFoundOne=TRUE;
	}
      else if(FalseFormQ(pcNewSubForm))       /* discard FALSE terms */
	;
      //FB this seems to give a slight improvement. But would also mean
      //occupying more memory after a GC.
      //else if(XorFormQ(pcNewSubForm))  /* raise lower level XOR */
      //{
      //  pcNewSubFormsEnd->pcNext=CopyCellListReturnEndCell(pcNewSubForm->pfForm->pcArgs,&pcTemp);
      //  pcNewSubFormsEnd=pcTemp;
      //}
      else                                                    /* prefix new terms */
	{
	  if(immutable)
	    pcNewSubForm=CopyCell(pcNewSubForm);
	  pcNewSubFormsEnd=pcNewSubFormsEnd->pcNext=pcNewSubForm;
	  pcNewSubFormsEnd->pcNext=NULL;
	}
    }

  if(!pcNewSubForms&&!bFoundOne) {
    *pImmutable=TRUE;
    pc=MakeFalseForm();
  }
  else if(bFoundOne)
    {
      if(!pcNewSubForms) {
	*pImmutable=TRUE;
	pc=MakeTrueForm();
      }
      else if(!pcNewSubForms->pcNext) {
	*pImmutable=FALSE;
	pc=MakeNotForm(FALSE,pcNewSubForms);
      }
      else {
	*pImmutable=FALSE;
	pc=MakeNotForm(FALSE,MakeOrForm(FALSE,pcNewSubForms));
      }
    }
  else if(!pcNewSubForms->pcNext) {
    *pImmutable=FALSE;
    pc=pcNewSubForms;
  }
  else {
    *pImmutable=FALSE;
    pc=MakeXorForm(FALSE,pcNewSubForms);
  }
  EXIT("ProgressXor");
  return pc;
  }

/* ProgressImplies

Description:
        Progress an impliesFormula checking for true/false.
Algorithm:
        p(implies a c) => (implies p(a) p(c))
Scheme:

(define (progress-implies form world/action bindings)
        (let* (
                        (subforms (get-args form))
                        (new-arg1 (progress-formula (first subforms) world/action bindings)))
                (if (not new-arg1)
                        #t
                        (let (
                                        (new-arg2 (progress-formula (second subforms) world/action bindings)))
                                (cond
                                        ((eq? #t new-arg2)
                                                #t)
                                        ((eq? #t new-arg1)
                                                new-arg2)
                                        ((not new-arg2)
                                                (make-not-form new-arg1))
                                        ((and (eq? new-arg1 (first subforms))
                                                (eq? new-arg2 (second subforms)))
                                                form)
                                        (else
                                                (make-implies-form new-arg1 new-arg2)))))))
*/

CELLP ProgressImplies
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  CELLP pcArg1;
  CELLP pcArg2;
  CELLP pcNewArg1;
  CELLP pcNewArg2;
  BOOL immutable1;
  BOOL immutable2;

  ENTER("ProgressImplies",TRUE);
  pcArg1=pcFormula->pfForm->pcArgs;
  pcArg2=pcArg1->pcNext;
  pcNewArg1=(*pcArg1->pfForm->paAction->pfProgress)(pcArg1,plpLinearPlan,pbBindings,&immutable1);
  if(FalseFormQ(pcNewArg1)) {
    *pImmutable=TRUE;
    pc=MakeTrueForm();
  }
  else
  {
    pcNewArg2=(*pcArg2->pfForm->paAction->pfProgress)(pcArg2,plpLinearPlan,pbBindings,&immutable2);
    if(TrueFormQ(pcNewArg2)) {
      *pImmutable=TRUE;
      pc=MakeTrueForm();
    }
    else if(TrueFormQ(pcNewArg1)) {
      *pImmutable=immutable2;
      pc=pcNewArg2;
    }
    else if(FalseFormQ(pcNewArg2)) {
      *pImmutable=FALSE;
      pc=MakeNotForm(FALSE,(pcNewArg1->pcNext&&immutable1)?CopyCell(pcNewArg1):pcNewArg1);
    }
    else if(pcNewArg1->pfForm==pcArg1->pfForm&&pcNewArg2->pfForm==pcArg2->pfForm) {
      //else if(FormulaEqQ(pcNewArg1,pcArg1)&&FormulaEqQ(pcNewArg2,pcArg2)) {
      *pImmutable=TRUE;
      pc=pcFormula;
    }
    else {
      *pImmutable=FALSE;
      pc=MakeImpliesForm(FALSE,immutable1?CopyCell(pcNewArg1):pcNewArg1,
			 (pcNewArg2->pcNext&&immutable2)?CopyCell(pcNewArg2):pcNewArg2);
    }
  }
  EXIT("ProgressImplies");
  return pc;
}

/* ProgressIfThenElse

Description:
        Progress an ifThenElse formula checking for true/false.
Algorithm:
        p(if-then-else a t f) => (if-then-else p(a) p(t) p(f))
Scheme:

(define (progress-if-then-else form world/action bindings)
        (let* (
                        (subforms (get-args form))
                        (arg1 (first subforms))
                        (arg2 (second subforms))
                        (arg3 (third subforms))
                        (new-arg1 (progress-formula arg1 world/action bindings))
                        (new-arg2 #f)
                        (new-arg3 #f))
                (cond
                        ((eq? new-arg1 #t)
                                (progress-formula arg2 world/action bindings))
                        ((not new-arg1)
                                (progress-formula arg3 world/action bindings))
                        (else
                                (set! new-arg2 (progress-formula arg2 world/action bindings))
                                (set! new-arg3 (progress-formula arg3 world/action bindings))
                                (cond
                                        ((eq? new-arg2 #t)
                                                (if (eq? new-arg3 #t)
                                                        #t
                                                        (if (not new-arg3)
                                                                new-arg1
                                                                (make-implies-form (make-not-form new-arg1) new-arg3))))
                                        ((not new-arg2)
                                                (if (eq? new-arg3 #t)
                                                        (make-not-form new-arg1)
                                                        (if (not new-arg3)
                                                                #f
                                                                (make-and-form (list (make-not-form new-arg1) new-arg3)))))
                                        (else
                                                (if (eq? new-arg3 #t)
                                                        (make-implies-form new-arg1 new-arg2)
                                                        (if (not new-arg3)
                                                                (make-and-form (list new-arg1 new-arg2))
                                                                (if (and (eq? new-arg1 arg1)
                                                                                (eq? new-arg2 arg2)
                                                                                (eq? new-arg3 arg3))
                                                                        form
                                                                        (make-if-then-else-form
                                                                                new-arg1 new-arg2 new-arg3))))))))))
*/

CELLP ProgressIfThenElse
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  CELLP pcArg1;
  CELLP pcArg2;
  CELLP pcArg3;
  CELLP pcNewArg1;
  CELLP pcNewArg2;
  CELLP pcNewArg3;
  BOOL immutable1;
  BOOL immutable2;
  BOOL immutable3;


  ENTER("ProgressIfThenElse",TRUE);
  pcArg1=pcFormula->pfForm->pcArgs;
  pcArg2=pcArg1->pcNext;
  pcArg3=pcArg2->pcNext;

  pcNewArg1=(*pcArg1->pfForm->paAction->pfProgress)(pcArg1,plpLinearPlan,pbBindings,&immutable1);
  if(TrueFormQ(pcNewArg1))
  {
    pc=(*pcArg2->pfForm->paAction->pfProgress)(pcArg2,plpLinearPlan,pbBindings,&immutable2);
    *pImmutable=immutable2;    
    EXIT("ProgressIfThenElse");
    return pc;
  }

  if(FalseFormQ(pcNewArg1))
  {
    pc=(*pcArg3->pfForm->paAction->pfProgress)(pcArg3,plpLinearPlan,pbBindings,&immutable3);
    *pImmutable=immutable3;    
    EXIT("ProgressIfThenElse");
    return pc;
  }

  pcNewArg2=(*pcArg2->pfForm->paAction->pfProgress)(pcArg2,plpLinearPlan,pbBindings,&immutable2);
  pcNewArg3=(*pcArg3->pfForm->paAction->pfProgress)(pcArg3,plpLinearPlan,pbBindings,&immutable3);

  if(TrueFormQ(pcNewArg2))
  {
    if(TrueFormQ(pcNewArg3)) {
      *pImmutable=TRUE;    
      pc=MakeTrueForm();
    }
    else if(FalseFormQ(pcNewArg3)) {
      *pImmutable=immutable1;    
      pc=pcNewArg1;
    }
    else {
      *pImmutable=FALSE;    
      pc=MakeImpliesForm(FALSE,MakeNotForm(FALSE,(pcNewArg1->pcNext&&immutable1)
					   ?CopyCell(pcNewArg1):pcNewArg1),
			 (pcNewArg3->pcNext&&immutable3)?CopyCell(pcNewArg3):pcNewArg3);
    }
    EXIT("ProgressIfThenElse");
    return pc;
  }

  if(FalseFormQ(pcNewArg2))
  {
    if(TrueFormQ(pcNewArg3)) {
      *pImmutable=FALSE;
      pc=MakeNotForm(FALSE,(pcNewArg1->pcNext&&immutable1)?CopyCell(pcNewArg1):pcNewArg1);
    }
    else if(FalseFormQ(pcNewArg3)) {
      *pImmutable=TRUE;
      pc=MakeFalseForm();
    }
    else
    {
      *pImmutable=FALSE;
      pc=MakeNotForm(FALSE,(pcNewArg1->pcNext&&immutable1)?CopyCell(pcNewArg1):pcNewArg1);
      pc->pcNext=(pcNewArg3->pcNext&&immutable3)?CopyCell(pcNewArg3):pcNewArg3;
      pc=MakeAndForm(FALSE,pc);
    }
    EXIT("ProgressIfThenElse");
    return pc;
  }
  
  if(TrueFormQ(pcNewArg3)) {
    *pImmutable=FALSE;
    pc=MakeImpliesForm(FALSE,immutable1?CopyCell(pcNewArg1):pcNewArg1,
		       (pcNewArg2->pcNext&&immutable2)?CopyCell(pcNewArg2):pcNewArg2);
  }
  else if(FalseFormQ(pcNewArg3))
  {
    *pImmutable=FALSE;
    if(immutable1)
        pcNewArg1=CopyCell(pcNewArg1);
    pcNewArg1->pcNext=(pcNewArg2->pcNext&&immutable2)?CopyCell(pcNewArg2):pcNewArg2;
    pc=MakeAndForm(FALSE,pcNewArg1);
  }
  else if(pcNewArg1->pfForm==pcArg1->pfForm&&pcNewArg2->pfForm==pcArg2->pfForm&&pcNewArg3->pfForm==pcArg3->pfForm)
  //else if(FormulaEqQ(pcNewArg1,pcArg1)&&FormulaEqQ(pcNewArg2,pcArg2)&&FormulaEqQ(pcNewArg3,pcArg3))
  {
    *pImmutable=TRUE;
    pc=pcFormula;
  }
  else {
    *pImmutable=FALSE;
    pc=MakeIfThenElseForm(FALSE,
			  immutable1?CopyCell(pcNewArg1):pcNewArg1,
			  immutable2?CopyCell(pcNewArg2):pcNewArg2,
			  (pcNewArg3->pcNext&&immutable3)?CopyCell(pcNewArg3):pcNewArg3);
  }
  EXIT("ProgressIfThenElse");
  return pc;
}

/* Quantified Formulas --------------------------------------------------------- */

/* ProgressForAll

Description:
        Progress a forall quantifier, by reducing to instances.
Algorithm:
        p(forall vars gen form) => (and (binding vars vals p(form)) ...)
Scheme:

(define (progress-forall form world/action bindings)
        (let* (
                        (args (get-args form))
                        (qf-vars (get-qf-variables args))
                        (gen-lit (get-qf-generator args))
                        (qf-form (get-qf-formula args))
                        (generator '())
                        (false? #f)
                        (progressed-form #f)
                        (new-subforms '()))
                (set! bindings (extend-bindings qf-vars qf-vars bindings))
                (set! generator (make-generator gen-lit qf-vars world/action bindings))
                (if generator
                        (do
                                ((finish? #f))
                                (finish? #f)
                                (cond
                                        ((generator)
                                                (set! progressed-form
                                                        (progress-formula qf-form world/action bindings))
                                                (cond
                                                        ((eq? progressed-form #t))
                                                        ((not progressed-form)
                                                                (set! finish? #t)
                                                                (set! false? #t)
                                                                (set! new-subforms '()))
                                                        (else
                                                                (set! new-subforms
                                                                        (cons
                                                                                (make-binding-form
                                                                                        qf-vars (eval-terms qf-vars
                                                                                                world/action bindings)
                                                                                        progressed-form)
                                                                                new-subforms)))))
                                        (else
                                                (set! finish? #t)))))

                (cond
                        (false?
                                #f)
                        ((null? new-subforms)
                                #t)
                        ((length=1 new-subforms)
                                (first new-subforms))
                        (else
                                (make-and-form new-subforms)))))
*/

CELLP ProgressForAll
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
)
{
	CELLP pc;
	CELLP pcVars;
	CELLP pcGenLit;
	void *pvContext;
	CELLP pcNewForm;
	CELLP pcNewSubForms;
	CELLP pcArgs;
	BOOL bFalse;
	BOOL immutable;
	int nBaseSP;						// saved base btree stack index
	
	ENTER("ProgressForAll",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	  /* make room */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();
	
	if(!pcArgs)
	{
		*pImmutable=TRUE;
		pc=MakeFalseForm();
		EXIT("ProgressForAll");
		return pc;
	}
	
	bFalse=FALSE;
	pcNewSubForms=NULL;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		pcNewForm=(*pcArgs->pfForm->paAction->pfProgress)(pcArgs,plpLinearPlan,pbBindings,&immutable);
		if(FalseFormQ(pcNewForm))
		{
			*pImmutable=TRUE;
			bFalse=TRUE;
			pcNewSubForms=NULL;
			SetBTreeSP(nBaseSP);		// restore btree stack pointer
			break;
		}
		else if(!TrueFormQ(pcNewForm))
			AddInstance(pcNewForm,&pcNewSubForms,MakeForAllForm,
			pcVars,plpLinearPlan,pbBindings,immutable);
	}
	
	if(bFalse) {
		*pImmutable=TRUE;
		pc=MakeFalseForm();
	}
	else if(!pcNewSubForms) {
		*pImmutable=TRUE;
		pc=MakeTrueForm();
	}
	else if(!pcNewSubForms->pcNext) {
		*pImmutable=FALSE;
		pc=pcNewSubForms;
	}
	else {
		*pImmutable=FALSE;
		pc=MakeAndForm(FALSE,pcNewSubForms);
	}
	EXIT("ProgressForAll");
	return pc;
}

/* AddInstance

Description:
        Add a formula to a list of "in-the-set" quantifiers,
        making reductions where possible.
*/

static void AddInstance
(
	CELLP pcNewForm,					/* formula to add */
	CELLP *ppcNewSubForms,				/* pointer to list of formulas */
	CELLP (*pfMake)(BOOL, CELLP, CELLP,	CELLP),	/* make form routine */
	CELLP pcVars,						/* variables to instantiate */
	LINEARPLANP plpLinearPlan,			/* current world */
	BINDINGP pbBindings,
	BOOL immutable
)
{
	CELLP pcTerms;
	CELLP pc;

#if USE_BINDING_FORMULAS
	pcTerms=ComputeTerms(pcVars,plpLinearPlan,pbBindings);
	if(!pcTerms)
		TermError("add-instance",pcNewForm);
	pc=MakeBindingForm(FALSE,pcVars,pcTerms,
		(pcNewForm->pcNext&&immutable)?CopyCell(pcNewForm):pcNewForm);
	pc->pcNext=*ppcNewSubForms;
	*ppcNewSubForms=pc;
#else // USE_BINDING_FORMULAS
	CELLP pcGenLit;

	pcTerms=ComputeTerms(pcVars,plpLinearPlan,pbBindings);
	if(!pcTerms)
		TermError("add-instance",pcNewForm,pbBindings);

	/* look for a matching formula
	we don't need to check the quantifier, since we are never called across
	differing quantifier contexts.  */
	
	for(pc=*ppcNewSubForms;pc;pc=pc->pcNext)
	{
		if(FormulaEqQ(pcNewForm,pc->pfForm->pcArgs))
			break;
	}
	if(pc)								/* if formula matches */
	{
		for(pc=pc->pfForm->pcGenLit->pfForm->pcArgs;pc->pcNext;pc=pc->pcNext);
		pc->pcNext=pcTerms;
	}
	else								/* if new formula */
	{
		pcGenLit=MakeInTheSetForm(FALSE,pcVars,pcTerms);
//		pc=(*pfMake)(FALSE,pcVars,pcGenLit,pcNewForm);
		pc=(*pfMake)(FALSE,pcVars,pcGenLit,(pcNewForm->pcNext&&immutable)?CopyCell(pcNewForm):pcNewForm);
		pc->pcNext=*ppcNewSubForms;
		*ppcNewSubForms=pc;
	}
#endif // USE_BINDING_FORMULAS
}

/* ProgressExists

Description:
        Progress an exists quantifier, by reducing to instances.
Algorithm:
        p(exists vars gen form) => (or (binding vars vals p(form)) ...)
Scheme:

(define (progress-exists form world/action bindings)
        (let* (
                        (args (get-args form))
                        (qf-vars (get-qf-variables args))
                        (gen-lit (get-qf-generator args))
                        (qf-form (get-qf-formula args))
                        (generator '())
                        (true? #f)
                        (progressed-form #f)
                        (new-subforms '()))
                (set! bindings (extend-bindings qf-vars qf-vars bindings))
                (set! generator (make-generator gen-lit qf-vars world/action bindings))
                (if generator
                        (do
                                ((finish? #f))
                                (finish? #f)
                                (cond
                                        ((generator)
                                                (set! progressed-form
                                                        (if (not qf-form)
                                                                #t
                                                                (progress-formula qf-form world/action bindings)))
                                                (cond
                                                        ((eq? progressed-form #t)
                                                                (set! finish? #t)
                                                                (set! true? #t)
                                                                (set! new-subforms '()))
                                                        ((not progressed-form))
                                                        (else
                                                                (set! new-subforms
                                                                        (cons
                                                                                (make-binding-form
                                                                                        qf-vars (eval-terms qf-vars
                                                                                                world/action bindings)
                                                                                        progressed-form)
                                                                                new-subforms)))))
                                        (else
                                                (set! finish? #t)))))

                (cond
                        (true?
                                #t)
                        ((null? new-subforms)
                                #f)
                        ((length=1 new-subforms)
                                (first new-subforms))
                        (else
                                (make-or-form new-subforms)))))
*/

CELLP ProgressExists
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
)
{
	CELLP pc;
	CELLP pcVars;
	CELLP pcGenLit;
	CELLP pcArgs;
	void *pvContext;
	CELLP pcNewForm;
	CELLP pcNewSubForms;
	BOOL bTrue;
	BOOL immutable;
	int nBaseSP;						// saved base btree stack index
	
	ENTER("ProgressExists",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pcArgs=pcFormula->pfForm->pcArgs;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	  /* make room */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();
	
	bTrue=FALSE;
	pcNewSubForms=NULL;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!pcArgs)
			pcNewForm=MakeTrueForm();
		else
			pcNewForm=(*pcArgs->pfForm->paAction->pfProgress)
			(pcArgs,plpLinearPlan,pbBindings,&immutable);
		if(TrueFormQ(pcNewForm))
		{
			*pImmutable=TRUE;
			bTrue=TRUE;
			pcNewSubForms=NULL;
			SetBTreeSP(nBaseSP);		// restore btree stack pointer
			break;
		}
		else if(!FalseFormQ(pcNewForm))
			AddInstance(pcNewForm,&pcNewSubForms,MakeExistsForm,
			pcVars,plpLinearPlan,pbBindings,immutable);
	}
	
	if(bTrue) {
		*pImmutable=TRUE;
		pc=MakeTrueForm();
	}
	else if(!pcNewSubForms) {
		*pImmutable=TRUE;
		pc=MakeFalseForm();
	}
	else if(!pcNewSubForms->pcNext) {
		*pImmutable=FALSE; 
		pc=pcNewSubForms;
	}
	else {
		*pImmutable=FALSE;
		pc=MakeOrForm(FALSE,pcNewSubForms);
	}
	EXIT("ProgressExists");
	return pc;
}

/* ProgressExistsX

Description:
        Progress an exists! quantifier, by reducing to instances.
Algorithm:
        p(exists! vars gen form) => (xor (binding vars vals p(form)) ...)
Scheme:

(define (progress-exists! form world/action bindings)
        (let* (
                        (args (get-args form))
                        (qf-vars (get-qf-variables args))
                        (gen-lit   (get-qf-generator args))
                        (qf-form   (get-qf-formula args))
                        (generator '())
                        (false? #f)
                        (progressed-form #f)
                        (new-subforms '())
                        (found-one? #f))
                (set! bindings (extend-bindings qf-vars qf-vars bindings))
                (set! generator (make-generator gen-lit qf-vars world/action bindings))
                (if generator
                        (do
                                ((finish? #f))
                                (finish? #f)
                                (cond
                                        ((generator)
                                                (set! progressed-form
                                                        (if (not qf-form)
                                                                #t
                                                                (progress-formula qf-form world/action bindings)))
                                                (cond
                                                        ((and (eq? progressed-form #t) found-one?)
                                                                (set! finish? #t)
                                                                (set! false? #t)
                                                                (set! new-subforms '()))
                                                        ((eq? progressed-form #t)
                                                                (set! found-one? #t))
                                                        ((not progressed-form))
                                                        (else
                                                                (set! new-subforms
                                                                        (cons (make-binding-form
                                                                                        qf-vars (eval-terms qf-vars
                                                                                                world/action bindings)
                                                                                        progressed-form)
                                                                                new-subforms)))))
                                        (else
                                                (set! finish? #t)))))
                (cond
                        (false?
                                #f)
                        ((null? new-subforms)
                                (if found-one?
                                        #t
                                        #f))
                        ((and found-one? (length=1 new-subforms))
                                (make-not-form new-subforms))
                        (found-one?
                                (make-not-form (make-or-form new-subforms)))
                        ((length=1 new-subforms)
                                (first new-subforms))
                        (else
                                (make-xor-form new-subforms)))))
*/

CELLP ProgressExistsX
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
	CELLP pc;
	CELLP pcVars;
	CELLP pcGenLit;
	void *pvContext;
	CELLP pcNewForm;
	CELLP pcNewSubForms;
	BOOL bFalse;
	BOOL bFoundOne;
	BOOL immutable;
	int nBaseSP;						// saved base btree stack index
	
	ENTER("ProgressExistsX",TRUE);
	pcVars=pcFormula->pfForm->pcVars;
	pcGenLit=pcFormula->pfForm->pcGenLit;
	pbBindings=ExtendBindings(pcVars,pcVars,pbBindings);	  /* make room */
	pvContext=NULL;
	nBaseSP=GetBTreeSP();
	
	bFalse=FALSE;
	bFoundOne=FALSE;
	pcNewSubForms=NULL;
	while((*pcGenLit->pfForm->paAction->pfGenerator)
		(pcGenLit,&pvContext,pcVars,plpLinearPlan,pbBindings))
//		(pcGenLit,&(pbBindings->pvContext),pcVars,plpLinearPlan,pbBindings))
	{
		if(!pcFormula->pfForm->pcArgs)
			pcNewForm=MakeTrueForm();
		else
			pcNewForm=(*pcFormula->pfForm->pcArgs->pfForm->paAction->pfProgress)
			(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings,&immutable);
		if(TrueFormQ(pcNewForm))
		{
			if(bFoundOne)
			{
				bFalse=TRUE;
				pcNewSubForms=NULL;
				SetBTreeSP(nBaseSP);	// restore btree stack pointer
				break;
			}
			else
				bFoundOne=TRUE;
		}
		else if(!FalseFormQ(pcNewForm))
			AddInstance(pcNewForm,&pcNewSubForms,MakeExistsXForm,
			pcVars,plpLinearPlan,pbBindings,immutable);
	}
	
	if(bFalse) {
		*pImmutable=TRUE;
		pc=MakeFalseForm();
	}
	else if(!pcNewSubForms)
	{
		if(bFoundOne) {
			*pImmutable=TRUE;
			pc=MakeTrueForm();
		}
		else {
			*pImmutable=TRUE;
			pc=MakeFalseForm();
		}
	}
	else if(bFoundOne)
	{
		if(!pcNewSubForms->pcNext) {
			*pImmutable=FALSE;
			pc=MakeNotForm(FALSE,pcNewSubForms);
		}
		else {
			*pImmutable=FALSE;
			pc=MakeNotForm(FALSE,MakeOrForm(FALSE,pcNewSubForms));
		}
	}
	else if(!pcNewSubForms->pcNext) {
		*pImmutable=FALSE;
		pc=pcNewSubForms;
	}
	else {
		*pImmutable=FALSE;
		pc=MakeXorForm(FALSE,pcNewSubForms);
	}
	EXIT("ProgressExistsX");
	return pc;
}

/* Temporal Formulas ----------------------------------------------------------- */

/* ProgressAlways

Description:
        Progress an ALWAYS modality.
Algorithm:
        p(always form) => (and p(form) (always form))
Scheme:

(define (progress-always form world/action bindings)
        (let (
                        (progressed-form
                                (progress-formula (first (get-args form)) world/action bindings)))
                (cond
                        ((eq? progressed-form #t)
                                form)
                        ((not progressed-form)
                                #f)
                        ((and-formp progressed-form)
                                (make-and-form (append (get-args progressed-form) (list form))))
                        (else
                                (make-and-form (list progressed-form form))))))
*/

CELLP ProgressAlways
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  CELLP pcNewArg1;
  CELLP pcArg1;
  BOOL immutable;

  ENTER("ProgressAlways",TRUE);
  pcArg1=pcFormula->pfForm->pcArgs;
  pcNewArg1=(*pcArg1->pfForm->paAction->pfProgress)(pcArg1,plpLinearPlan,pbBindings,&immutable);
  if(TrueFormQ(pcNewArg1)) {
      *pImmutable=TRUE;
      pc=pcFormula;
  }
  else if(FalseFormQ(pcNewArg1)) {
    *pImmutable=TRUE;
    pc=MakeFalseForm();
  }
  else if(AndFormQ(pcNewArg1)) {
    *pImmutable=FALSE;
    pc=MakeAndForm(FALSE,AppendFormula(TRUE,pcNewArg1->pfForm->pcArgs,
		pcFormula->pcNext?CopyCell(pcFormula):pcFormula));
  }
  else
  {
    if(immutable) 
        pcNewArg1=CopyCell(pcNewArg1);
    pcNewArg1->pcNext=pcFormula->pcNext?CopyCell(pcFormula):pcFormula;
    //pcNewArg1->pcNext=pcFormula;
    pc=MakeAndForm(FALSE,pcNewArg1);
  }
  EXIT("ProgressAlways");
  return pc;
}

/* ProgressEventually

Description:
        Progress an EVENTUALLY modality.
Algorithm:
        p(eventually form) => (or p(form) (eventually form))
Scheme:

(define (progress-eventually form world/action bindings)
        (let (
                        (progressed-form
                                (progress-formula (first (get-args form)) world/action bindings)))
                (cond
                        ((not progressed-form)
                                form)
                        ((eq? progressed-form #t)
                                #t)
                        ((or-formp progressed-form)
                                (make-or-form (append (get-args progressed-form) (list form))))
                        (else
                                (make-or-form (list progressed-form form))))))
*/

CELLP ProgressEventually
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  CELLP pcNewArg1;
  CELLP pcArg1;
  BOOL immutable;

  ENTER("ProgressEventually",TRUE);
  pcArg1=pcFormula->pfForm->pcArgs;
  pcNewArg1=(*pcArg1->pfForm->paAction->pfProgress)(pcArg1,plpLinearPlan,pbBindings,&immutable);
  if(FalseFormQ(pcNewArg1)) {
    *pImmutable=TRUE;
    pc=pcFormula;
  }
  else if(TrueFormQ(pcNewArg1)) {
    *pImmutable=TRUE;
    pc=MakeTrueForm();
  }
  else if(OrFormQ(pcNewArg1)) {
    *pImmutable=FALSE;
    pc=MakeOrForm(FALSE,AppendFormula(TRUE,pcNewArg1->pfForm->pcArgs,
		pcFormula->pcNext?CopyCell(pcFormula):pcFormula));
  }
  else
  {
    *pImmutable=FALSE;
    if(immutable) 
        pcNewArg1=CopyCell(pcNewArg1);
    pcNewArg1->pcNext=pcFormula->pcNext?CopyCell(pcFormula):pcFormula;
    pc=MakeOrForm(FALSE,pcNewArg1);
  }
  EXIT("ProgressEventually");
  return pc;
}

/* ProgressNext

Description:
        Progress a NEXT modality.
Algorithm:
        p(next form) => form
Scheme:

(define (progress-next form world/action bindings)
  (first (get-args form)))
*/

CELLP ProgressNext
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  ENTER("ProgressNext",TRUE);
  *pImmutable=TRUE;
  pc=pcFormula->pfForm->pcArgs;
  EXIT("ProgressNext");
  return pc;
}

/* ProgressUntil

Description:
        Progress an UNTIL modality of the form (condition UNTIL achievement).
Algorithm:
        p(until c a) => (or p(a) (and p(c) (until c a)))
Scheme:

(define (progress-until form world/action bindings)
        (let (
                        (progressed-achievement
                                (progress-formula (second (get-args form)) world/action bindings)))
                (if (eq? progressed-achievement #t)
                        #t
                        (let (
                                        (progressed-condition
                                                (progress-formula (first (get-args form)) world/action bindings)))
                                (set! progressed-condition
                                        (cond
                                                ((not progressed-condition)
                                                        #f)
                                                ((eq? progressed-condition #t)
                                                        form)
                                                ((and-formp progressed-condition)
                                                        (make-and-form
                                                                (append (get-args progressed-condition) (list form))))
                                                (else
                                                        (make-and-form (list progressed-condition form)))))
                                (cond
                                        ((not progressed-achievement)
                                                progressed-condition)
                                        ((not progressed-condition)
                                                progressed-achievement)
                                        ((or-formp progressed-achievement)
                                                (make-or-form (cons progressed-condition
                                                                (get-args progressed-achievement))))
                                        (else
                                                (make-or-form (list progressed-achievement
                                                                progressed-condition))))))))
*/

CELLP ProgressUntil
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  CELLP pcAchievement;
  CELLP pcCondition;
  CELLP pcNewAchievement;
  CELLP pcNewCondition;
  BOOL immutableA;
  BOOL immutableC;

  ENTER("ProgressUntil",TRUE);
  pcCondition=pcFormula->pfForm->pcArgs;
  pcAchievement=pcCondition->pcNext;
  pcNewAchievement=(*pcAchievement->pfForm->paAction->pfProgress)(pcAchievement,plpLinearPlan,pbBindings,&immutableA);
  if(TrueFormQ(pcNewAchievement))
  {
    *pImmutable=TRUE;
    pc=MakeTrueForm();
    EXIT("ProgressUntil");
    return pc;
  }
  pcNewCondition=(*pcCondition->pfForm->paAction->pfProgress)(pcCondition,plpLinearPlan,pbBindings,&immutableC);
  if(FalseFormQ(pcNewCondition)) {
    immutableC=TRUE;
    pcNewCondition=MakeFalseForm();
  }
  else if(TrueFormQ(pcNewCondition)) {
    immutableC=TRUE;
    pcNewCondition=pcFormula;
  }
  else if(AndFormQ(pcNewCondition)) {
    immutableC=FALSE;
    pcNewCondition=MakeAndForm(FALSE,AppendFormula(TRUE,pcNewCondition->pfForm->pcArgs,
		pcFormula->pcNext?CopyCell(pcFormula):pcFormula));
  }
  else
  {
    if(immutableC)
       pcNewCondition=CopyCell(pcNewCondition);
    pcNewCondition->pcNext=pcFormula->pcNext?CopyCell(pcFormula):pcFormula;
    pcNewCondition=MakeAndForm(FALSE,pcNewCondition);
    immutableC=FALSE;
  }

  if(FalseFormQ(pcNewAchievement)) {
    *pImmutable=immutableC;
    pc=pcNewCondition;
  }
  else if(FalseFormQ(pcNewCondition)) {
    *pImmutable=immutableA;
    pc=pcNewAchievement;
  }
  else if(OrFormQ(pcNewAchievement))
  {
    if(immutableC)
       pcNewCondition=CopyCell(pcNewCondition);
    pcNewCondition->pcNext=pcNewAchievement->pfForm->pcArgs;
    *pImmutable=FALSE;
    pc=MakeOrForm(FALSE,pcNewCondition);
  }
  else
  {
    if(immutableA)
       pcNewAchievement=CopyCell(pcNewAchievement);
    pcNewAchievement->pcNext=pcNewCondition;
    *pImmutable=FALSE;
    pc=MakeOrForm(FALSE,pcNewAchievement);
  }
  EXIT("ProgressUntil");
  return pc;
}

/* MTL Timed Temporal Formulas ------------------------------------------------- */

/* ProgressTAlways

Description:
        Progress a timed ALWAYS modality.
Scheme:

(define (progress-t-always form world/action bindings)
        (let (
                        (ispec (get-bf-ispec form))
                        (forms (get-bf-formulas form))
                        (shifted-form (make-delta-form form)))
                (cond
                        ((ispec<0 ispec)
                                #t) ;timed out
                        ((ispec0in ispec)
                                (let (
                                                (progressed-form
                                                        (progress-formula (first forms) world/action bindings)))
                                        (cond
                                                ((eq? progressed-form #t)
                                                        shifted-form)
                                                ((not progressed-form)
                                                        #f)
                                                ((and-formp progressed-form)
                                                        (make-and-form (append (get-args progressed-form)
                                                                        (list shifted-form))))
                                                (else
                                                        (make-and-form (list progressed-form shifted-form))))))
                        (else
                                shifted-form))))
*/

CELLP ProgressTAlways
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  ISPECP pisISpec;
  CELLP pcForm;
  CELLP pcShiftedForm;
  CELLP pcNewForm;
  BOOL immutable;

  ENTER("ProgressTAlways",TRUE);
  pisISpec=GetBFISpec(pcFormula)->pfForm->uValue.piISpec;
  if(ISpecLt0(pisISpec))
    {
      *pImmutable=TRUE;
      pc=MakeTrueForm();                      /* timed out */
      EXIT("ProgressTAlways");
      return pc;
    }
  pcShiftedForm=MakeDeltaForm(FALSE,pcFormula->pcNext?CopyCell(pcFormula):pcFormula);
  if(ISpec0In(pisISpec))
    {
      pcForm=GetBFFormulas(pcFormula);
      pcNewForm=(*pcForm->pfForm->paAction->pfProgress)(pcForm,plpLinearPlan,pbBindings,&immutable);
      if(TrueFormQ(pcNewForm)) {
	*pImmutable=FALSE;
	pc=pcShiftedForm;
      }
      else if(FalseFormQ(pcNewForm)) {
	*pImmutable=TRUE;
	pc=MakeFalseForm();
      }
      else if(AndFormQ(pcNewForm)) {
	*pImmutable=FALSE;
	pc=MakeAndForm(FALSE,AppendFormula(TRUE,pcNewForm->pfForm->pcArgs,pcShiftedForm));
      }
      else
	{
	  if(immutable)
	    pcNewForm=CopyCell(pcNewForm);
	  pcNewForm->pcNext=pcShiftedForm;
	  pc=MakeAndForm(FALSE,pcNewForm);
	  *pImmutable=FALSE;
	}
    }
  else {
    pc=pcShiftedForm;
    *pImmutable=FALSE;
  }
  EXIT("ProgressTAlways");
  return pc;
}

/* ProgressDelta

Description:
        Progress a delta formula.
        Copy the formula, shift it and then progress that.
Algorithm:
        p(delta form) => p(s(form))
Scheme:

(define (progress-delta form world/action bindings)
        (let* (
                        (formula (get-delta-formula form))
                        (ispec (get-bf-ispec formula))
                        (shifted-form
                                (append
                                        (list
                                                (get-operator formula)
                                                (ispec-lshift ispec (world/action-action-duration world/action)))
                                        (get-bf-formulas formula))))
                (progress-formula shifted-form world/action bindings)))
*/

CELLP ProgressDelta
(
 CELLP pcFormula,                                        /* calling formula */
 LINEARPLANP plpLinearPlan,                      /* current world/action */
 BINDINGP pbBindings,
 BOOLP pImmutable                                  /* current variables bindings */
 )
{
  CELLP pc;
  CELLP pcISpec;                                          /* interval to shift */
  CELLP pcTail;                                           /* trailing arguments */
  CELLP pcShiftedForm;                            /* shifted formula */
  BOOL immutable;

  ENTER("ProgressDelta",TRUE);

  /* copy the formula */
        
  //pcShiftedForm=CopyCell(pcFormula->pfForm->pcArgs);
  pcShiftedForm=pcFormula->pfForm->pcArgs;
  pcShiftedForm->pfForm=CopyAlloc(pcShiftedForm->pfForm,sizeof(FORMULA));
  pcISpec=pcShiftedForm->pfForm->pcArgs;
  pcTail=pcISpec->pcNext;

  /* shift the ispec and reassemble */
        
  pcISpec=LShiftForm(pcISpec,LinearPlanActionDuration(plpLinearPlan));
  pcISpec->pcNext=pcTail;
  pcShiftedForm->pfForm->pcArgs=pcISpec;

  /* progress the result */
        
  pc=(*pcShiftedForm->pfForm->paAction->pfProgress)(pcShiftedForm,plpLinearPlan,pbBindings,&immutable);
  *pImmutable=immutable;
  EXIT("ProgressDelta");
  return pc;
}

/* ProgressTEventually

Description:
        Progress a timed EVENTUALLY modality.
Scheme:

(define (progress-t-eventually form world/action bindings)
        (let (
                        (ispec (get-bf-ispec form))
                        (forms (get-bf-formulas form))
                        (shifted-form (make-delta-form form)))
                (cond
                        ((ispec<0 ispec)
                                #f) ;timed out
                        ((ispec0in ispec)
                                (let (
                                                (progressed-form
                                                        (progress-formula (first forms) world/action bindings)))
                                        (cond
                                                ((not progressed-form)
                                                        shifted-form)
                                                ((eq? progressed-form #t)
                                                        #t)
                                                ((or-formp progressed-form)
                                                        (make-or-form (append (get-args progressed-form)
                                                                        (list shifted-form))))
                                                (else
                                                        (make-or-form (list progressed-form shifted-form))))))
                        (else
                                shifted-form))))
*/

CELLP ProgressTEventually
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  ISPECP pisISpec;
  CELLP pcForm;
  CELLP pcShiftedForm;
  CELLP pcNewForm;
  BOOL immutable;

  ENTER("ProgressTEventually",TRUE);
  pisISpec=GetBFISpec(pcFormula)->pfForm->uValue.piISpec;
  if(ISpecLt0(pisISpec)) {
    *pImmutable=TRUE;
    pc=MakeFalseForm();                     /* timed out */
  }
  else
    {
      if(ISpec0In(pisISpec))
	{
	  pcForm=GetBFFormulas(pcFormula);
	  pcNewForm=(*pcForm->pfForm->paAction->pfProgress)(pcForm,plpLinearPlan,pbBindings,&immutable);
	  if(TrueFormQ(pcNewForm)) {
	    *pImmutable=TRUE;
	    pc=MakeTrueForm();
	  }
	  else
	    {
	      pcShiftedForm=MakeDeltaForm(FALSE,pcFormula->pcNext?CopyCell(pcFormula):pcFormula);
	      if(FalseFormQ(pcNewForm)) {
		*pImmutable=TRUE;
		pc=pcShiftedForm;
	      }
	      else if(OrFormQ(pcNewForm)) {
		*pImmutable=FALSE;
		pc=MakeOrForm(FALSE,AppendFormula(TRUE,pcNewForm->pfForm->pcArgs,pcShiftedForm));
	      }
	      else
		{
		  if(immutable)
		    pcNewForm=CopyCell(pcNewForm);
		  pcNewForm->pcNext=pcShiftedForm;
		  pc=MakeOrForm(FALSE,pcNewForm);
		  *pImmutable=FALSE;
		}
	    }
	}
      else {
	*pImmutable=FALSE;
	pc=MakeDeltaForm(FALSE,pcFormula->pcNext?CopyCell(pcFormula):pcFormula);       /* same as pcShiftedForm */
      }
    }
  EXIT("ProgressTEventually");
  return pc;
}

/* ProgressTUntil

Description:
        Progress a timed UNTIL modality of the form (condition UNTIL achievement).
Scheme:

(define (progress-t-until form world/action bindings)
        (let (
                        (ispec (get-bf-ispec form))
                        (forms (get-bf-formulas form))
                        (shifted-form (make-delta-form form)))
                (cond
                        ((ispec<0 ispec)
                                #f) ;timed out
                        ((ispec0in ispec)
                                (let (
                                                (progressed-achievement
                                                        (progress-formula (second forms) world/action bindings)))
                                        (if (eq? progressed-achievement #t)
                                                #t
                                                (let (
                                                                (progressed-condition
                                                                        (progress-formula (first forms) world/action bindings)))
                                                        (set! progressed-condition
                                                                (cond
                                                                        ((not progressed-condition)
                                                                                #f)
                                                                        ((eq? progressed-condition #t)
                                                                                shifted-form)
                                                                        ((and-formp progressed-condition)
                                                                                (make-and-form
                                                                                        (append (get-args progressed-condition)
                                                                                                (list shifted-form))))
                                                                        (else
                                                                                (make-and-form
                                                                                        (list progressed-condition shifted-form)))))
                                                        (cond
                                                                ((not progressed-achievement)
                                                                        progressed-condition)
                                                                ((not progressed-condition)
                                                                        progressed-achievement)
                                                                ((or-formp progressed-achievement)
                                                                        (make-or-form
                                                                                (cons progressed-condition
                                                                                        (get-args progressed-achievement))))
                                                                (else
                                                                        (make-or-form (list progressed-achievement
                                                                                        progressed-condition))))))))
                        (else
                                (let (
                                                (progressed-condition
                                                        (progress-formula (first forms) world/action bindings)))
                                        (set! progressed-condition
                                                (cond
                                                        ((not progressed-condition)
                                                                #f)
                                                        ((eq? progressed-condition #t)
                                                                shifted-form)
                                                        ((and-formp progressed-condition)
                                                                (make-and-form
                                                                        (append (get-args progressed-condition)
                                                                                (list shifted-form))))
                                                        (else
                                                                (make-and-form
                                                                        (list progressed-condition shifted-form))))))))))

*/

CELLP ProgressTUntil
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  ISPECP piISpec;
  CELLP pcForm;
  CELLP pcShiftedForm;
  CELLP pcNewCondition;
  CELLP pcNewAchievement;
  BOOL immutableA;
  BOOL immutableC;

  ENTER("ProgressTUntil",TRUE);
  piISpec=GetBFISpec(pcFormula)->pfForm->uValue.piISpec;
  if(ISpecLt0(piISpec))
    {
      *pImmutable=TRUE;
      pc=MakeFalseForm();                     /* timed out */
      EXIT("ProgressTUntil");
      return pc;
    }
  pcForm=GetBFFormulas(pcFormula);
  pcShiftedForm=MakeDeltaForm(FALSE,pcFormula->pcNext?CopyCell(pcFormula):pcFormula);
  if(ISpec0In(piISpec))
    {
      pcNewAchievement=(*pcForm->pcNext->pfForm->paAction->pfProgress)(pcForm->pcNext,plpLinearPlan,pbBindings,&immutableA);
      if(TrueFormQ(pcNewAchievement))
	{
	  *pImmutable=TRUE;
	  pc=MakeTrueForm();
	  EXIT("ProgressTUntil");
	  return pc;
	}
      pcNewCondition=(*pcForm->pfForm->paAction->pfProgress)(pcForm,plpLinearPlan,pbBindings,&immutableC);
      if(FalseFormQ(pcNewCondition)) {
	immutableC=TRUE;
	pcNewCondition=MakeFalseForm();
      }
      else if(TrueFormQ(pcNewCondition)) {
	immutableC=FALSE;
	pcNewCondition=pcShiftedForm;
      }
      else if(AndFormQ(pcNewCondition)) {
	immutableC=FALSE;
	pcNewCondition=MakeAndForm(FALSE,AppendFormula(TRUE,pcNewCondition->pfForm->pcArgs,
		pcShiftedForm));
      }
      else
	{
	  if(immutableC)
	    pcNewCondition=CopyCell(pcNewCondition);
	  pcNewCondition->pcNext=pcShiftedForm;
	  pcNewCondition=MakeAndForm(FALSE,pcNewCondition);
	  immutableC=FALSE;
	}
      if(FalseFormQ(pcNewAchievement)) {
	pc=pcNewCondition;
	*pImmutable=immutableC;
      }
      else if(FalseFormQ(pcNewCondition)) {
	pc=pcNewAchievement;
	*pImmutable=immutableA;
      }
      else if(OrFormQ(pcNewAchievement))
	{
	  if(immutableC)
	    pcNewCondition=CopyCell(pcNewCondition);
	  pcNewCondition->pcNext=pcNewAchievement->pfForm->pcArgs;
	  pc=MakeOrForm(FALSE,pcNewCondition);
	  *pImmutable=FALSE;
	}
      else
	{
	  if(immutableA)
	    pcNewAchievement=CopyCell(pcNewAchievement);
	  pcNewAchievement->pcNext=pcNewCondition;
	  *pImmutable=FALSE;
	  pc=MakeOrForm(FALSE,pcNewAchievement);
	}
      EXIT("ProgressTUntil");
      return pc;
    }
  pcNewCondition=(*pcForm->pfForm->paAction->pfProgress)(pcForm,plpLinearPlan,pbBindings,&immutableC);
  if(FalseFormQ(pcNewCondition)) {
    *pImmutable=TRUE;
    pc=MakeFalseForm();
  }
  else if(TrueFormQ(pcNewCondition)) {
    pc=pcShiftedForm;
    *pImmutable=FALSE;
  }
  else if(AndFormQ(pcNewCondition)) {
    *pImmutable=FALSE;
    pc=MakeAndForm(FALSE,AppendFormula(TRUE,pcNewCondition->pfForm->pcArgs,pcShiftedForm));
  }
  else
    {
      if(immutableC)
	pcNewCondition=CopyCell(pcNewCondition);
      pcNewCondition->pcNext=pcShiftedForm;
      *pImmutable=FALSE;
      pc=MakeAndForm(FALSE,pcNewCondition);
    }
  EXIT("ProgressTUntil");
  return pc;
}

/* Auxiliary Formulas ---------------------------------------------------------- */

/* ProgressBinding

Description:
        Progress a binding node, by extending the current bindings. Also if
        internal formula is not TRUE or FALSE preserve the bindings but reset
        the formula to be the progressed formula.
Algorithm:
        p(binding vars vals form) => (binding vars vals p(form))
Scheme:

(define (progress-binding form world/action bindings)
        (let (
                        (progressed-form #f))
                (set! progressed-form
                        (progress-formula
                                (get-binding-formula form) world/action
                                (extend-bindings (get-binding-vars form)
                                (get-binding-vals form) bindings)))
                (cond
                        ((or (eq? progressed-form #t) (not progressed-form))
                                progressed-form)
                        ((eq? progressed-form (get-binding-formula form))
                                form)
                        (else
                                (make-binding-form (get-binding-vars form)
                                        (get-binding-vals form) progressed-form)))))
*/

CELLP ProgressBinding
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
)
{
	CELLP pc;
	CELLP pcProgressedForm;
	BOOL immutable;
	
	ENTER("ProgressBinding",TRUE);
	pcProgressedForm=(*GetBindingFormula(pcFormula)->pfForm->paAction->pfProgress)
		(GetBindingFormula(pcFormula),plpLinearPlan,
		ExtendBindings(GetBindingVars(pcFormula),GetBindingVals(pcFormula),pbBindings),&immutable);
	if(TrueFormQ(pcProgressedForm)||FalseFormQ(pcProgressedForm))
	{
		*pImmutable=TRUE;
		pc=pcProgressedForm;
	}
	else if(FormulaEqQ(pcProgressedForm,GetBindingFormula(pcFormula)))
	{
		*pImmutable=TRUE;
		pc=pcFormula;
	}
	else {
		*pImmutable=TRUE;
		pc=MakeBindingForm(FALSE,GetBindingVars(pcFormula),GetBindingVals(pcFormula),
			(pcProgressedForm->pcNext&&immutable)?CopyCell(pcProgressedForm):pcProgressedForm);
	}
	EXIT("ProgressBinding");
	return pc;
}

/* Modalities ----------------------------------------------------------------- */

/* ProgressGoal

Description:
        Progress a goal formula. Should not have modalities inside.
Algorithm:
        p(goal form) => ?(goal form)
Scheme:

(define (progress-goal form world/action bindings)
  (eval-formula (first (get-args form)) (get-goal-world) bindings))
*/

CELLP ProgressGoal
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  ENTER("ProgressGoal",TRUE);
  *pImmutable=TRUE;
  pc=pcFormula->pfForm->pcArgs;
  if((*pc->pfForm->paAction->pfEval)(pc,GetGoalWorld(TRUE),pbBindings))
     pc=MakeTrueForm();
  else
     pc=MakeFalseForm();
  EXIT("ProgressGoal");
  return pc;
}

/* ProgressCurrent

Description:
        Progress a current formula. Should not have modalities inside.
Algorithm:
        p(goal form) => ?(goal form)
Scheme:

(define (progress-goal form world/action bindings)
  (eval-formula (first (get-args form)) (get-goal-world) bindings))
*/

CELLP ProgressCurrent
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;

  ENTER("ProgressCurrent",TRUE);
  *pImmutable=TRUE;
  pc=pcFormula->pfForm->pcArgs;
  if((*pc->pfForm->paAction->pfEval)(pc,plpCurrentPlan,pbBindings))
     pc=MakeTrueForm();
  else
     pc=MakeFalseForm();
  EXIT("ProgressCurrent");
  return pc;
}

/* ProgressPrevious

Description:
        Progress a previous formula. Should not have modalities inside.
Algorithm:
        p(previous form) => ?(previous form)
*/

CELLP ProgressPrevious
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  LINEARPLANP plp;
  CELLP pc;
  ENTER("ProgressPrevious",TRUE);
  *pImmutable=TRUE;
  /* if the current plan is not null, get the previous plan */
  plp=plpLinearPlan;
  if(plp)
    plp=plp->plpParent;

  /* if the previous plan exists, evaluate the passed formula, 
     otherwise return FALSE */
        
  pc=pcFormula->pfForm->pcArgs;
  if(plp&&(*pc->pfForm->paAction->pfEval)(pc,plp,pbBindings))
    pc=MakeTrueForm();
  else
    pc=MakeFalseForm();
  EXIT("ProgressPrevious");
  return pc;
}

/* ProgressAtomic

Description:
        Progress an atomic formula.  
Notes:
        This routine works for any predicate that can be immediately reduced to
        true or false (any atomic formula).
Algorithm:
        p(predicate form) => ?(predicate form)
*/

CELLP ProgressAtomic
(
 CELLP pcFormula,
 LINEARPLANP plpLinearPlan,
 BINDINGP pbBindings,
 BOOLP pImmutable
 )
{
  CELLP pc;
  ENTER("ProgressAtomic",TRUE);
  *pImmutable=TRUE;
  StartTimer(tsTimers.adfProgressAtomic);
  if((*pcFormula->pfForm->paAction->pfEval)(pcFormula,plpLinearPlan,pbBindings))
    pc=MakeTrueForm();
  else
    pc=MakeFalseForm();
  StopTimer(tsTimers.adfProgressAtomic);
  EXIT("ProgressAtomic");
  return pc;
}
