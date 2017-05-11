/* interval.c

Copyright C, 1996 - 99	F. Bacchus

Functions to handle interval specs.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tlplan.h"
#include "interval.h"
#include "formula.h"
#include "util.h"

/* ISpecLShift

Description:
	Compute a new ispec shifted to the left by delta; i.e., ispec - delta.
Scheme:

(define (ispec-lshift ispec delta)
  (let ((lb (ispec- (second ispec) delta))
		(ub (ispec- (third ispec) delta)))
	(list (first ispec) lb ub (fourth ispec))))
*/

//ISPECP ISpecLShift
//(
//	ISPECP piISpec,
//	double dfDelta
//)
//{
//	ISPECP pi;							/* pointer to interval spec */
//
//	ENTER("ISpecLShift",TRUE);
//	pi=(ISPECP)CopyAlloc(piISpec,sizeof(struct ISpec));
//	if(!piISpec->bLowerInfinite)
//		pi->dfLower-=dfDelta;
//	if(!piISpec->bUpperInfinite)
//		pi->dfUpper-=dfDelta;
//	EXIT("ISpecLShift");
//	return(pi);
//}

/* LShiftForm

Description:
	Shift an ispec formula to the left by delta; i.e., ispec - delta.
*/

CELLP LShiftForm
(
	CELLP pcFormula,					/* ispec formula to shift */
	double dfDelta						/* time to shift */
)
{
	CELLP pc;
	ISPECP pi;							/* interval pointer */
	char acBuffer[64];
	char *ps;
	int nPosition;

	ENTER("LShiftForm",TRUE);
	pc=CopyFormula(pcFormula);
	pi=pc->pfForm->uValue.piISpec;
	if(!pi->bLowerInfinite)
		pi->dfLower-=dfDelta;
	if(!pi->bUpperInfinite)
		pi->dfUpper-=dfDelta;

	ps=acBuffer;
	*ps++=(char)(pi->bLowerOpen?'(':'[');
	if(pi->bLowerInfinite)
		strcpy(ps,"inf-,");
	else
		sprintf(ps,"%f,",pi->dfLower);
	ps+=strlen(ps);
	if(pi->bUpperInfinite)
		strcpy(ps,"inf+");
	else
		sprintf(ps,"%f",pi->dfUpper);
	ps+=strlen(ps);
	*ps++=(char)(pi->bUpperOpen?')':']');
	*ps=NULL;

	pc->pfForm->psName=IdentAllocAndPos(acBuffer,&nPosition);
	pc->pfForm->nHashPos=nPosition;
	EXIT("LShiftForm");
	return(pc);
}

/* ISpecLt0

Description:
	Test if ispec has timed out, i.e. ispec < 0.
Scheme:

(define (ispec<0 ispec)
  (or (ispec< (third ispec) 0)
	  (and (ispec= 0 (third ispec)) (eq? (fourth ispec) '<))))
*/

BOOL ISpecLt0
(
	ISPECP piISpec
)
{
	BOOL b;

	ENTER("ISpecLt0",FALSE);
	if(piISpec->bUpperInfinite)
		b=FALSE;
	else if(piISpec->bUpperOpen)
		b=piISpec->dfUpper<=0.0;
	else
		b=piISpec->dfUpper<0.0;
	EXIT("ISpecLt0");
	return b;
}

/* ISpec0In

Description:
	Test if ispec overlaps 0.
Scheme:

(define (ispec0in ispec)
  (or (and (ispec< (second ispec) 0) (ispec> (third ispec) 0))
	  (and (ispec= 0 (second ispec)) (eq? (first ispec) '<))
	  (and (ispec= 0 (third ispec)) (eq? (fourth ispec) '>))))
*/

BOOL ISpec0In
(
	ISPECP piISpec
)
{
	ENTER("ISpec0In",TRUE);
	if(piISpec->bLowerOpen)
	{
		if(!piISpec->bLowerInfinite&&piISpec->dfLower>=0.0)
		{
			EXIT("ISpec0In");
			return FALSE;
		}
	}
	else
	{
		if(!piISpec->bLowerInfinite&&piISpec->dfLower>0.0)
		{
			EXIT("ISpec0In");
			return FALSE;
		}
	}
	if(piISpec->bUpperOpen)
	{
		if(!piISpec->bUpperInfinite&&piISpec->dfUpper<=0.0)
		{
			EXIT("ISpec0In");
			return FALSE;
		}
	}
	else
	{
		if(!piISpec->bUpperInfinite&&piISpec->dfUpper<0.0)
		{
			EXIT("ISpec0In");
			return FALSE;
		}
	}
	EXIT("ISpec0In");
	return TRUE;
}



