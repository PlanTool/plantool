/* world.c

Copyright C, 1996 - 2001  F. Bacchus

Worlds

Functions for maintaining world states (i.e., databases describing
a world state.)

Worlds:

The current world is defined by a database. On top of the database
are other parasitic functions and defined predicates. However
these are dealt with elsewhere. Here we have functions for
maintaining/updating the world database. Note that the only
dynamics in a world (parts that change) are generated via changes
to the database of described predicates/functions. The parasitic
functions/predicates might also change, but only as a result of
changes to the described predicates/functions.

For each described predicate we have a full list of all positive
instances. For each described function we have a full list of all
function values (those values we do not have have the special
value *noValue*.

----------------------------------------------------------------------
Internally world structures are stored in arrays. Each
predicate/function symbols is given an index into this array. This
index is stored on the symbols property list during the setup of
the domain (see the domains module). The array at the symbol's index
stores an argument tree, containing all positive tuples in the
case of a predicate, and all tuples that have a value in the case
of a function.  The argument tree is implemented as a binary
tree, where the tuple acts as a key into this tree, and in the
case of a function the value associated with the key is the function
value.
*/

#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "tlplan.h"
#include "adl.h"
#include "btree.h"
#include "compute.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "iface.h"
#include "list.h"
#include "makeform.h"
#include "oper.h"
#include "search.h"
#include "tl_tab.h"
#include "tlparse.h"
#include "util.h"
#include "world.h"
#include "zone.h"

/* global data */

int nWorldNumber;						/* world counter */
LINEARPLANP plpGoalWorld;				/* the goal world */

/* local data */

static BTREEP *apbtNewWorld;			/* new world pointer */
static CELLP pcAdd;						/* add list */
static CELLP pcAddEnd;
static CELLP pcDel;						/* delete list */
static CELLP pcDelEnd;

/* local function prototypes */

//static BOOL SearchArgTree
//(
//	CELLP pcArg,
//	BTREEP pbtArgTree
//);
static BTREEP InsertArgTree
(
	BTREEP *apbtOwner,
	CELLP pcNewArg,
	BTREEP pbtArgTree,
	CELLP plValue
);
static LISTP ArgTreeToList
(
	char *psName,
	BTREEP pbtArgTree
);
static LISTP ArgTreeToListTransform
(
	CELLP pcSymbol,
	BTREEP pbtNode
);
static CELLP NewArgTreeToListTransform
(
	CELLP pcSymbol,
	BTREEP pbtNode
);

//static unsigned int BTreeSignatureH
//(
//	unsigned int nCRC,					/* initial CRC */
//	BTREEP pbtTree						/* btree to signature */
//);
//static unsigned int BTreeSignatureS
//(
//	unsigned int nCRC,					/* initial CRC */
//	BTREEP pbtTree						/* btree to signature */
//);
static unsigned int BTreeSignatureR
(
	unsigned int nCRC,					/* initial CRC */
	BTREEP pbtTree						/* btree to signature */
);
//static unsigned int BTreeSignatureN
//(
//	unsigned int nCRC,					/* initial CRC */
//	BTREEP pbtTree						/* btree to signature */
//);
static unsigned int FormulaListSignature
(
	unsigned int nCRC,					/* initial crc */
	CELLP pcFormula						/* formula list to signature */
);
static unsigned int FormulaSignature
(
	unsigned int nCRC,					/* initial crc */
	CELLP pcFormula						/* formula to signature */
);
//static BTREEP *CreateWorld
//(
//	CELLP pcDescription
//);
static CELLP ComputePlusEq
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
static CELLP ComputeMinusEq
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

/* MakeWorld

Description:
	Construct an empty world.
Scheme:

(define (make-world )
  "Construct an empty internal world"
  (do ((world (make-vector *number-of-described-symbols*))
	   (i 0 (+ 1 i)))
	  ((=  i *number-of-described-symbols*) world)
	(vector-set! world i (cons '() 0))))
*/

BTREEP *MakeWorld(void)
{
	BTREEP *ppbt;

	ENTER("MakeWorld",TRUE);
	if(nNumberOfDescribedSymbols)
		ppbt=(BTREEP *)MemAlloc(sizeof(BTREEP)*nNumberOfDescribedSymbols);
	else
		ppbt=NULL;
	EXIT("MakeWorld");
	return ppbt;
}

/* MarkWorld

Description:
	Mark a world and its btrees (for garbage collection)
*/

void MarkWorld
(
	BTREEP *apbtWorld
)
{
	int i;

	ENTER("MarkWorld",TRUE);
	if(apbtWorld)
	{
		ZoneMark(apbtWorld);
		for(i=0;i<nNumberOfDescribedSymbols;i++)
			MarkBTree(apbtWorld[i]);
	}
	EXIT("MarkWorld");
}

/* WorldSizeOf

Description:
	Calculate the incrmental size of a world and its btrees.

*/

void WorldSizeOf
(
	BTREEP *apbtWorld,
	int *pnSize
)
{
	int i;

	ENTER("WorldSizeOf",TRUE);
	if(apbtWorld)
	{
		if(!ZoneMarkedQ(apbtWorld))
			*pnSize+=ZoneSizeOf(apbtWorld);
		ZoneMark(apbtWorld);
		for(i=0;i<nNumberOfDescribedSymbols;i++)
			BTreeSizeOf(apbtWorld[i],pnSize);
	}
	EXIT("WorldSizeOf");
}

/* CopyWorld

Description:
	Generate a copy of a world, but share substructures (btrees).
Scheme:

(define (copy-world world)
  (do ((new-world (make-world))
	   (i 0 (+ 1 i)))
	  ((>= i *number-of-described-symbols*) new-world)
	(put-symbol-args new-world i (get-symbol-args world i))
	(put-symbol-args-# new-world i (get-symbol-args-# world i))))
*/

BTREEP *CopyWorld
(
	BTREEP *apbtWorld
)
{
	ENTER("CopyWorld",TRUE);
	apbtNewWorld=(BTREEP *)CopyAlloc(apbtWorld,sizeof(BTREEP)*nNumberOfDescribedSymbols);
	EXIT("CopyWorld");
	return apbtNewWorld;
}

/* -----------------------------------------------------------------------------
Interface routines.
 Convert to and from described lists of predicates/functions.
 Equality predicates are used to set function values.
*/

/* CreateWorld

Description:
	Construct an internal world from a list of literals/functions.
Scheme:

(define (create-world description)
	(do ((world (make-world))
			(index '())
			(lit '()))
		((null? description) world)
		(set! lit (first description))
		(set! description (rest description))
		(if (fn-lit? lit)
			(set! index (get-symbol-index (get-fn-name lit)))
			(set! index (get-symbol-index (get-operator lit))))
		(cond
			((and (predicate? index)
					(not (search-arg-tree (get-args lit)
							(get-symbol-args world index))))
				(put-symbol-args
					world index
					(insert-arg-tree world
						(get-args lit)
						(get-symbol-args world index)
						'()))
				(incr-symbol-args-# world index))
			;;otherwise it is a (= (fn x) y) literal
			((fn-lit? lit)
				(if (not
						(search-arg-tree (get-fn-args lit)
							(get-symbol-args world index)))
					(incr-symbol-args-# world index))
				(put-symbol-args
					world index
					(insert-arg-tree world
						(get-fn-args lit)
						(get-symbol-args world index)
						(get-fn-value lit)))))))
*/

//static BTREEP *CreateWorld
//(
//	CELLP pcDescription					/* list description of world */
//)
//{
//	BTREEP *apbtWorld;
//	CELLP pcLit,pc;
//	SYMBOLINFOP psiSymbolInfo;
//	int nIndex;
//	char ac[40];						/* string buffer */
//
//	ENTER("CreateWorld",TRUE);
//	apbtWorld=MakeWorld();				/* create an empty world */
//
//	for(pcLit=pcDescription;pcLit;pcLit=pcLit->pcNext)
//	{
//		if(FnLitQ(pcLit))
//		{
//			pc=pcLit->pfForm->pcArgs;
//			psiSymbolInfo=GetSymbolInfoPtr(GetName(pc->pfForm,ac));
//			if(!DescribedQ(psiSymbolInfo))	/* if not described symbol */
//				ErrorMessage("CreateWorld:  Not a described function, %s ignored\n",
//					GetName(pc->pfForm,ac));
//			else
//			{
//				nIndex=psiSymbolInfo-asiWorldSymbols;
//				PutSymbolArgs(apbtWorld,nIndex,InsertArgTree(apbtWorld,
//					pc->pfForm->pcArgs,GetSymbolArgs(apbtWorld,nIndex),pc->pcNext));
//			}
//		}
//		else
//		{
//			psiSymbolInfo=GetSymbolInfoPtr(GetName(pcLit->pfForm,ac));
//			if(PredicateQ(psiSymbolInfo))
//			{
//				if(!DescribedQ(psiSymbolInfo))	/* if not described symbol */
//					ErrorMessage("CreateWorld:  Not a described predicate, %s ignored\n",
//						GetName(pcLit->pfForm,ac));
//				else
//				{
//					nIndex=psiSymbolInfo-asiWorldSymbols;
//					if(!BTreeSearch(pcLit->pfForm->pcArgs,GetSymbolArgs(apbtWorld,nIndex)))
//					{
//						PutSymbolArgs(apbtWorld,nIndex,InsertArgTree(apbtWorld,
//							pcLit->pfForm->pcArgs,GetSymbolArgs(apbtWorld,nIndex),NULL));
//					}
//				}
//			}
//			else
//				ErrorMessage("CreateWorld:  Unknown symbol type, %s ignored\n",
//					GetName(pcLit->pfForm,ac));
//		}
//	}
//	EXIT("CreateWorld");
//	return apbtWorld;
//}

/* MakeWorldAction -------------------------------------------------------------

Description:
	Constructor
Notes:
	The action name is copied.
	We only fill in the world/action parts of the plan.
Scheme:

(define (make-world/action world action-name action-duration action-cost
			   action-priority)
  (vector 'world/action
	  world action-name action-duration action-cost action-priority))
*/

LINEARPLANP MakeWorldAction
(
	BTREEP *apbtWorld,
	CELLP pcActionName,
	double dfActionDuration,
	double dfActionCost,
	double dfActionPriority
)
{
	LINEARPLANP plp;

	ENTER("MakeWorldAction",TRUE);
	plp=(LINEARPLANP)MemAlloc(sizeof(struct LinearPlan));
	plp->apbtWorld=apbtWorld;
	plp->pcActionName=pcActionName;
	plp->dfActionDuration=dfActionDuration;
	plp->dfActionCost=dfActionCost;
	plp->dfActionPriority=dfActionPriority;
	plp->nWorldNumber=nWorldNumber++;
	if(!(nWorldNumber&0x0000003F))
		CommandPrintf(stderr,".");

	/* make sure global add and delete lists are empty */

//	pcAdd=NULL;
//	pcAddEnd=(CELLP)&pcAdd;
//	pcDel=NULL;
//	pcDelEnd=(CELLP)&pcDel;

	EXIT("MakeWorldAction");
	return plp;
}

// PrintWorldX

// Description:
//		Debug version of PrintWorld, that can be called while we're processing
//		CreateWorldAction.

#ifdef _DEBUG
BOOL PrintWorldX
(
	BTREEP *apbtWorld,
	int nFile							/* output file handle */
)
{
	FILE *pfFile;
	int nCount;
	int i;

	ENTER("PrintWorld",TRUE);

	/* handle default print world */
	
	pfFile=HandleToFileStream(nFile);
	if(!pfFile)
	{
		ErrorMessage("print-world:  Invalid file handle, %d\n",nFile);
		EXIT("PrintWorld");
		return FALSE;
	}
	for(i=0;i<nNumberOfDescribedSymbols;i++)
	{
		nCount=BTreeCount(GetSymbolArgs(apbtWorld,i),0);
		CommandPrintf(pfFile,"[%s, %d entr%s]",GetSymbolName(i),
			nCount,(nCount==1)?"y":"ies");
		PrintList(pfFile,ArgTreeToList(GetSymbolName(i),GetSymbolArgs(apbtWorld,i)),0);
		CommandPrintf(pfFile,"\n");
	}
	EXIT("PrintWorld");
    return TRUE;
}
#endif // _DEBUG

/* CreateInitialWorldAction

Description:
	Construct an initial world from a list of ADL modifiers.
	Here, we make apbtNewWorld point to the world we create, so we evaluate
	and modify the same world!
Notes:
	We only fill in the world/action portions of the plan.
*/

LINEARPLANP CreateInitialWorldAction
(
	CELLP pcDescription,				/* list description of world */
	CELLP pcActionName,					/* the action that got us here */
	double dfActionDuration,			/* total time so far */
	double dfActionCost,				/* total cost so far */
	double dfActionPriority,			/* this world's priority */
	BINDINGP pbBindings					/* bindings */
)
{
	BTREEP *apbtWorld;
	CELLP pcMod;
	LINEARPLANP plpLinearPlan;
//	double dfTime;

	/* Handle initial world */
	
	ENTER("CreateInitialWorldAction",TRUE);
	apbtNewWorld=apbtWorld=MakeWorld();	/* create an empty world */
	plpSuccessorWorld=plpLinearPlan=MakeWorldAction(apbtWorld,pcActionName,dfActionDuration,
		dfActionCost,dfActionPriority);

	/* make sure global add and delete lists are empty */

	pcAdd=NULL;
	pcAddEnd=(CELLP)&pcAdd;
	pcDel=NULL;
	pcDelEnd=(CELLP)&pcDel;

	for(pcMod=pcDescription;pcMod;pcMod=pcMod->pcNext)
	{
		if(!(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings))
			break;
	}

//	dfTime=GetInternalRunTime();
	
	UpdateWorld();						/* apply changes */

//	dfTime=GetInternalRunTime()-dfTime;
//	CommandPrintf(stderr,"\nUpdate Initial World List:  Elapsed CPU time %.3f sec.\n",dfTime);

	/* Handle initialization sequence */

	for(pcMod=pcInitializationSequence;pcMod;pcMod=pcMod->pcNext)
	{
		int i;

		for(i=0;i<nNumberOfDescribedSymbols;i++)
			plpLinearPlan->apbtWorld[i]=BTreeBalance(plpLinearPlan->apbtWorld[i]);

//		dfTime=GetInternalRunTime();
	
		(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
	
//		dfTime=GetInternalRunTime()-dfTime;
//		CommandPrintf(stderr,"\nCalculate Initialization Formula:  Elapsed CPU time %.3f sec.\n",dfTime);

//		dfTime=GetInternalRunTime();
	
		UpdateWorld();					/* apply changes after every formula */

//		dfTime=GetInternalRunTime()-dfTime;
//		CommandPrintf(stderr,"\nUpdate Initialization Formula:  Elapsed CPU time %.3f sec.\n",dfTime);

	}

	apbtNewWorld=NULL;					/* disable adds and dels on this world */
	plpSuccessorWorld=NULL;
	EXIT("CreateInitialWorldAction");
	return plpLinearPlan;
}

/* InitializeMacroWorld

Description:
	Apply the initialization sequence to the initial world for macro expansion.
Notes:
	We create a new world database so we have a different owner, to avoid 
	corrupting the btrees of latter worlds.
*/

void InitializeMacroWorld
(
	LINEARPLANP plpLinearPlan,			/* our starting world for macro expansion */
	BINDINGP pbBindings
)
{
	CELLP pcMod;

	/* Handle initial world */
	
	ENTER("InitializeMacroWorld",TRUE);

	apbtNewWorld=CopyWorld(plpLinearPlan->apbtWorld);	/* copy initial world */
	plpLinearPlan->apbtWorld=apbtNewWorld;	/* discard old world database */
	plpSuccessorWorld=plpLinearPlan;

	/* make sure global add and delete lists are empty */

	pcAdd=NULL;
	pcAddEnd=(CELLP)&pcAdd;
	pcDel=NULL;
	pcDelEnd=(CELLP)&pcDel;

	/* Handle initialization sequence */

	for(pcMod=pcInitializationSequence;pcMod;pcMod=pcMod->pcNext)
	{
		(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
		UpdateWorld();					/* apply changes after every formula */
	}

	apbtNewWorld=NULL;					/* disable adds and dels on this world */
	plpSuccessorWorld=NULL;
	EXIT("InitializeMacroWorld");
}

/* CreateGoalWorldAction

Description:
	Construct a goal world from a list of ADL modifiers.
	Here, we make apbtNewWorld point to the world we create, so we evaluate
	and modify the same world!
Notes:
	We only fill in the world/action portions of the plan.
*/

LINEARPLANP CreateGoalWorldAction
(
	CELLP pcDescription,				/* list description of world */
	CELLP pcActionName,					/* the action that got us here */
	double dfActionDuration,			/* total time so far */
	double dfActionCost,				/* total cost so far */
	double dfActionPriority,			/* this world's priority */
	BINDINGP pbBindings					/* bindings */
)
{
	BTREEP *apbtWorld;
	CELLP pcMod;
	LINEARPLANP plpLinearPlan;

	/* Handle goal world */
	
	ENTER("CreateGoalWorldAction",TRUE);
	apbtNewWorld=apbtWorld=MakeWorld();	/* create an empty world */
	plpLinearPlan=MakeWorldAction(apbtWorld,pcActionName,dfActionDuration,
		dfActionCost,dfActionPriority);

	/* make sure global add and delete lists are empty */

	pcAdd=NULL;
	pcAddEnd=(CELLP)&pcAdd;
	pcDel=NULL;
	pcDelEnd=(CELLP)&pcDel;

	for(pcMod=pcDescription;pcMod;pcMod=pcMod->pcNext)
	{
		if(!(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings))
			break;
	}
	UpdateWorld();						/* apply changes */

	/* Handle goal sequence */

	for(pcMod=pcGoalSequence;pcMod;pcMod=pcMod->pcNext)
	{
		(*pcMod->pfForm->paAction->pfEval)(pcMod,plpLinearPlan,pbBindings);
		UpdateWorld();					/* apply changes after every formula */
	}

	apbtNewWorld=NULL;					/* disable adds and dels on this world */
	EXIT("CreateGoalWorldAction");
	return plpLinearPlan;
}

/* CreateGoalWorldAction

Description:
	Set up a goal world.
Scheme:

(define (create-goal-world/action goal-list)
  (set! goal-world
	(make-world/action (create-world goal-list)
			   '() 0 0 0)))
*/

//void CreateGoalWorldAction
//(
//	CELLP pcGoalList
//)
//{
//	int i;
//
//	ENTER("CreateGoalWorldAction",TRUE);
//	plpGoalWorld=MakeWorldAction(CreateWorld(pcGoalList),NULL,0,0,0);
//
//	for(i=0;i<nNumberOfDescribedSymbols;i++)
//		plpGoalWorld->apbtWorld[i]=BTreeBalance(plpGoalWorld->apbtWorld[i]);
//	
//	EXIT("CreateGoalWorldAction");
//}

/* GetGoalWorld

Description:
	Fetch the goal world.
Notes:
	Since the goal world is only available for classical goals, it is an
	error to reference the goal world while searching for an extended goal.
	The bMessage flag should be TRUE, except for background maintenance calls,
    like garbage collection.
Scheme:

(define (get-goal-world )
  goal-world)
*/

LINEARPLANP GetGoalWorld
(
	BOOL bMessage						/* print message flag */
)
{
	ENTER("GetGoalWorld",TRUE);
	if(bMessage&&!plpGoalWorld)
	{
		ErrorMessage("GetGoalWorld:  No goal world\n");
		longjmp(*pjbCurrentJump,1);		/* tear down stack */
	}
	EXIT("GetGoalWorld");
	return plpGoalWorld;
}

/* PrintWorld

Description:
	If the user has defined their own print routine, evaluate that.
	Otherwise, print a standard description of a world.

Scheme:

(define (default-print-world world/action . optional-args)
  (do ((world (world/action-world world/action))
	   (i 0 (+ 1 i))
	   (stream (if (null? optional-args)
		   *trace-stream*
		   (first optional-args))))
	  ((>= i *number-of-described-symbols*))
	(format stream "~%<~S,~S>:~@4A"
		(get-symbol-name i) (get-symbol-args-# world i)
		(arg-tree-to-list (get-symbol-name i)
				  (get-symbol-args world i)))))
*/

BOOL PrintWorld
(
	int nFile,							/* file handle */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	BTREEP *apbtWorld;
	FILE *pfFile;
	int nCount;
	int i;
	char ac[40];

	ENTER("PrintWorld",TRUE);

	/* handle user defined print world predicate */

	if(pcPrintWorld)
	{
		BINDINGP pb;
		CELLP pcVar,pcVal;

		/* bind our stream to the caller's (only) argument */
		
		pcVar=pcPrintWorld->pfForm->pcArgs;
		pcVal=MakeIntegerForm(nFile);
		pb=ExtendBindings(pcVar,pcVal,pbBindings);

		/* evaluate caller's formula for side effects */

		if(!(*pcPrintWorld->pfForm->paAction->pfEval)(pcPrintWorld,plpLinearPlan,pb))
			ErrorMessage("print-world:  Print routine %s terminated abnormally\n",
				GetName(pcPrintWorld->pfForm,ac));
		EXIT("PrintWorld");
		return TRUE;
	}

	/* handle default print world */
	
	pfFile=HandleToFileStream(nFile);
	if(!pfFile)
	{
		ErrorMessage("print-world:  Invalid file handle, %d\n",nFile);
		EXIT("PrintWorld");
		return FALSE;
	}
	apbtWorld=LinearPlanWorld(plpLinearPlan);
	for(i=0;i<nNumberOfDescribedSymbols;i++)
	{
		nCount=BTreeCount(GetSymbolArgs(apbtWorld,i),0);
		CommandPrintf(pfFile,"[%s, %d entr%s]",GetSymbolName(i),
			nCount,(nCount==1)?"y":"ies");
		PrintList(pfFile,ArgTreeToList(GetSymbolName(i),GetSymbolArgs(apbtWorld,i)),0);
		CommandPrintf(pfFile,"\n");
	}
	EXIT("PrintWorld");
    return TRUE;
}

/* -----------------------------------------------------------------------------
 Argument trees are simply a list of all distinct arguments of a
 predicate true in the current world. They are implemented as sorted
 binary trees, and we use the binaryTree support code.

 Throw up an abstraction layer between binary trees and argument
 trees.
*/

/* InsertArgTree

Description:
	Insert NEW arg owned by owner into argTree (perhaps owned by someone else).
Scheme:

(define (insert-arg-tree owner new-arg arg-tree value)
  (if value
	  (tree-insert owner
		   (make-b-tree new-arg value '() '() owner)
		   arg-tree)
	  (tree-insert owner
		   (make-b-tree new-arg '() '() '() owner)
		   arg-tree)))
*/

static BTREEP InsertArgTree
(
	BTREEP *apbtOwner,					// world database to update
	CELLP pcNewArg,						// list of arguments (i.e. key) to add
	BTREEP pbtArgTree,					// pointer to btree to operate on
	CELLP pcValue						// function value (optional)
)
{
	BTREEP pbt;

	ENTER("InsertArgTree",TRUE);
	pbt=BTreeInsert(apbtOwner,MakeBTree(pcNewArg,pcValue,NULL,NULL,apbtOwner),
		pbtArgTree);
	EXIT("InsertArgTree");
	return pbt;
}

/* DeleteArgTree

Description:
	Delete OLD argument owned by owner from arg tree
	(perhaps owned by someone else).
Scheme:

(define (delete-arg-tree owner arg arg-tree)
  (tree-delete owner arg arg-tree))
*/

//static BTREEP DeleteArgTree
//(
//	BTREEP *apbtOwner,
//	CELLP pcArg,
//	BTREEP pbtArgTree
//)
//{
//	BTREEP pbt;
//
//	ENTER("DeleteArgTree",TRUE);
//	pbt=BTreeDelete(apbtOwner,pcArg,pbtArgTree);
//	EXIT("DeleteArgTree");
//	return pbt;
//}

/* SearchArgTree

Description:
	Return TRUE if arg is found in argTree.
Scheme:

(define (search-arg-tree arg arg-tree)
  (if (tree-search arg arg-tree) #t #f))
*/

//static BOOL SearchArgTree
//(
//	CELLP pcArgs,
//	BTREEP pbtArgTree
//)
//{
//	BOOL b;
//
//	ENTER("SearchArgTree",TRUE);
//	b=BTreeSearch(pcArgs,pbtArgTree)?TRUE:FALSE;
//	EXIT("SearchArgTree");
//	return b;
//}

/* FindValueArgTree

Description:
	Fetch a function value from a btree.
Scheme:

(define (find-value-arg-tree arg arg-tree)
  (let ((node (tree-search arg arg-tree)))
	(if node (b-tree-value node) *no-value*)))
*/

static CELLP FindValueArgTree
(
	CELLP pcArg,
	BTREEP pbtArgTree
)
{
	BTREEP pbtNode;
	CELLP pc;

	ENTER("FindValueArgTree",TRUE);
	pbtNode=BTreeSearch(pcArg,pbtArgTree);
	pc=pbtNode?pbtNode->pcValue:NULL;
	EXIT("FindValueArgTree");
	return pc;
}

/* ArgTreeToList

Description:
	Generate sorted list from tree.
Scheme:

(define (arg-tree-to-list symbol-name arg-tree)
	(tree-to-list arg-tree
		:transform
		(lambda (b-tree-node)
			(if (b-tree-value b-tree-node)
				(list '=
					(cons symbol-name (b-tree-key b-tree-node))
					(b-tree-value b-tree-node))
				(cons symbol-name (b-tree-key b-tree-node))))))
*/

static LISTP ArgTreeToList
(
	char *psName,						/* symbol name */
	BTREEP pbtArgTree					/* its argument tree */
)
{
	CELL cCell;
	FORMULA fFormula;
	LISTP pl;

	ENTER("ArgTreeToList",TRUE);
	fFormula.psName=psName;				/* fill in symbol name */
	cCell.pfForm=&fFormula;
	cCell.pcNext=NULL;
	pl=BTreeToList(pbtArgTree,NULL,NULL,ArgTreeToListTransform,&cCell);
	EXIT("ArgTreeToList");
	return pl;
}

static LISTP ArgTreeToListTransform
(
	CELLP pcSymbol,
	BTREEP pbtNode
)
{
	CELLP pc;
	CELLP pcKey,pcValue;
	LISTP plStart,plEnd;
	LISTP pl;
	char ac[40];						/* string buffer */

	ENTER("ArgTreeToListTransform",TRUE);
	plStart=NULL;
	plEnd=(LISTP)&plStart;
	pcKey=pbtNode->pcKey;
	if(pcKey)
	{
		for(pc=pcKey;pc;pc=pc->pcNext)
			plEnd=plEnd->plNext=NewList(ATOM_IDENT,GetName(pc->pfForm,ac));
	}

	pcValue=pbtNode->pcValue;
	if(pcValue)							/* if function literal */
	{
		plEnd=NewList(ATOM_IDENT,GetName(pcSymbol->pfForm,ac));
		plEnd->plNext=plStart;
		pl=Cons(NewList(ATOM_IDENT,"="),
			Cons(NewList(ATOM_LISTP,"",plEnd),
			NewList(pcValue->pfForm->nType,GetName(pcValue->pfForm,ac))));
	}
	else if(pcKey)
		pl=Cons(NewList(ATOM_IDENT,GetName(pcSymbol->pfForm,ac)),plStart);
	else
		pl=NewList(ATOM_LISTP,"",NewList(ATOM_IDENT,GetName(pcSymbol->pfForm,ac)));
	EXIT("ArgTreeToListTransform");
	return pl;
}

/* ArgTreeToFormulaList

Description:
	Generate a sorted list from a tree.
*/

CELLP ArgTreeToFormulaList
(
	BTREEP *apbtWorld,					/* array of btrees */
	int nIndex							/* array index */
)
{
	CELL cSymbol;
	FORMULA fSymbol;
	CELLP pc;
	SYMBOLINFOP psi;
	BTREEP pbtArgTree					/* its argument tree */

	ENTER("ArgTreeToList",TRUE);
	psi=asiWorldSymbols+nIndex;
	pbtArgTree=apbtWorld[nIndex];
	
	memset(&fSymbol,0,sizeof(FORMULA));
	fSymbol.psName=psi->psName;			/* fill in symbol name */
	fSymbol.nType=ATOM_IDENT;
	fSymbol.uValue.psiSymbolInfo=psi;
	fSymbol.paAction=psi->paAction;
	cSymbol.pfForm=&fSymbol;
	cSymbol.pcNext=NULL;
	pc=BTreeToFormulaList(pbtArgTree,NULL,NULL,NewArgTreeToListTransform,&cSymbol);
	EXIT("ArgTreeToList");
	return pc;
}

static CELLP NewArgTreeToListTransform
(
	CELLP pcSymbol,
	BTREEP pbtNode
)
{
	CELLP pc;
	CELLP pcKey,pcValue;

	ENTER("ArgTreeToListTransform",TRUE);
	pc=pcKey=CopyFormula(pcSymbol);		/* make a new formula */
	pcKey->pfForm->pcArgs=pbtNode->pcKey;
	pcValue=pbtNode->pcValue;
	if(pcValue)							/* if function literal */
		pc=MakeEqForm(FALSE,pcKey,pcValue);
	EXIT("ArgTreeToListTransform");
	return pc;
}

/* -----------------------------------------------------------------------------
 World equality test
*/

/* WorldEqQ

Description:
	Do a series of tests in order of complexity to determine if two
	worlds are equal.  Extra described symbols are ignored.
Scheme:

(define (world= world1 world2)
  (cond
   ((eq? world1 world2)
	#t)
   ((do ((i 0 (+ 1 i))
	 (found? #f))
	((or (>= i *number-of-described-symbols*) found?)
	 found?)
	  (set! found? (not (= (get-symbol-args-# world1 i)
			   (get-symbol-args-# world2 i)))))
	#f)
   ((do ((i 0 (+ 1 i))
	 (found? #f))
	((or (>= i *number-of-described-symbols*) found?)
	 found?)
	  (set! found? (not (tree= (get-symbol-args world1 i)
				   (get-symbol-args world2 i)))))
	#f)
   (else #t)))
*/

BOOL WorldEqQ
(
	LINEARPLANP plpPlan1,
	LINEARPLANP plpPlan2
)
{
	BTREEP *apbtWorld1;
	BTREEP *apbtWorld2;
	int i;

	ENTER("WorldEqQ",TRUE);
	if(plpPlan1->nSignature!=plpPlan2->nSignature)
	{
		EXIT("WorldEqQ");
		return FALSE;
	}
	if(plpPlan1==plpPlan2)
	{
		EXIT("WorldEqQ");
		return TRUE;
	}

	apbtWorld1=LinearPlanWorld(plpPlan1);
	apbtWorld2=LinearPlanWorld(plpPlan2);
	
	for(i=0;i<nNumberOfCheckedSymbols;i++)
	{
		if(!BTreeEqQ(GetSymbolArgs(apbtWorld1,i),(GetSymbolArgs(apbtWorld2,i))))
		{
			EXIT("WorldEqQ");
//			Message("CRC Leak: (not (= world%d world%d)) \n",
//				plpPlan1->nWorldNumber,plpPlan2->nWorldNumber);
			return FALSE;
		}
	}
	EXIT("WorldEqQ");
	return TRUE;
}

// debug

//BOOL WorldEqQ
//(
//	LINEARPLANP plpPlan1,
//	LINEARPLANP plpPlan2
//)
//{
//	BTREEP *apbtWorld1;
//	BTREEP *apbtWorld2;
//	int i;
//
//	ENTER("WorldEqQ",TRUE);
//	if(plpPlan1==plpPlan2)
//	{
//		EXIT("WorldEqQ");
//		if(plpPlan1->nSignature!=plpPlan2->nSignature)
//			Message("CRC failed\n");
//		return TRUE;
//	}
//
//	apbtWorld1=LinearPlanWorld(plpPlan1);
//	apbtWorld2=LinearPlanWorld(plpPlan2);
//	
//	for(i=0;i<nNumberOfDomainSymbols;i++)
//		if(!BTreeEqQ(GetSymbolArgs(apbtWorld1,i),(GetSymbolArgs(apbtWorld2,i))))
//		{
//			EXIT("WorldEqQ");
//			if(plpPlan1->nSignature==plpPlan2->nSignature)
//				Message("CRC leak\n");
//			return FALSE;
//		}
//	if(plpPlan1->nSignature!=plpPlan2->nSignature)
//		Message("CRC failed\n");
//	EXIT("WorldEqQ");
//	return TRUE;
//}

/* Signature routines ------------------------------------------------------- */

/* WorldSignature

Description:
	Calculate a signature longword for a world.  We calculate a CRC-32 value
	for a world by enumerating all btrees in canonical order (sorted order).
	We convert to lowercase to ensure the signature is case independent.
Notes:
	So we aren't confused by runs across two or more btrees, we insert
	a delimiter between signaturing btrees.
*/

void WorldSignature
(
	LINEARPLANP plpPlan					/* plan to update */
)
{
	BTREEP *apbtWorld;					/* world to signature */
	BTREEP pbtTree;						/* btree to signature */
	int nCRC;							/* running crc */
	unsigned char c;					// delimiter
	int i;								/* loop index */

	nCRC=-1;							/* initial seed is -1 */
	c=0xA5;								// delimiter
	apbtWorld=LinearPlanWorld(plpPlan);
	for(i=0;i<nNumberOfCheckedSymbols;i++)
	{
		nCRC=CRC32(&c,sizeof(unsigned char),nCRC);
		pbtTree=GetSymbolArgs(apbtWorld,i);
		if(pbtTree)
			nCRC=BTreeSignatureR(nCRC,pbtTree);
	}
	plpPlan->nSignature=~nCRC;			/* the result is complemented */
	EXIT("WorldSignature");
}

/* BTreeSignatureS

Description:
	Calculate a signature for an entire btree.
	The tree is traversed "in-order".
Note:
	This version is uses the btree stack.
	It is reentrant, but moderately fast.
*/

//static unsigned int BTreeSignatureS
//(
//	unsigned int nCRC,					/* initial CRC */
//	BTREEP pbtTree						/* btree to signature */
//)
//{
//	BTREEP pbt;
//	BTREEP pbtCurrent;
//
//	ENTER("BTreeSignatureS",TRUE);
//
//	/* Find the minimum node... while saving traversal context on btree stack */
//
//	CheckBTreeStack();
//	BTreePush(NULL);
//	for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
//	{
//		CheckBTreeStack();
//		BTreePush(pbt);
//	}
//	
//	for(pbtCurrent=BTreePop();pbtCurrent;pbtCurrent=BTreePop())
//	{
//		// signature current node
//		
//		nCRC=FormulaListSignature(nCRC,pbtCurrent->pcKey);
//		if(pbtCurrent->pcValue)
//			nCRC=FormulaSignature(nCRC,pbtCurrent->pcValue);
//
//		/* find successor node...  saving any new context on btree stack */
//	
//		for(pbt=pbtCurrent->pbtRight;pbt;pbt=pbt->pbtLeft)
//		{
//			CheckBTreeStack();
//			BTreePush(pbt);
//		}
//	}
//
//	EXIT("BTreeSignatureS");
//	return nCRC;
//}

/* BTreeSignatureR

Description:
	Calculate a signature for an entire btree.
	The tree is traversed "in-order".
Note:
	This version is recursive.
	It is reentrant, but fast.
*/

static unsigned int BTreeSignatureR
(
	unsigned int nCRC,					/* initial CRC */
	BTREEP pbtTree						/* btree to signature */
)
{
	ENTER("BTreeSignatureR",TRUE);

	/* calculate the signature for the left subtree */
		
	if(pbtTree->pbtLeft)
		nCRC=BTreeSignatureR(nCRC,pbtTree->pbtLeft);

	/* calculate the signature for the current node */
		
	nCRC=FormulaListSignature(nCRC,pbtTree->pcKey);
	if(pbtTree->pcValue)
		nCRC=FormulaSignature(nCRC,pbtTree->pcValue);

	/* calculate the signature for the right subtree */
		
	if(pbtTree->pbtRight)
		nCRC=BTreeSignatureR(nCRC,pbtTree->pbtRight);

	EXIT("BTreeSignatureR");
	return nCRC;
}

/* BTreeSignatureN

Description:
	Calculate a signature for an entire btree.
	The tree is traversed "in-order".
Note:
	This version uses pointers on btrees, which are shared between worlds.
	It is non-reentrant, but quite fast.
*/

//static unsigned int BTreeSignatureN
//(
//	unsigned int nCRC,					/* initial CRC */
//	BTREEP pbtTree						/* btree to signature */
//)
//{
//	BTREEP pbt;							/* temporary pointer */
//	BTREEP pbtCurrent;					/* pointer to enumerated node */
//	BTREEP pbtContext;					/* pointer to next node */
//
//	ENTER("BTreeSignatureN",TRUE);
//
//	/* find the left most node and thread back pointers. */
//
//	pbtContext=NULL;
//	for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
//	{
//		pbt->pbtNext=pbtContext;
//		pbtContext=pbt;
//	}
//
//	/* enumerate the btree */
//
//	while(pbtContext)
//	{
//		pbtCurrent=pbtContext;			/* pop */
//		pbtContext=pbtContext->pbtNext;		
//
//		/* set up context to point to element, and thread back pointers */
//
//		if(pbtCurrent->pbtRight)
//		{
//			pbt=pbtCurrent->pbtRight;
//			pbt->pbtNext=pbtContext;
//			pbtContext=pbt;
//
//			for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft)
//			{
//				pbt->pbtNext=pbtContext;
//				pbtContext=pbt;
//			}
//		}	
//
//		/* calculate the signature for the current node */
//		
//		nCRC=FormulaListSignature(nCRC,pbtCurrent->pcKey);
//		if(pbtCurrent->pcValue)
//			nCRC=FormulaSignature(nCRC,pbtCurrent->pcValue);
//	}
//	EXIT("BTreeSignatureN");
//	return nCRC;
//}

/* BTreeSignatureH

Description:
	Calculate a signature for an entire btree.
	The tree is traversed "in-order".
Note:
	This version allocates memory from the heap to store its traversal context.
	It is reentrant, but expensive in terms of both cpu and memory.
*/

//static unsigned int BTreeSignatureH
//(
//	unsigned int nCRC,					/* initial CRC */
//	BTREEP pbtTree						/* btree to signature */
//)
//{
//	BTREEP pbt,pbtCurrent;				/* btree pointers */
//	BTCONTEXTP pbtcStack;
//	BTCONTEXTP pbtcNode;
//	BTCONTEXTP pbtc;
//
//	ENTER("BTreeSignatureH",TRUE);
//
//	/* set up the initial stack:
//	find a path to the minimum element of the tree and put that on the stack. */
//
//	pbtcStack=pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//	pbtcNode->pbtcNext=NULL;
//	pbtcNode->pbtTree=pbtTree;
//
//	for(pbt=pbtTree;pbt->pbtLeft;pbt=pbt->pbtLeft)
//	{
//		pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//		pbtcNode->pbtcNext=pbtcStack;
//		pbtcStack=pbtcNode;
//		pbtcNode->pbtTree=pbt->pbtLeft;
//	}
//
//	/* enumerate the btree */
//
//	for(pbtc=pbtcStack;pbtc;pbtc=pbtcStack)
//	{
//		pbtCurrent=pbtc->pbtTree;			/* pop the next node from the stack */
//		pbtcStack=pbtc->pbtcNext;
//
//		/* find a path to the successor and put that on the stack */
//
//		if(pbtCurrent->pbtRight)
//		{
//			pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//			pbtcNode->pbtcNext=pbtcStack;
//			pbtcStack=pbtcNode;
//			pbtcNode->pbtTree=pbtCurrent->pbtRight;
//
//			for(pbt=pbtCurrent->pbtRight;pbt->pbtLeft;pbt=pbt->pbtLeft)
//			{
//				pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//				pbtcNode->pbtcNext=pbtcStack;
//				pbtcStack=pbtcNode;
//				pbtcNode->pbtTree=pbt->pbtLeft;
//			}
//		}	
//
//		/* calculate the signature for the current node */
//		
//		nCRC=FormulaListSignature(nCRC,pbtCurrent->pcKey);
//		if(pbtCurrent->pcValue)
//			nCRC=FormulaSignature(nCRC,pbtCurrent->pcValue);
//	}
//	EXIT("BTreeSignatureH");
//	return nCRC;
//}

/* FormulaListSignature

Description:
	Calculate a canonical CRC for a formula list.
*/

static unsigned int FormulaListSignature
(
	unsigned int nCRC,					/* initial crc */
	CELLP pcFormula						/* formula list to signature */
)
{
	CELLP pc;							/* list pointer */
	unsigned int n;						/* running crc */
	unsigned char c;
	
	ENTER("FormulaListSignature",TRUE);

	/* So we aren't confused simple boolean nodes, we insert
	a delimiter if a node is null.  */
	
	if(!pcFormula)
	{
		c=0xA4;
		n=CRC32(&c,sizeof(unsigned char),nCRC);
		EXIT("FormulaListSignature");
		return n;
	}	
	n=nCRC;
	for(pc=pcFormula;pc;pc=pc->pcNext)
		n=FormulaSignature(n,pc);
	EXIT("FormulaListSignature");
	return n;
}

/* FormulaSignature

Description:
	Calculate a canonical CRC for a formula.
Notes:
	For numeric values, we convert integers to floating point and signature
	the floating point binary value.
	For everything else, we convert names and string values to lower case and
	signture that.
*/

static unsigned int FormulaSignature
(
	unsigned int nCRC,					/* initial crc */
	CELLP pcFormula						/* formula to signature */
)
{
	unsigned int n;						/* running crc */
	double df;							/* floating point value */
	char ac[40];						/* string buffer */
	char *ps;							/* string pointer */

	ENTER("FormulaSignature",TRUE);
	if(!pcFormula)
	{
		EXIT("FormulaSignature");
		return nCRC;
	}

	/* treat numbers specially */

	n=nCRC;
	if(pcFormula->pfForm->nType==ATOM_INTEGER)
	{
		df=(double)pcFormula->pfForm->uValue.nInteger;
		n=CRC32((unsigned char *)&df,sizeof(double),n);
		EXIT("FormulaSignature");
		return n;
	}
	if(pcFormula->pfForm->nType==ATOM_FLOAT)
	{
		n=CRC32((unsigned char *)&pcFormula->pfForm->uValue.dfFloat,sizeof(double),n);
		EXIT("FormulaSignature");
		return n;
	}

	/* strings and other things */

	ps=GetName(pcFormula->pfForm,ac);
	n=CRC32((unsigned char *)ps,strlen(ps),n);

	if(pcFormula->pfForm->pcVars)
		n=FormulaListSignature(n,pcFormula->pfForm->pcVars);

	if(pcFormula->pfForm->pcGenLit)
		n=FormulaListSignature(n,pcFormula->pfForm->pcGenLit);

	if(pcFormula->pfForm->pcArgs)
		n=FormulaListSignature(n,pcFormula->pfForm->pcArgs);

	EXIT("FormulaSignature");
	return n;
}

/* Actions interface ----------------------------------------------------------- */

/* EvalAdd

Description:
	Add a list of literals to the global add list.
	These will be used by UpdateWorld to update the new world.
Notes:
	The use of CopyAlloc instead of CopyFormula "temporarily" shares some
	of the Add formula with the add list.  So it's not a good idea to
	explicitly free the add list!
Scheme:

(define (add-world-lit pred-sym args world)
	(let ((arg-tree '())
			(index '()))
		(set! index (get-symbol-index pred-sym))
		(set! arg-tree (get-symbol-args world index))
		(if (not (search-arg-tree args arg-tree))
			(begin
				(put-symbol-args
					world index (insert-arg-tree world args arg-tree '()))
				(incr-symbol-args-# world index)))
		world))
*/

BOOL EvalAdd
(
	CELLP pcFormula,					/* add formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
)
{
	CELLP pc,pc1,pc2,pc3;
	CELLP pcArgs;
	CELLP pcValue;
	CELLP pcTerm1,pcTerm2;
	SYMBOLINFOP psiSymbolInfo;
	int nIndex;
	char ac[40];						/* string buffer */

	ENTER("EvalAdd",TRUE);

	for(pc1=pcFormula->pfForm->pcArgs;pc1;pc1=pc1->pcNext)
	{
		if(EqFormQ(pc1))				/* if this is a function */
		{
			pc=pc1->pfForm->pcArgs;
			pcValue=ComputeTerm(pc->pcNext,plpLinearPlan,pbBindings);
			if(!pcValue)
				TermError("eval-add",pc->pcNext,pbBindings);
		}
		else if(PlusEqFormQ(pc1))		/* if this is a function */
		{
			pc=pc1->pfForm->pcArgs;
			pcTerm1=ComputeTerm(pc,plpLinearPlan,pbBindings);
			if(!pcTerm1)
				TermError("eval-add",pc,pbBindings);
			pcTerm2=ComputeTerm(pc->pcNext,plpLinearPlan,pbBindings);
			if(!pcTerm2)
				TermError("eval-add",pc->pcNext,pbBindings);
			pcTerm1->pcNext=pcTerm2;
			pcValue=ComputePlusEq(pcTerm1,plpLinearPlan,pbBindings);
		}
		else if(MinusEqFormQ(pc1))		/* if this is a function */
		{
			pc=pc1->pfForm->pcArgs;
			pcTerm1=ComputeTerm(pc,plpLinearPlan,pbBindings);
			if(!pcTerm1)
				TermError("eval-add",pc,pbBindings);
			pcTerm2=ComputeTerm(pc->pcNext,plpLinearPlan,pbBindings);
			if(!pcTerm2)
				TermError("eval-add",pc->pcNext,pbBindings);
			pcTerm1->pcNext=pcTerm2;
			pcValue=ComputeMinusEq(pcTerm1,plpLinearPlan,pbBindings);
		}
		else							/* it must be a predicate */
		{
			pc=pc1;
			pcValue=NULL;
		}
		if(pc->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("eval-add:  Not a symbol, \"%s\" ignored\n",
				GetName(pc->pfForm,ac));
			continue;
		}
		psiSymbolInfo=pc->pfForm->uValue.psiSymbolInfo;
		nIndex=psiSymbolInfo-asiWorldSymbols;
		if(nIndex<0||nIndex>=nNumberOfDescribedSymbols)	/* if not described symbol */
		{
			ErrorMessage("eval-add:  Not a described symbol, \"%s\" ignored\n",
				GetName(pc->pfForm,ac));
			continue;
		}
		pcArgs=NULL;
		if(pc->pfForm->pcArgs)
		{
			pcArgs=ComputeTerms(pc->pfForm->pcArgs,plpLinearPlan,pbBindings);
			if(!pcArgs)
				TermError("eval-add",pcFormula,pbBindings);
		}

		pc2=(CELLP)MemAlloc(sizeof(CELL));
		pc2->pfForm=(FORMULAP)CopyAlloc(pc1->pfForm,sizeof(FORMULA));
		if(EqFormQ(pc1)||PlusEqFormQ(pc1)||MinusEqFormQ(pc1))	/* if this is a function */
		{
			pc3=(CELLP)MemAlloc(sizeof(CELL));
			pc3->pfForm=(FORMULAP)CopyAlloc(pc->pfForm,sizeof(FORMULA));
			pc3->pfForm->pcArgs=pcArgs;
			pc3->pcNext=pcValue;
			pc2->pfForm->pcArgs=pc3;
		}
		else
			pc2->pfForm->pcArgs=pcArgs;

		pcAddEnd=pcAddEnd->pcNext=pc2;
	}
	EXIT("EvalAdd");
	return TRUE;
}

/* EvalDel

Description:
	Add a list of literals to the global delete list.
	These will be used by UpdateWorld to update the new world.
Notes:
	The use of CopyAlloc instead of CopyFormula "temporarily" shares some
	of the Del formula with the delete list.  So it's not a good idea to
	explicitly free the delete list!
Scheme:

(define (delete-world-lit pred-sym args world)
  (let ((arg-tree '())
	(index '()))
	(set! index (get-symbol-index pred-sym))
	(set! arg-tree (get-symbol-args world index))
	(if (search-arg-tree args arg-tree)
	(begin (put-symbol-args
		world index (delete-arg-tree world args arg-tree))
		   (decr-symbol-args-# world index)))
	world))
*/

BOOL EvalDel
(
	CELLP pcFormula,					/* del formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
)
{
	CELLP pc,pc1,pc2;
	CELLP pcArgs;
	SYMBOLINFOP psiSymbolInfo;
	int nIndex;
	char ac[80];

	ENTER("EvalDel",TRUE);
	for(pc1=pcFormula->pfForm->pcArgs;pc1;pc1=pc1->pcNext)
	{
		if(EqFormQ(pc1)||PlusEqFormQ(pc1)||MinusEqFormQ(pc1))	/* if this is a function */
		{
			pc=pc1->pfForm->pcArgs;
			Message("eval-del:  Cannot delete a function symbol, \"%s\" ignored.\n",
				GetName(pc->pfForm,ac));
			PrintFormula(pfError,pc1,0);
			continue;
		}
		else
			pc=pc1;

		if(pc->pfForm->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("eval-del:  Not a symbol, \"%s\" ignored\n",
				GetName(pc->pfForm,ac));
			continue;
		}
		psiSymbolInfo=pc->pfForm->uValue.psiSymbolInfo;
		nIndex=psiSymbolInfo-asiWorldSymbols;
		if(nIndex<0||nIndex>=nNumberOfDescribedSymbols)	/* if not described symbol */
		{
			ErrorMessage("eval-del:  Not a described symbol, \"%s\" ignored\n",
				GetName(pc->pfForm,ac));
			continue;
		}
		pcArgs=NULL;
		if(pc->pfForm->pcArgs)
		{
			pcArgs=ComputeTerms(pc->pfForm->pcArgs,plpLinearPlan,pbBindings);
			if(!pcArgs)
				TermError("eval-del",pcFormula,pbBindings);
		}

		pc2=(CELLP)MemAlloc(sizeof(CELL));
		pc2->pfForm=(FORMULAP)CopyAlloc(pc1->pfForm,sizeof(FORMULA));
		pc2->pfForm->pcArgs=pcArgs;

		pcDelEnd=pcDelEnd->pcNext=pc2;
	}
	EXIT("EvalDel");
	return TRUE;
}

/* EvalUpdateWorld

Description:
	Update the new world with the global add and delete lists
*/

BOOL EvalUpdateWorld
(
	CELLP pcFormula,					/* add formula */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings					/* current bindings */
)
{
	UpdateWorld();
	return TRUE;
}

/* MSRandx

Description:
	Generate a random number.
	This algorithm is compatible with Microsoft's rand function.
*/

static int nSeed;						/* plan number */

static int MSRandx(void)
{
	nSeed=nSeed*214013+2531011;			/* 0x00034DFD & 0x00269EC3 */
	return (nSeed>>16)&0x00007FFF;
}

/* PermuteFormulaListX

Description:
	Permute the order of a list of formulas.
Notes:
	This version is disused.  The algorithm is N^2 in list length.
*/

//static CELLP PermuteFormulaListX
//(
//	CELLP pcList						/* list of formulas to permute */
//)
//{
//	int nLength;						/* length of the list */
//	int nRand;							/* random selection */
//	int i,j;							/* loop indices */
//	CELLP pc;							/* block pointer */
//	CELLP pcStart,pcEnd;				/* list head */
//	CELLP pc1;							/* pointer to previous formula */
//
//	/* get the length of the list */
//
//	nLength=0;
//	for(pc=pcList;pc;pc=pc->pcNext)
//		nLength++;
//
//	/* select items at random from list, and move to new list */
//
//	pcStart=NULL;
//	pcEnd=(CELLP)&pcStart;
//
//	for(i=nLength;i>0;i--)
//	{
//		nRand=(MSRandx()%i)-1;			/* generate random index */
//		pc1=(CELLP)&pcList;				/* locate preceding entry in list */
//		for(j=0;j<nRand;j++)
//			pc1=pc1->pcNext;
//		pcEnd=pcEnd->pcNext=pc1->pcNext;	/* transfer to other list */
//		if(pc1->pcNext)
//			pc1->pcNext=pc1->pcNext->pcNext;
//	}
//	pcEnd->pcNext=NULL;					/* terminate list */
//	return pcStart;
//}

/* PermuteFormulaList

Description:
	Permute the order of a list of formulas.
Notes:
	This version allocates memory to improve its performance...
	it's linear in list length.
*/

static CELLP PermuteFormulaList
(
	CELLP pcList						/* list of formulas to permute */
)
{
	int nLength;						/* length of the list */
	int nRand;							/* random selection */
	int i;								/* loop indices */
	CELLP pc;							/* block pointer */
	CELLP pcStart,pcEnd;				/* list head */
	CELLP *ppcArray;					/* permutation pointer array */
	CELLP *ppc;							/* permutation array pointer */

	/* get the length of the list */

	nLength=0;
	for(pc=pcList;pc;pc=pc->pcNext)
		nLength++;

	/* allocate and fill in the pointer array */
	
	ppcArray=(CELLP *)malloc(sizeof(CELLP)*(nLength+1));	/* we need an extra spot so we can swap in 2 moves instead of 3 */
	if(!ppcArray)
	{
		ErrorMessage("Failed to allocate memory.");
		exit(1);
	}
	ppc=ppcArray;
	for(pc=pcList;pc;pc=pc->pcNext)
		*ppc++=pc;

	/* randomly permute the pointer array */

	for(i=nLength;i>0;i--)
	{
		nRand=MSRandx()%i;				/* generate random index */
		ppcArray[i]=ppcArray[nRand];	/* move random pointer to end of array */
		ppcArray[nRand]=ppcArray[i-1];	/* more penultimate pointer to random location */
	}		

	/* reassemble list in permuted order */

	pcStart=NULL;
	pcEnd=(CELLP)&pcStart;

	for(i=1;i<=nLength;i++)
		pcEnd=pcEnd->pcNext=ppcArray[i];	/* transfer permuted cell to new list */
	pcEnd->pcNext=NULL;					/* terminate list */

	free(ppcArray);
	return pcStart;
}

/* UpdateWorld

Description:
	Apply the global add and delete lists to the new world.
*/

void UpdateWorld(void)
{
	CELLP pc,pc1;
	CELLP pcValue;
	CELLP pcArgs;
	BTREEP pbtArgTree;
	SYMBOLINFOP psiSymbolInfo;
	int nIndex;
	char ac[80];

	StartTimer(tsTimers.adfBTreeModify);

	/* first apply the deletes */

	for(pc=pcDel;pc;pc=pc->pcNext)
	{
		psiSymbolInfo=pc->pfForm->uValue.psiSymbolInfo;
		nIndex=psiSymbolInfo-asiWorldSymbols;
		pbtArgTree=GetSymbolArgs(apbtNewWorld,nIndex);
		pcArgs=pc->pfForm->pcArgs;
		if(BTreeSearch(pcArgs,pbtArgTree))
		{
			PutSymbolArgs(apbtNewWorld,nIndex,
				BTreeDelete(apbtNewWorld,pcArgs,pbtArgTree));
		}
		else
		{
			Message("eval-del:  Deleting nonexistant symbol \"%s\" in world %d\n",
				GetName(pc->pfForm,ac),plpSuccessorWorld->nWorldNumber);
			PrintFormula(pfError,pc,0);
		}
	}
	pcDel=NULL;
	pcDelEnd=(CELLP)&pcDel;

	/* then apply the adds */

	pcAdd=PermuteFormulaList(pcAdd);	/* try to keep the btrees balanced */
	for(pc1=pcAdd;pc1;pc1=pc1->pcNext)
	{
		if(EqFormQ(pc1)||PlusEqFormQ(pc1)||MinusEqFormQ(pc1))	/* if this is a function */
		{
			pc=pc1->pfForm->pcArgs;
			pcValue=pc->pcNext;			/* += and -= have already been taken care of */
		}
		else							/* must be a predicate */
		{
			pc=pc1;
			pcValue=NULL;
		}
		psiSymbolInfo=pc->pfForm->uValue.psiSymbolInfo;
		if(!psiSymbolInfo)
		{
			Message("eval-del:  Cannot add non-described symbol \"%s\", addition ignored.\n",
				GetName(pc->pfForm,ac));
			PrintFormula(pfError,pc1,0);
			continue;
		}
		nIndex=psiSymbolInfo-asiWorldSymbols;
		pbtArgTree=GetSymbolArgs(apbtNewWorld,nIndex);
		pcArgs=pc->pfForm->pcArgs;
//		if(BTreeSearch(pcArgs,pbtArgTree))
//		{
//			if(PredicateQ(psiSymbolInfo)&&!psiSymbolInfo->bRewritable)
//			{
//				Message("eval-add:  Rewriting predicate symbol \"%s\"\n",
//					GetName(pc->pfForm,ac));
//				PrintFormula(pfError,pc1,0);
//			}
//			pbtArgTree=BTreeDelete(apbtNewWorld,pcArgs,pbtArgTree);
//		}
//		PutSymbolArgs(apbtNewWorld,nIndex,
//			InsertArgTree(apbtNewWorld,pcArgs,pbtArgTree,pcValue));
		if(BTreeSearch(pcArgs,pbtArgTree))
		{
			if(PredicateQ(psiSymbolInfo)&&!psiSymbolInfo->bRewritable)
			{
				Message("eval-add:  Rewriting predicate symbol \"%s\" in world %d\n",
					GetName(pc->pfForm,ac),plpSuccessorWorld->nWorldNumber);
				PrintFormula(pfError,pc1,0);
			}
			else						/* function:  always update function values */
			{
				pbtArgTree=BTreeDelete(apbtNewWorld,pcArgs,pbtArgTree);
				PutSymbolArgs(apbtNewWorld,nIndex,
					InsertArgTree(apbtNewWorld,pcArgs,pbtArgTree,pcValue));
			}
		}
		else
			PutSymbolArgs(apbtNewWorld,nIndex,
				InsertArgTree(apbtNewWorld,pcArgs,pbtArgTree,pcValue));
	}

	pcAdd=NULL;
	pcAddEnd=(CELLP)&pcAdd;
	StopTimer(tsTimers.adfBTreeModify);
}

/* Evaluator interface ---------------------------------------------------------

Provide information about the current world.

 1. Evaluate ground literal for described predicate in world.
 2. Return generator for described predicate in world.
 3. Evaluate ground function for descrbibed function in world.
	*Alternate compile lisp functions to do these tasks*
*/

/* EvalDescPredicate

Description:
	Evaluate lit in world.
Scheme:

(define (eval-desc-predicate index args world/action)
  (search-arg-tree
   args (get-symbol-args (world/action-world world/action) index)))
*/

/* FAHIEM DEC 27th 2000. 
   Use nocopy version of computeterms, and allocate space for
   pcTerms on the stack. There is bound to be a better way of doing this,
   but for now simply allocate sufficent space, say 50 cells (we never
   have a described predicate of more than 50 arguments!)
*/

BOOL EvalDescPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELL pcTerms[50];
	int nIndex;
	BOOL b,bTerms;
	
	ENTER("EvalDescPredicate",TRUE);
	nIndex=pcFormula->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
	if(pcFormula->pfForm->pcArgs)
	{
		bTerms=ComputeTermsNoCopy(pcFormula->pfForm->pcArgs,pcTerms,plpLinearPlan,pbBindings);
		if(!bTerms)
			TermError("eval-desc-predicate",pcFormula,pbBindings);
	}
	b=(BOOL)BTreeSearch(pcTerms,GetSymbolArgs(LinearPlanWorld(plpLinearPlan),nIndex));
	EXIT("EvalDescPredicate");
	return b;
}

/*
BOOL EvalDescPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcTerms;
	int nIndex;
	BOOL b;

	ENTER("EvalDescPredicate",TRUE);
	nIndex=pcFormula->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
	pcTerms=NULL;
	if(pcFormula->pfForm->pcArgs)
	{
		pcTerms=ComputeTerms(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
		if(!pcTerms)
			TermError("eval-desc-predicate",pcFormula);
	}
	b=(BOOL)BTreeSearch(pcTerms,GetSymbolArgs(LinearPlanWorld(plpLinearPlan),nIndex));
	EXIT("EvalDescPredicate");
	return b;
}
*/

/* ComputeDescFunction

Description:
	Compute function in world, args are fully evaluated.
Scheme:

(define (eval-desc-function index args world/action)
  (find-value-arg-tree
   args
   (get-symbol-args (world/action-world world/action) index)))
*/

/* FAHIEM DEC 27th 2000. 
   Use nocopy version of computeterms, and allocate space for
   pcTerms on the stack. There is bound to be a better way of doing this,
   but for now simply allocate sufficent space, say 50 cells (we never
   have a described predicate of more than 50 arguments!)
*/

CELLP ComputeDescFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELL pcTerms[50];
	int nIndex;
	CELLP pc,pc1;
	BOOL bTerms;
	
	ENTER("ComputeDescFunction",TRUE);
	nIndex=pcFormula->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
	if(pcFormula->pfForm->pcArgs)
	{
		bTerms=ComputeTermsNoCopy(pcFormula->pfForm->pcArgs,pcTerms,plpLinearPlan,pbBindings);
		if(!bTerms)
			TermError("compute-desc-function",pcFormula,pbBindings);
	}
	if(!plpLinearPlan->apbtWorld[nIndex])	// if function isn't initialized
		return 0;
	pc=FindValueArgTree(pcTerms,plpLinearPlan->apbtWorld[nIndex]);
	pc1=CopyCell(pc);
	EXIT("ComputeDescFunction");
	return pc1;
}

/*
CELLP ComputeDescFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pcTerms;
	int nIndex;
	CELLP pc,pc1;

	ENTER("ComputeDescFunction",TRUE);
	nIndex=pcFormula->pfForm->uValue.psiSymbolInfo-asiWorldSymbols;
	pcTerms=NULL;
	if(pcFormula->pfForm->pcArgs)
	{
		pcTerms=ComputeTerms(pcFormula->pfForm->pcArgs,plpLinearPlan,pbBindings);
		if(!pcTerms)
			TermError("compute-desc-function",pcFormula);
	}
	if(!plpLinearPlan->apbtWorld[nIndex])	// if function isn't initialized
		return 0;
	pc=FindValueArgTree(pcTerms,plpLinearPlan->apbtWorld[nIndex]);
	pc1=CopyCell(pc);
	EXIT("ComputeDescFunction");
	return pc1;
}
*/

/* ZoneCopyWorld

Description:
	ZoneCopy a world and its btrees
*/

//void ZoneCopyWorld
//(
//	BTREEP *apbtWorld
//)
//{
//	int i;
//
//	ENTER("ZoneCopyWorld",TRUE);
//	if(apbtWorld)
//	{
//		ZoneCopy(apbtWorld);
//		for(i=0;i<nNumberOfDescribedSymbols;i++)
//			ZoneCopyBTree(apbtWorld[i]);
//	}
//	EXIT("ZoneCopyWorld");
//}

/* ZoneRelocWorld

Description:
	ZoneReloc a world and its btrees
*/

//void ZoneRelocWorld
//(
//	BTREEP *apbtWorld
//)
//{
//	int i;
//
//	ENTER("ZoneRelocWorld",TRUE);
//	if(apbtWorld)
//	{
//		for(i=0;i<nNumberOfDescribedSymbols;i++)
//		{
//			ZoneReloc(&apbtWorld[i]);
//			ZoneRelocBTree(apbtWorld[i]);
//		}
//	}
//	EXIT("ZoneRelocWorld");
//}

/* ComputePlusEq

Description:
	Add a value to a function.
*/

static CELLP ComputePlusEq
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfSum;
	BOOL bInteger;
	int nSum;

	ENTER("ComputePlusEq",TRUE);
	bInteger=TRUE;
	dfSum=0.0;
	nSum=0;

	for(pc=pcFormula;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-plus-eq",pcFormula,pbBindings);
		if(bInteger)
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					nSum+=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfSum=nSum+pcValue->pfForm->uValue.dfFloat;
					bInteger=FALSE;
					break;
				default:
					ErrorMessage("ComputePlusEq:  Invalid or non-numeric argument\n");
					pc=MakeIntegerForm(0);
					EXIT("ComputePlusEq");
					return pc;
			}
		}
		else
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					dfSum+=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfSum+=pcValue->pfForm->uValue.dfFloat;
					break;
				default:
					ErrorMessage("ComputePlusEq:  Invalid or non-numeric argument\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputePlusEq");
					return pc;
			}
		}
	}
	if(bInteger)
		pc=MakeIntegerForm(nSum);
	else
		pc=MakeFloatForm(dfSum);
	EXIT("ComputePlusEq");
	return pc;
}

/* ComputeMinusEq

Description:
	Subtract a value from a function.
*/

static CELLP ComputeMinusEq
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	CELLP pcValue;
	double dfDifference;
	BOOL bInteger;
	int nDifference;

	ENTER("ComputeMinusEq",TRUE);
	bInteger=TRUE;
	nDifference=0;
	dfDifference=0.0;

	pc=pcFormula;
	if(!pc)
	{
		pc=MakeIntegerForm(0);
		EXIT("ComputeMinusEq");
		return pc;
	}
	pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("compute-minus-eq",pcFormula,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			nDifference=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			dfDifference=pcValue->pfForm->uValue.dfFloat;
			bInteger=FALSE;
			break;
		default:
			ErrorMessage("ComputeMinusEq:  Invalid or non-numeric argument\n");
			pc=MakeFloatForm(0.0);
			EXIT("ComputeMinusEq");
			return pc;
	}
	if(!pc->pcNext)
	{
		if(bInteger)
			pc=MakeIntegerForm(-nDifference);
		else
			pc=MakeFloatForm(-dfDifference);
		EXIT("ComputeMinusEq");
		return pc;
	}

	for(pc=pc->pcNext;pc;pc=pc->pcNext)
	{
		pcValue=ComputeTerm(pc,plpLinearPlan,pbBindings);
		if(!pcValue)
			TermError("compute-minus-eq",pcFormula,pbBindings);
		if(bInteger)
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					nDifference-=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfDifference=nDifference-pcValue->pfForm->uValue.dfFloat;
					bInteger=FALSE;
					break;
				default:
					ErrorMessage("ComputeMinusEq:  Invalid or non-numeric argument\n");
					pc=MakeIntegerForm(0);
					EXIT("ComputeMinusEq");
					return pc;
			}
		}
		else
		{
			switch(pcValue->pfForm->nType)
			{
				case ATOM_INTEGER:
					dfDifference-=pcValue->pfForm->uValue.nInteger;
					break;
				case ATOM_FLOAT:
					dfDifference-=pcValue->pfForm->uValue.dfFloat;
					break;
				default:
					ErrorMessage("ComputeMinusEq:  Invalid or non-numeric argument\n");
					pc=MakeFloatForm(0.0);
					EXIT("ComputeMinusEq");
					return pc;
			}
		}
	}
	if(bInteger)
		pc=MakeIntegerForm(nDifference);
	else
		pc=MakeFloatForm(dfDifference);
	EXIT("ComputeMinusEq");
	return pc;
}

// debug

//void debug_world
//(
//	LINEARPLANP plp
//)
//{
//	SYMBOLINFOP psi;
//	int nCount;
//	int nBTree;
//	BTREEP *apbt;
//	BTREEP pbt;
//
//	psi=GetSymbolInfoPtr(IdentAlloc("walked"));
//	if(!psi)
//		printf("Can't find walked\n");
//	apbt=plp->apbtWorld;
//	nBTree=psi-asiWorldSymbols;
//	pbt=GetSymbolArgs(apbt,nBTree);
//
//	nCount=BTreeCount(pbt,0);
//	printf("Cardinality %d\n",nCount);
//
//	PrintList(stdout,ArgTreeToList(GetSymbolName(nBTree),GetSymbolArgs(apbt,nBTree)),0);
//}

