/* btree.c

Copyright C, 1996 - 2002, F. Bacchus

Persistent Binary Trees

	a) Constructor, Selectors, Modifiers, copier, and predicate
	b) Comparing arguments
	c) Tree utilities.

Worlds are lists of atomic formulas, the set of atomic formulas
that are true in the current world.	Instead of maintaining these as
unstructured lists, we maintain these as argument trees.

Each world is a set of argument trees, one tree for each predicate.
The predicate's argument tree contains the instances that are true
of that predicate in this world.  For example, if in the current
world {on(A,B), on(B,C), on(C,D)} are the true instances of "on",
then the argument tree for "on" will contain the lists (A B), (B C)
and (C D).

The argument tree is a binary tree, so we can test if
a particular instance of "on" is true or false in the current world
quite efficiently.	When we modify a world by applying an action we
generate a new set of argument trees by updating them according to the
action's add and delete lists.	Persistence comes from the fact that
these new argument trees share as much structure as is possible with
the old trees, hence they are "persistent binary trees" in the
data structures terminology.

This module contains functions for dealing with argument trees.
Note that this implementation does not allow an argument tree to
contain duplicates (equal? keys are not allowed). Furthermore
insert tree will not insert an item that is already in the tree.
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
#include "btree.h"
#include "formula.h"
#include "iface.h"
#include "list.h"
#include "search.h"
#include "tl_tab.h"
#include "tlparse.h"
#include "util.h"
#include "zone.h"
#include "var.h"

#define MIN_BTREE_STACK			32		// default btree stack size
#define BTREE_STACK_SIGNATURE	0xABCDDCBA	// stack reserved world signature

// Global Data

BTREEP *ppbtStack=NULL;					// btree stack
int nBTreeSP=0;							// btree stack pointer
int nBTreeMaxSP=0;						// btree stack limit

/* Local Function Prototypes */

static BOOL BTreeCheck
(
	BTREEP pbt1,
	BTREEP pbt2,
	BTREEP pbtTree1,
	BTREEP pbtTree2
);
static BTREEP Insert
(
	BTREEP *apbtOwner,					/* owning world */
	BTREEP pbtNode,						/* node to insert */
	BTREEP pbtTree						/* tree to act on */
);
static LISTP BTreeWalk
(
	LISTP plAccumulator,				/* accumulated list */
	BTREEP pbtTree,						/* current tree location */
	BOOL (*pfTest)(CELLP, BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* test argument (optional) */
	LISTP (*pfTransform)(CELLP, BTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
);
static void FillNodeArray
(
	BTREEP **pppbt,						/* address of pointer to node pointer array */
	BTREEP pbt							/* pointer to current node */
);
static BTREEP BTreeBuild
(
	BTREEP *ppbtNodes,					/* sorted node array */
	int i,								/* lower limit */
	int j								/* upper limit */
);
static CELLP NewBTreeWalk
(
	CELLP pcAccumulator,				/* accumulated list */
	BTREEP pbtTree,						/* current tree location */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* test argument (optional) */
	CELLP (*pfTransform)(CELLP, BTREEP),	/* node transform */
	CELLP pcArg							/* argument to transform */
);
/* static BOOL ArgsLtQ */
/* ( */
/* 	CELLP pcArgList1, */
/* 	CELLP pcArgList2 */
/* ); */
/* static int ArgsCmpQ */
/* ( */
/* 	CELLP pcArgList1, */
/* 	CELLP pcArgList2 */
/* ); */
/*FAHIEM DEC 2000*/

static int BTreeDepth
(
	BTREEP pbtTree						/* btree to measure */
);

//static void ShowBTreeStack
//(
//	char *ps,							// (one word) description
//	int nBaseSP							// base stack pointer
//);

/* Comparitors (any change here should be replicated in hbtree.c) */

static BOOL (*pfKeyLtQ)(CELLP, CELLP)=ArgsLtQ_Hash;
static BOOL (*pfKeyEqQ)(CELLP, CELLP)=FormulaListEqQ;
static BOOL (*pfArgsCmpQ)(CELLP, CELLP)=ArgsCmpQ_Hash;
static BOOL (*pfArgsCmpQWithVars)(CELLP, CELLP)=ArgsCmpQWithVars_Hash;

// Old comparitors
//static BOOL (*pfKeyLtQ)(CELLP, CELLP)=ArgsLtQ;
//static BOOL (*pfKeyEqQ)(CELLP, CELLP)=FormulaListEqQ;
//static BOOL (*pfArgsCmpQ)(CELLP, CELLP)=ArgsCmpQ;
//static BOOL (*pfArgsCmpQWithVars)(CELLP, CELLP)=ArgsCmpQWithVars;



/* MakeBTree

Description:
	Constructor
Scheme:

(define (make-b-tree key value left right owner)
  (vector 'b-tree key value left right owner))
*/

BTREEP MakeBTree
(
	CELLP pcKey,
	CELLP pcValue,
	BTREEP pbtLeft,
	BTREEP pbtRight,
	BTREEP *apbtOwner
)
{
	BTREEP pbt;

	ENTER("MakeBTree",TRUE);
	pbt=(BTREEP)MemAlloc(sizeof(struct BTree));
	pbt->pcKey=CopyFormulaList(pcKey);
	pbt->pcValue=pcValue;
	pbt->pbtLeft=pbtLeft;
	pbt->pbtRight=pbtRight;
	pbt->apbtOwner=apbtOwner;
	EXIT("MakeBTree");
	return pbt;
}

/* CopyBTree

Description:
	Copy a single btree structure.
Scheme:

(define copy-b-tree vector-copy)
*/

static BTREEP CopyBTree
(
	BTREEP pbt
)
{
	BTREEP pbtCopy;

	ENTER("CopyBTree",TRUE);
	pbtCopy=(BTREEP)CopyAlloc(pbt,sizeof(struct BTree));
	EXIT("CopyBTree");
	return pbtCopy;
}

/* ArgsLtQ

Description:
	Return TRUE iff argList1 should sort prior to argList2,
	this function must be called with identical size lists.
Scheme:

(define (args< arg-list1 arg-list2)
  (let ((x1 (convert-to-string (first arg-list1)))
	(x2 (convert-to-string (first arg-list2))))
	(cond
	 ((string=? x1 x2) (args< (rest arg-list1) (rest arg-list2)))
	 (else (string<? x1 x2)))))
*/

BOOL ArgLt_Hash
(
 FORMULAP pf1, 
 FORMULAP pf2
) 
{
  // just in case...
  IdentName(pf1); IdentName(pf2);
  // do comparison
  //  printf("Comparing (%s,%d) with (%s,%d)\n",
  //	 pf1->psName,pf1->nHashPos,
  //	 pf2->psName,pf2->nHashPos);
  if (pf1->nHashPos!=pf2->nHashPos) 
    return pf1->nHashPos<pf2->nHashPos;
  else 
    return (stricmp(pf1->psName,pf2->psName)<0);
}



BOOL ArgsLtQ_Hash
(
	CELLP pcArgList1,
	CELLP pcArgList2
)
{
  CELLP pc1,pc2;						/* list pointers */
  char *ps1,*ps2;						/* string pointers */

  ENTER("ArgsLtQ_Hash",TRUE);
  for
    (
     pc1=pcArgList1,pc2=pcArgList2;
     pc1&&pc2;
     pc1=pc1->pcNext,pc2=pc2->pcNext
     )
    {
		ps1=IdentName(pc1->pfForm);
		ps2=IdentName(pc2->pfForm);
		if(!StringEqQ(ps1,ps2))
		{
			EXIT("ArgsLtQ");
			return ArgLt_Hash(pc1->pfForm,pc2->pfForm);
		}
    }
    
  EXIT("ArgsLtQ_Hash");
  return FALSE;						/* lists are equal */
  
}





BOOL ArgsLtQ
(
	CELLP pcArgList1,
	CELLP pcArgList2
)
{
	CELLP pc1,pc2;						/* list pointers */
	char *ps1,*ps2;						/* string pointers */

	ENTER("ArgsLtQ",TRUE);
	for
	(
		pc1=pcArgList1,pc2=pcArgList2;
		pc1&&pc2;
		pc1=pc1->pcNext,pc2=pc2->pcNext
	)
	{
		ps1=IdentName(pc1->pfForm);
		ps2=IdentName(pc2->pfForm);
		if(!StringEqQ(ps1,ps2))
		{
			EXIT("ArgsLtQ");
			return StringLtQ(ps1,ps2);
		}
	}
	EXIT("ArgsLtQ");
	return FALSE;						/* lists are equal */
}


//int ArgsCmpQ_Hash //************** HEREEEE **********************//


/* ArgsCmpQ

Description:
	Return -1 if argList1 should sort prior to argList2.
	Return 0 if argList1 is equal to argList2 up to the first variable
	Return 1 if argList1 should sort after argList2.
Note:
	This function must be called with identical size lists.
*/



int ArgsCmpQ_Hash
(
	CELLP pcArgList1,
	CELLP pcArgList2
)
{
	CELLP pc1,pc2;						/* list pointers */
	char *ps1,*ps2;						/* string pointers */

	ENTER("ArgsCmpQ_Hash",TRUE);
	for
	(
		pc1=pcArgList1,pc2=pcArgList2;
		pc1&&pc2;
		pc1=pc1->pcNext,pc2=pc2->pcNext
	)
	{
		ps1=IdentName(pc1->pfForm);
		ps2=IdentName(pc2->pfForm);
		if(!StringEqQ(ps1,ps2))
		{
		  if (ArgLt_Hash(pc1->pfForm,pc2->pfForm))
			{
				EXIT("ArgsCmpQ_Hash");
				return -1;
			}
			EXIT("ArgsCmpQ_Hash");
			return 1;
		}
	}
	EXIT("ArgsCmpQ_Hash");
	return 0;							/* lists are equal */
}



/* ArgsCmpQ

Description:
	Return -1 if argList1 should sort prior to argList2.
	Return 0 if argList1 is equal to argList2 up to the first variable
	Return 1 if argList1 should sort after argList2.
Note:
	This function must be called with identical size lists.
*/

int ArgsCmpQ
(
	CELLP pcArgList1,
	CELLP pcArgList2
)
{
	CELLP pc1,pc2;						/* list pointers */
	char *ps1,*ps2;						/* string pointers */

	ENTER("ArgsCmpQ",TRUE);
	for
	(
		pc1=pcArgList1,pc2=pcArgList2;
		pc1&&pc2;
		pc1=pc1->pcNext,pc2=pc2->pcNext
	)
	{
		ps1=IdentName(pc1->pfForm);
		ps2=IdentName(pc2->pfForm);
		if(!StringEqQ(ps1,ps2))
		{
		  if (StringLtQ(ps1,ps2))
			{
				EXIT("ArgsCmpQ");
				return -1;
			}
			EXIT("ArgsCmpQ");
			return 1;
		}
	}
	EXIT("ArgsCmpQ");
	return 0;							/* lists are equal */
}

/* FAHIEM DEC 2000
   A new version of ArgsComQ which allows for variables in the tuple.

Description:
	Return -1 if argList1 should sort prior to argList2 ignoring
	          all arguments in argList1 from the first variable on.
	Return 0 if argList1 is equal to argList2 up to the first variable
	          in argList1
	Return 1 if argList1 should sort after argList2 ignoring
	          all arguments in argList1 from the first variable on.
Note:
	This function must be called with pcArgList2 at least as long as the
	prefix of pcargList1 that appears before the first variable in
        pcargList1. 
*/



int ArgsCmpQWithVars_Hash
(
	CELLP pcArgList1,
	CELLP pcArgList2
)
{
	CELLP pc1,pc2;						/* list pointers */
	char *ps1,*ps2;						/* string pointers */

	ENTER("ArgsCmpQWithVars",TRUE);
	for
	(
		pc1=pcArgList1,pc2=pcArgList2;
		pc1&&pc2&&!VarQ(pc1);             /*up to first var*/
		pc1=pc1->pcNext,pc2=pc2->pcNext
	)
	{
		ps1=IdentName(pc1->pfForm);
		ps2=IdentName(pc2->pfForm);
		if(!StringEqQ(ps1,ps2))
		{
		  if (ArgLt_Hash(pc1->pfForm,pc2->pfForm))
			{
				EXIT("ArgsCmpQWithVars_Hash");
				return -1;
			}
			EXIT("ArgsCmpQWithVars_Hash");
			return 1;
		}
	}
	EXIT("ArgsCmpQWithVars_Hash");
	return 0;							/* lists are equal */
}



int ArgsCmpQWithVars
(
	CELLP pcArgList1,
	CELLP pcArgList2
)
{
	CELLP pc1,pc2;						/* list pointers */
	char *ps1,*ps2;						/* string pointers */

	ENTER("ArgsCmpQWithVars",TRUE);
	for
	(
		pc1=pcArgList1,pc2=pcArgList2;
		pc1&&pc2&&!VarQ(pc1);             /*up to first var*/
		pc1=pc1->pcNext,pc2=pc2->pcNext
	)
	{
		ps1=IdentName(pc1->pfForm);
		ps2=IdentName(pc2->pfForm);
		if(!StringEqQ(ps1,ps2))
		{
			if StringLtQ(ps1,ps2)
			{
				EXIT("ArgsCmpQWithVars");
				return -1;
			}
			EXIT("ArgsCmpQWithVars");
			return 1;
		}
	}
	EXIT("ArgsCmpQWithVars");
	return 0;							/* lists are equal */
}

/* SetBTreeDefaultLt

Description:
	Reset the default key comparator.
Scheme:

(define (set-tree-default< fn)
  (set! *key<* fn))
*/

//static void SetBTreeDefaultLt
//(
//	BOOL (*pf)(CELLP, CELLP)
//)
//{
//	ENTER("SetBTreeDefaultLt",TRUE);
//	pfKeyLtQ=pf;
//	EXIT("SetBTreeDefaultLT");
//}

/* SetBTreeDefaultEq

Description:
	Reset the default key equality.
Scheme:

(define (set-tree-default= fn)
  (set! *key=* fn))
*/

//static void SetBTreeDefaultEq
//(
//	BOOL (*pf)(CELLP, CELLP)
//)
//{
//	ENTER("SetBTreeDefaultEq",TRUE);
//	pfKeyEqQ=pf;
//	EXIT("SetBTreeDefaultEq");
//}

/* c) BTree utilities ------------------------------------------------------- */

/* BTreeSearch

Description:
	Locate a key in a tree.
Scheme:

(define (tree-search item tree)
  (let ((key (if (b-tree? item) (b-tree-key item) item)))
	(letrec ((b-search
		  (lambda (tree)
		(if (or (null? tree)
			(*key=* key (b-tree-key tree)))
			tree
			(if (*key<* key (b-tree-key tree))
			(b-search (b-tree-left tree))
			(b-search (b-tree-right tree)))))))

	  (b-search tree))))
*/

BTREEP BTreeSearch
(
	CELLP pcKey,						/* search key */
	BTREEP pbtTree
)
{
	BTREEP pbt;

	ENTER("BTreeSearch",TRUE);
	StartTimer(tsTimers.adfBTreeLookUp);
	for(pbt=pbtTree;pbt;)
	{
		switch((*pfArgsCmpQ)(pcKey,pbt->pcKey))
		{
			case -1:
				pbt=pbt->pbtLeft;
				break;
			case 1:
				pbt=pbt->pbtRight;
				break;
			default:
				goto end;
		}
//		if((*pfKeyLtQ)(pcKey,pbt->pcKey))
//			pbt=pbt->pbtLeft;
//		else if((*pfKeyEqQ)(pcKey,pbt->pcKey))
//			break;
//		else
//			pbt=pbt->pbtRight;
	}
end:
	StopTimer(tsTimers.adfBTreeLookUp);
	EXIT("BTreeSearch");
	return pbt;
}

/* GetReversedPath

Description:
	Return path from key to root.
	This is a special internal utility.
	It returns a list containing the path to a node, or a path to where
	the node would be if it was in the tree.
Notes:
	The returned path contains pointers to btree nodes!
Scheme:

(define (get-reversed-path key tree)
  (let ((accumulator '()))
	(letrec ((get-path
		  (lambda (tree)
		(cond
		 ((null? tree) accumulator)
		 (else (set! accumulator (cons tree accumulator))
			   (if (*key=* key (b-tree-key tree))
			   accumulator
			   (if (*key<* key (b-tree-key tree))
				   (get-path (b-tree-left tree))
				   (get-path (b-tree-right tree)))))))))
	  (get-path tree))))
*/

static LISTP GetReversedPath
(
	CELLP pcKey,
	BTREEP pbtTree
)
{
	LISTP plAccumulator;				/* list accumulator */
	LISTP pl;							/* list pointer */
	BTREEP pbt;							/* tree pointer */

	ENTER("GetReversedPath",TRUE);
	plAccumulator=NULL;					/* initialize list */

	for(pbt=pbtTree;pbt;)
	{
		pl=NewList(ATOM_BTREEP,"",pbt);	/* insert new node at front of list */
		pl->plNext=plAccumulator;
		plAccumulator=pl;

		if((*pfKeyEqQ)(pcKey,pbt->pcKey))
		{
			EXIT("GetReversedPath");
			return plAccumulator;
		}
		if((*pfKeyLtQ)(pcKey,pbt->pcKey))
			pbt=pbt->pbtLeft;
		else
			pbt=pbt->pbtRight;
	}
	EXIT("GetReversedPath");
	return plAccumulator;
}

// GetReversedPathX

// Description:
//	Locates the specified key in the btree and links up
//	the pbtNext pointers from the key node back to the root.
//	This version is memory efficient and runs faster than GetReversedPath.

/*static BTREEP GetReversedPathX
(
	CELLP pcKey,
	BTREEP pbtTree
)
{
        BTREEP pbt;							// tree pointer 
	BTREEP pbtPrev;						// previous node pointer

	ENTER("GetReversedPath",TRUE);

	pbtPrev=NULL;
	for(pbt=pbtTree;pbt;)
	{
		pbt->pbtNext=pbtPrev;
		pbtPrev=pbt;
		if((*pfKeyEqQ)(pcKey,pbt->pcKey))
		{
			EXIT("GetReversedPath");
			return pbt;
		}
		if((*pfKeyLtQ)(pcKey,pbt->pcKey))
			pbt=pbt->pbtLeft;
		else
			pbt=pbt->pbtRight;
	}
	EXIT("GetReversedPath");
	return pbt;							// if we got here, pbt is NULL
}
*/
/* BTreeMin

Description:
	Get the minimum element of a tree.
Scheme:

(define (tree-min tree)
  (if (null? (b-tree-left tree))
	  tree
	  (tree-min (b-tree-left tree))))
*/

static BTREEP BTreeMin
(
	BTREEP pbtTree
)
{
	BTREEP pbt;							/* tree pointer */

	ENTER("BTreeMin",TRUE);
	for(pbt=pbtTree;pbt->pbtLeft;pbt=pbt->pbtLeft);
	EXIT("BTreeMin");
	return(pbt);
}

/* BTreeMax

Description:
	Get the maximum element of a tree.
Scheme:

(define (tree-max tree)
  (if (null? (b-tree-right tree))
	  tree
	  (tree-max (b-tree-right tree))))
*/

//static BTREEP BTreeMax
//(
//	BTREEP pbtTree
//)
//{
//	BTREEP pbt;							/* tree pointer */
//
//	ENTER("BTreeMax",TRUE);
//	for(pbt=pbtTree;pbt->pbtRight;pbt=pbt->pbtRight);
//	EXIT("BTreeMax");
//	return(pbt);
//}

/* BTreeCount

Description:
	Count up the nodes in a btree.
*/

int BTreeCount
(
	BTREEP pbtTree,						/* btree to count */
	int nCount							/* initialize to 0 */
)
{
	BTREEP pbt;

	/* to reduce recursion, we walk directly down the left branch of the tree */

	ENTER("BTreeCount",TRUE);
	for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
	{
		nCount++;						/* count this node */
		if(pbt->pbtRight)
			nCount+=BTreeCount(pbt->pbtRight,0);	/* count the right subtree */
	}
	EXIT("BTreeCount");
	return nCount;
}

/* BTreeEnumerate

Description:
	Fill an array with pointers to the nodes in a btree.
*/

BTREEP *BTreeEnumerate
(
	BTREEP pbtTree,						/* btree to count */
	BTREEP *ppbtArray					/* array of btree pointers to fill */
)
{
	BTREEP pbt;							/* btree node pointer */
	BTREEP *ppbt;						/* btree array pointer */

	/* to reduce recursion, we walk directly down the left branch of the tree */

	ppbt=ppbtArray;
	ENTER("BTreeEnumerate",TRUE);
	for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
	{
		*ppbt++=pbt;					/* save pointer to this node */
		if(pbt->pbtRight)
			ppbt=BTreeEnumerate(pbt->pbtRight,ppbt);	/* store pointers to right subtree */
	}
	EXIT("BTreeEnumerate");
	return ppbt;
}

/* BTreeSuccessor

Description:
	Find an item's successor, or NULL if it is the max or is not a
	member of the tree.
	(See Cormen Leiserson Rivest for algorithm, and note that final loop
	returns NULL if no ancestor of item is its successor.
Scheme:

(define (tree-successor item tree)
  (let* ((key (if (b-tree? item) (b-tree-key item) item))
		 (rev-path-to-item (get-reversed-path key tree))
		 (item-node (first rev-path-to-item))) ;This should be the item.
	(cond
	 ((not (*key=* key (b-tree-key item-node))) ;Item not in tree!
	  '())
	 ((not (null? (b-tree-right item-node)))
	  (tree-min (b-tree-right item-node)))
	 (else (do ((parents (rest rev-path-to-item))
		(found #f)
		(parent '()))
		   ((or found (null? parents)) (if found parent '()))
		 (set! parent (first parents))
		 (set! parents (rest parents))
		 (if (and parent (eq? item-node (b-tree-right parent)))
		 (set! item-node parent)
		 (set! found #t)))))))  ;The do loop returns '() if it runs out of parent.
*/

static BTREEP BTreeSuccessor
(
	CELLP pcKey,
	BTREEP pbtTree
)
{
	BTREEP pbt;
	BTREEP pbtParent;
	LISTP plRevPathToItem;
	LISTP pl;

	ENTER("BTreeSuccessor",TRUE);
	plRevPathToItem=GetReversedPath(pcKey,pbtTree);
	if(!plRevPathToItem)
	{
		EXIT("BTreeSuccessor");
		return NULL;
	}
	if(!(*pfKeyEqQ)(pcKey,((BTREEP)plRevPathToItem->uValue.pbtTree)->pcKey))
	{
		EXIT("BTreeSuccessor");
		return NULL;					/* key not found */
	}
	pl=plRevPathToItem;					/* point to node */
	pbt=pl->uValue.pbtTree;
	if(pbt->pbtRight)					/* if right tree exists */
	{
		pbt=BTreeMin(pbt->pbtRight);
		EXIT("BTreeSuccessor");
		return pbt;
	}

	/* find the lowest ancestor whose left child is also ancestor */

	pl=pl->plNext;						/* point to parent node */
	if(!pl)
	{
		EXIT("BTreeSuccessor");
		return NULL;
	}
	pbtParent=pl->uValue.pbtTree;
	while(pbt==pbtParent->pbtRight)
	{
		pbt=pbtParent;
		pl=pl->plNext;					/* point to parent node */
		if(!pl)
		{
			EXIT("BTreeSuccessor");
			return NULL;
		}
		pbtParent=pl->uValue.pbtTree;
	}
	EXIT("BTreeSuccessor");
	return pbtParent;
}

// BTreeSuccessorX

// Description:
//	This version uses GetReversedPathX for reduced memory usage, and better speed.

//static BTREEP BTreeSuccessorX
//(
//	CELLP pcKey,
//	BTREEP pbtTree
//)
//{
//	BTREEP pbt;
//	BTREEP pbtParent;
//	BTREEP pbtRevPathToItem;
//
//	ENTER("BTreeSuccessorX",TRUE);
//	pbtRevPathToItem=GetReversedPathX(pcKey,pbtTree);
//	if(!pbtRevPathToItem)
//	{
//		EXIT("BTreeSuccessorX");
//		return NULL;
//	}
//	if(!(*pfKeyEqQ)(pcKey,pbtRevPathToItem->pcKey))	// is this test needed?
//	{
//		EXIT("BTreeSuccessorX");
//		return NULL;					/* key not found */
//	}
//	pbt=pbtRevPathToItem;				/* point to node */
//	if(pbt->pbtRight)					/* if right tree exists */
//	{
//		pbt=BTreeMin(pbt->pbtRight);
//		EXIT("BTreeSuccessorX");
//		return pbt;
//	}
//
//	/* find the lowest ancestor whose left child is also an ancestor */
//
//	pbtParent=pbt->pbtNext;				/* point to parent node */
//	if(!pbtParent)
//	{
//		EXIT("BTreeSuccessorX");
//		return NULL;
//	}
//	while(pbt==pbtParent->pbtRight)
//	{
//		pbt=pbtParent;
//		pbtParent=pbt->pbtNext;			/* point to parent node */
//		if(!pbtParent)
//		{
//			EXIT("BTreeSuccessor");
//			return NULL;
//		}
//	}
//	EXIT("BTreeSuccessorX");
//	return pbtParent;
//}

/*	BTreeInsert and BTreeDelete ------------------------------------------------

	The heart of the matter. Insert and delete make new trees, but
	at the same time reuse structure. Check Corment et al. above, for
	persistent data structures.

	The algorithm is not in their book, but it's not hard with recursion,
	look at the way it is done without persistence.
	The "owner" field of the tree node is used to decide
	when a tree node can be modified and when a copy needs to be made.
*/

/* BTreeInsert

Description:
	Insert a node into a tree.
	Must be called with a key that is not in tree already.
	If called with a node that has subtree under it this will corrupt the
	data structure. So TreeInsert REMOVES these subtrees! This makes
	it more useful, but perhaps more dangerous, as it does this
	silently.
Scheme:

(define (tree-insert owner node tree)
  (let* ((new-key (b-tree-key node)))
	(set-b-tree-right! node '())  ;Ensure only single node is
	(set-b-tree-left! node '())  ;inserted.
	(letrec ((insert
		  (lambda (tree)
		(cond
		 ;No tree return single node tree.
		 ((null? tree) node)
		 ;else put into correct side.
		 ((*key<* new-key (b-tree-key tree))
		  (let ((new-tree
			 (if (eq? (b-tree-owner tree) owner)
			     tree
			     (copy-b-tree tree))))
			(set-b-tree-left! new-tree
					  (insert (b-tree-left tree)))
			(set-b-tree-owner! new-tree owner)
		    new-tree))

		 (else
		  (let ((new-tree
			 (if (eq? (b-tree-owner tree) owner) 
			     tree
			     (copy-b-tree tree))))
		    (set-b-tree-right! new-tree
				       (insert (b-tree-right tree)))
		    (set-b-tree-owner! new-tree owner)
		    new-tree))))))
	  (insert tree))))
*/

BTREEP BTreeInsert
(
	BTREEP *apbtOwner,					/* owning world */
	BTREEP pbtNode,						/* node to insert */
	BTREEP pbtTree						/* tree to act on */
)
{
	pbtNode->pbtRight=NULL;				/* Ensure only single node is inserted */
	pbtNode->pbtLeft=NULL;
	return Insert(apbtOwner,pbtNode,pbtTree);
}

//int nInsertCount;

static BTREEP Insert
(
	BTREEP *apbtOwner,					/* owning world */
	BTREEP pbtNode,						/* node to insert */
	BTREEP pbtTree						/* tree to act on */
)
{
	CELLP pcNewKey;
	BTREEP pbtNewTree;

//	nInsertCount++;

	ENTER("Insert",TRUE);
	if(!pbtTree)
	{
		EXIT("Insert");
		return pbtNode;					/* No tree, return single node tree. */
	}
	pcNewKey=pbtNode->pcKey;
	if((*pfKeyLtQ)(pcNewKey,pbtTree->pcKey))	/* put into correct side. */
	{
		pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?pbtTree:CopyBTree(pbtTree);
		pbtNewTree->pbtLeft=Insert(apbtOwner,pbtNode,pbtTree->pbtLeft);
		pbtNewTree->apbtOwner=apbtOwner;
	}
	else
	{
		pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?pbtTree:CopyBTree(pbtTree);
		pbtNewTree->pbtRight=Insert(apbtOwner,pbtNode,pbtTree->pbtRight);
		pbtNewTree->apbtOwner=apbtOwner;
	}
	EXIT("Insert");
	return pbtNewTree;
}

/* BTreeDelete

Description:
	Delete an item, a key or a tree node, from a tree.
	Delete a key.
	Note difference from insert, it takes a key as an argument not a treeNode.
	Also the key must be in the tree.
Scheme:

(define (tree-delete owner key tree)
  (letrec ((b-delete
		(lambda (key tree)
		  (cond		   ;Need to delete root.
		   ((*key=* key (b-tree-key tree))
		(cond		 ;Has two children
		 ((and (b-tree-right tree) (b-tree-left tree))
		  (let* ((successor-node (tree-successor key tree))
			 (new-tree
			  (if (eq? (b-tree-owner successor-node) owner)
				  successor-node
				  (copy-b-tree successor-node))))
			(set-b-tree-right! new-tree
					   (b-delete (b-tree-key successor-node)
						 (b-tree-right tree)))
			(set-b-tree-owner! new-tree owner)
			(set-b-tree-left! new-tree (b-tree-left tree))
			new-tree))
		 ((not (null? (b-tree-right tree))) (b-tree-right tree))		 ;has only right child, return it.
		 ((not (null? (b-tree-left tree))) (b-tree-left tree))		 ;has only left child return it.
		 (else '())))             ;no children return '()
		   ((*key<* key (b-tree-key tree))
		(let ((new-tree
			   (if (eq? (b-tree-owner tree) owner)
			   tree
			   (copy-b-tree tree))))
		  (set-b-tree-left! new-tree
					(b-delete key (b-tree-left tree)))
		  (set-b-tree-owner! new-tree owner)
		  (set-b-tree-right! new-tree (b-tree-right tree))
		  new-tree))
		   (else
		(let ((new-tree
			   (if (eq? (b-tree-owner tree) owner)
			   tree
			   (copy-b-tree tree))))
		  (set-b-tree-right! new-tree
					 (b-delete key (b-tree-right tree)))
		  (set-b-tree-owner! new-tree owner)
		  (set-b-tree-left! new-tree (b-tree-left tree))
		  new-tree))))))
	(b-delete key tree)))
*/

BTREEP BTreeDelete
(
	BTREEP *apbtOwner,
	CELLP pcKey,
	BTREEP pbtTree
)
{
	BTREEP pbt;
	BTREEP pbtNewTree;
	BTREEP pbtSuccessorNode;

	ENTER("BTreeDelete",TRUE);
	if((*pfKeyEqQ)(pcKey,pbtTree->pcKey))	/* Need to delete root. */
	{
		if(pbtTree->pbtRight&&pbtTree->pbtLeft)	/* Has two children */
		{
			pbtSuccessorNode=BTreeSuccessor(pcKey,pbtTree);
			pbtNewTree=(pbtSuccessorNode->apbtOwner==apbtOwner)?
				pbtSuccessorNode:CopyBTree(pbtSuccessorNode);
			pbtNewTree->pbtRight=BTreeDelete(apbtOwner,pbtSuccessorNode->pcKey,
				pbtTree->pbtRight);
			pbtNewTree->apbtOwner=apbtOwner;
			pbtNewTree->pbtLeft=pbtTree->pbtLeft;
			pbt=pbtNewTree;
		}
		else if(pbtTree->pbtRight)		/* has only right child, return it. */
			pbt=pbtTree->pbtRight;
		else if(pbtTree->pbtLeft)		/* has only left child return it. */
			pbt=pbtTree->pbtLeft;
		else							/* no children return '() */
			pbt=NULL;
	}
	else if((*pfKeyLtQ)(pcKey,pbtTree->pcKey))
	{
		pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?
			pbtTree:CopyBTree(pbtTree);
		pbtNewTree->pbtLeft=BTreeDelete(apbtOwner,pcKey,pbtTree->pbtLeft);
		pbtNewTree->apbtOwner=apbtOwner;
		pbtNewTree->pbtRight=pbtTree->pbtRight;
		pbt=pbtNewTree;
	}
	else
	{
		pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?
			pbtTree:CopyBTree(pbtTree);
		pbtNewTree->pbtRight=BTreeDelete(apbtOwner,pcKey,pbtTree->pbtRight);
		pbtNewTree->apbtOwner=apbtOwner;
		pbtNewTree->pbtLeft=pbtTree->pbtLeft;
		pbt=pbtNewTree;
	}
	EXIT("BTreeDelete");
	return pbt;
}

/* BTreeEqQ

Description:
	Test if two trees are equal.
Scheme:

(define (tree= tree1 tree2)
  (letrec
	  ((tree-check
	(lambda (t1 t2)
	  (cond
	   ((and (null? t1) (null? t2)) #t)
	   ((or (null? t1) (null? t2)) #f)
	   ((and (*key=* (b-tree-key t1) (b-tree-key t2))
		 (eqv? (b-tree-value t1) (b-tree-value t2))
		 (tree-check (tree-successor t1 tree1)
				 (tree-successor t2 tree2))))
	   (else #f)))))
	(cond
	 ((and (null? tree1) (null? tree2)) #t)
	 ((or  (null? tree1) (null? tree2)) #f)
	 (else (tree-check (tree-min tree1) (tree-min tree2))))))
*/

BOOL BTreeEqQ
(
	BTREEP pbtTree1,
	BTREEP pbtTree2
)
{
	BOOL b;

	ENTER("BTreeEqQ",TRUE);
	if(!pbtTree1&&!pbtTree2)
		b=TRUE;
	else if(!pbtTree1||!pbtTree2)
		b=FALSE;
	else if(pbtTree1==pbtTree2)			// new
		b=TRUE;
	else
		b=BTreeCheck(BTreeMin(pbtTree1),BTreeMin(pbtTree2),pbtTree1,pbtTree2);
	EXIT("BTreeEqQ");
	return b;
}

static BOOL BTreeCheck
(
	BTREEP pbt1,
	BTREEP pbt2,
	BTREEP pbtTree1,
	BTREEP pbtTree2
)
{
	BOOL b;

	ENTER("BTreeCheck",TRUE);
	if(!pbt1&&!pbt2)
		b=TRUE;
	else if(!pbt1||!pbt2)
		b=FALSE;
	else if((*pfKeyEqQ)(pbt1->pcKey,pbt2->pcKey)&&
		FormulaListEqQ(pbt1->pcValue,pbt2->pcValue)&&
		BTreeCheck(BTreeSuccessor(pbt1->pcKey,pbtTree1),
			BTreeSuccessor(pbt2->pcKey,pbtTree2),pbtTree1,pbtTree2))
		b=TRUE;
	else
		b=FALSE;
	EXIT("BTreeCheck");
	return b;
}

/* Other useful functions ------------------------------------------------------

Also while we are at it, make it take a filter that it can apply to
elements. And a mapping function that can transform the raw nodes
into something more useful.
Use an internal recursive function to avoid having to invoke a
keyword parameter function recursively.

In particular.
:key	--- apply to node prior for testing.
:test	--- only add to list if passes test (after applying key!)
:transform apply to nodes that past the test.

Returned value is list of transformed nodes that passed the test.
*/

/* BTreeToList -----------------------------------------------------------------

Description:
	Return a list of the tree nodes that satisfy ":test transformed by :key".
	Convert tree to a list of Tree nodes.
	Then you can map across this list getting what you want.
	Since we are constructing a list it is easiest to visit the tree nodes in
	reverse order.
Scheme:

(define (tree-to-list tree . keyword-args)
	(let ((accumulator '())
			(test '())
			(key '())
			(transform '()))
		(do ()
			((null? keyword-args))
			(case (first keyword-args)
				(:test
					(set! test (cadr keyword-args)))
				(:key
					(set! key (cadr keyword-args)))
				(:transform
					(set! transform (cadr keyword-args)))
				(else
					(error "Illegal keyword: ~A" (first keyword-args))))
			(set! keyword-args (cddr keyword-args)))
		(letrec
			((tree-walk
					(lambda (tree)
						(cond
							((null? tree) accumulator)
							(else
								(tree-walk (b-tree-right tree))
								(if (or (and (not (null? test))
											(not (null? key))
											(test (key tree)))
										(and (not (null? test))
											(null? key) (test tree))
										(and (null? test) (null? key)))
									(set! accumulator
										(cons (if transform (transform tree) tree)
											accumulator)))
								(tree-walk (b-tree-left tree)))))))
			(tree-walk tree)
			accumulator)))
*/

LISTP BTreeToList
(
	BTREEP pbtTree,						/* tree to enumerate */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* key argument for test (optional) */
	LISTP (*pfTransform)(CELLP, BTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
)
{
	LISTP pl;
	LISTP plAccumulator;

	ENTER("BTreeToList",TRUE);
	plAccumulator=NULL;
	pl=BTreeWalk(plAccumulator,pbtTree,pfTest,pcKey,pfTransform,pcArg);
	EXIT("BTreeToList");
	return pl;
}

static LISTP BTreeWalk
(
	LISTP plAccumulator,				/* accumulated list */
	BTREEP pbtTree,						/* current tree location */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* test argument (optional) */
	LISTP (*pfTransform)(CELLP, BTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
)
{
	LISTP pl;

	ENTER("BTreeWalk",TRUE);
	if(!pbtTree)
	{
		EXIT("BTreeWalk");
		return plAccumulator;
	}
	plAccumulator=BTreeWalk(plAccumulator,pbtTree->pbtRight,
		pfTest,pcKey,pfTransform,pcArg);
	if((pfTest&&(*pfTest)(pcKey,pbtTree))||(!pfTest&&!pcKey))
	{
		if(pfTransform)
			plAccumulator=Cons((*pfTransform)(pcArg,pbtTree),plAccumulator);
		else
			plAccumulator=Cons(NewList(ATOM_BTREEP,"",pbtTree),plAccumulator);
	}
	pl=BTreeWalk(plAccumulator,pbtTree->pbtLeft,
		pfTest,pcKey,pfTransform,pcArg);
	EXIT("BTreeWalk");
	return pl;
}

/* BTreeToFormulaList ----------------------------------------------------------

Description:
	Return a formula list of the tree nodes that satisfy ":test transformed by :key".
	Convert tree to a list of Tree nodes.
	Since we are constructing a list it is easiest to visit the tree nodes in
	reverse order.
*/

CELLP BTreeToFormulaList
(
	BTREEP pbtTree,						/* tree to enumerate */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* key argument for test (optional) */
	CELLP (*pfTransform)(CELLP, BTREEP),	/* node transform */
	CELLP pcArg							/* argument to transform */
)
{
	CELLP pc;
	CELLP pcAccumulator;

	ENTER("BTreeToFormulaList",TRUE);
	pcAccumulator=NULL;
	pc=NewBTreeWalk(pcAccumulator,pbtTree,pfTest,pcKey,pfTransform,pcArg);
	EXIT("BTreeToFormulaList");
	return pc;
}

static CELLP NewBTreeWalk
(
	CELLP pcAccumulator,				/* accumulated list */
	BTREEP pbtTree,						/* current tree location */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* test argument (optional) */
	CELLP (*pfTransform)(CELLP, BTREEP),	/* node transform */
	CELLP pcArg							/* argument to transform */
)
{
	CELLP pc;

	ENTER("NewBTreeWalk",TRUE);
	if(!pbtTree)
	{
		EXIT("NewBTreeWalk");
		return pcAccumulator;
	}
	pcAccumulator=NewBTreeWalk(pcAccumulator,pbtTree->pbtRight,
		pfTest,pcKey,pfTransform,pcArg);
	if((pfTest&&(*pfTest)(pcKey,pbtTree))||(!pfTest&&!pcKey))
	{
		pc=(*pfTransform)(pcArg,pbtTree);
		pc->pcNext=pcAccumulator;
		pcAccumulator=pc;
	}
	pc=NewBTreeWalk(pcAccumulator,pbtTree->pbtLeft,
		pfTest,pcKey,pfTransform,pcArg);
	EXIT("NewBTreeWalk");
	return pc;
}

/* BTreeGenerator --------------------------------------------------------------

Description:
	Enumerate the keys of a btree one node at a time.
Notes:
	This isn't a true generator, since when we initialize, we don't enumerate a node!
	All other generators enumerate a value every time they're called.
	We don't here, because we're only called from GenerateDescPredicate,
	and it likes it that way!
Scheme:

(define (make-tree-generator tree transform)
  (let ((current-node '())
	(next-node (tree-min tree)))
	(lambda ()
	  (cond
	   (next-node
	(set! current-node next-node)
	(set! next-node (tree-successor current-node tree))
	(if transform
		(transform current-node)
		(b-tree-key current-node)))
	   (else '())))))
*/

/* BTreeGenerator

Description:
	Enumerate an entire btree.
	The tree is traversed "in-order".
Note:
	This version uses a stack and is fully reentrant.
	We don't need to store the stack base pointer in the context,
	but we need a flag anyways, and the base pointer is handy
	for debugging.
*/

CELLP BTreeGenerator
(
	BTREEP pbtTree,
	void **ppvContext
)
{
	BTREEP pbt;
	BTREEP pbtCurrent;

	/* handle initialization.*/
	
	ENTER("BTreeGenerator",TRUE);
	if(!*ppvContext)
	{
		if(!pbtTree)
		{
//			fprintf(stderr,"Empty btree\n");
			EXIT("BTreeGenerator");
			return NULL;				/* nothing to do */
		}
		
		*ppvContext=(void *)GetBTreeSP();	// save stack base index
		
		/* Traverse the tree to the left and store each node's 
		address on the stack. */

		CheckBTreeStack();
		BTreePush(NULL);
		for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
		{
			CheckBTreeStack();
			BTreePush(pbt);
		}
//		ShowBTreeStack("New",(int)*ppvContext);
		EXIT("BTreeGenerator");
		return NULL;					/* ignored return value */
	}
	
	// are we done yet?

	pbtCurrent=BTreePop();				// next node to enumerate
	if(!pbtCurrent)
	{
#ifdef _DEBUG
		if((int)*ppvContext!=nBTreeSP)
			fprintf(stderr,"\nStack Pointer doesn't match... is %d should be %d\n",
				nBTreeSP,(int)*ppvContext);
//		ShowBTreeStack("Done",(int)*ppvContext);
#endif // _DEBUG
		EXIT("BTreeGenerator");
		return NULL;					// we're done
	}

	/* find successor, saving any new context on stack */
	
	for(pbt=pbtCurrent->pbtRight;pbt;pbt=pbt->pbtLeft)
	{
		CheckBTreeStack();
		BTreePush(pbt);
	}

//	ShowBTreeStack("Key",(int)*ppvContext);
	EXIT("BTreeGenerator");
	return pbtCurrent->pcKey;
}

/* BTreeGeneratorO

Description:
	Enumerate an entire btree.
	The tree is traversed "in-order".
Note:
	This version uses the old tree-min and tree-successor algorithm.
	It is reentrant and very slow.
*/

//CELLP BTreeGeneratorO
//(
//	BTREEP pbtTree,
//	void **ppvContext,
//	BINDINGP pbBindings
//)
//{
//	BTREEP pbtCurrent;
//	BTOCONTEXTP pbtcContext;
//
//	/* handle initialization */
//
//	ENTER("BTreeGeneratorO",TRUE);
//	if(!*ppvContext)
//	{
//		if(!pbtTree)
//		{
//			EXIT("BTreeGenerator)");
//			return NULL;
//		}
//		pbtcContext=(BTOCONTEXTP)MemAlloc(sizeof(BTOCONTEXT));
//		*ppvContext=(void *)pbtcContext;
//		pbBindings->pvContext=(void *)pbtcContext;
//		pbtcContext->pbtRoot=pbtTree;
//
//		/* find the left most node */
//
//		pbtcContext->pbtNext=BTreeMin(pbtTree);
//		EXIT("BTreeGeneratorO");
//		return NULL;					/* ignored return value */
//	}
//
//	/* get the next key */
//
//	pbtcContext=(BTOCONTEXTP)*ppvContext;
//	pbtCurrent=pbtcContext->pbtNext;
//	if(!pbtCurrent)
//	{
//		EXIT("BTreeGeneratorO");
//		return NULL;
//	}
//	pbtcContext->pbtNext=BTreeSuccessorX(pbtCurrent->pcKey,pbtcContext->pbtRoot);
//	EXIT("BTreeGeneratorO");
//	return pbtCurrent->pcKey;
//}

/* BTreeGeneratorN

Description:
	Enumerate an entire btree.
	The tree is traversed "in-order".
Note:
	This version uses pointers on btrees, which are shared between worlds.
	It is non-reentrant, but fast.
*/

//CELLP BTreeGeneratorN
//(
//	BTREEP pbtTree,
//	void **ppvContext
//)
//{
//	BTREEP pbt;
//	BTREEP pbtCurrent;
//	BTREEP pbtContext;
//
//	/* handle initialization */
//
//	ENTER("BTreeGeneratorN",TRUE);
//	if(!*ppvContext)
//	{
//		if(!pbtTree)
//		{
//			EXIT("BTreeGeneratorN");
//			return NULL;
//		}
//
//		/* find the left most node, and thread back pointers */
//
//		pbtContext=NULL;
//		for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
//		{
//			pbt->pbtNext=pbtContext;
//			pbtContext=pbt;
//		}
//		*ppvContext=(void *)pbtContext;
//		EXIT("BTreeGeneratorN");
//		return NULL;					/* ignored return value */
//	}
//
//	/* get the next key */
//
//	pbtContext=(BTREEP)*ppvContext;
//	if(!pbtContext)
//	{
//		EXIT("BTreeGeneratorN");
//		return NULL;					/* show we're done */
//	}
//	pbtCurrent=pbtContext;				/* pop */
//	pbtContext=pbtContext->pbtNext;		/* assume no right child */
//
//	/* set up context to point to next key, and thread back pointers */
//	
//	if(pbtCurrent->pbtRight)
//	{
//		pbt=pbtCurrent->pbtRight;
//		pbt->pbtNext=pbtContext;
//		pbtContext=pbt;
//
//		for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft)
//		{
//			pbt->pbtNext=pbtContext;
//			pbtContext=pbt;
//		}
//	}
//	*ppvContext=(void *)pbtContext;
//	EXIT("BTreeGeneratorN");
//	return pbtCurrent->pcKey;
//}

/* BTreeGeneratorH

Description:
	Enumerate an entire btree.
	The tree is traversed "in-order".
Note:
	This version allocates memory from the heap to store its traversal context.
	It is reentrant, but expensive in terms of both cpu and memory.
*/

//CELLP BTreeGeneratorH
//(
//	BTREEP pbtTree,
//	void **ppvContext
//)
//{
//	BTREEP pbt;
//	BTREEP pbtCurrent;
//	BTCONTEXTP pbtcContext;
//	BTCONTEXTP pbtcNode;
//	BTCONTEXTP pbtc;
//
//	/* handle initialization */
//
//	ENTER("BTreeGeneratorH",TRUE);
//	if(!*ppvContext)
//	{
//		if(!pbtTree)
//		{
//			EXIT("BTreeGeneratorH");
//			return NULL;
//		}
//		pbtcContext=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//		*ppvContext=(void *)pbtcContext;
//		pbBindings->pvContext=(void *)pbtcContext;
//		pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//		pbtcNode->pbtcNext=NULL;
//		pbtcContext->pbtcNext=pbtcNode;
//		pbtcNode->pbtTree=pbtTree;
//
//		for(pbt=pbtTree;pbt->pbtLeft;pbt=pbt->pbtLeft)
//		{
//			pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//			pbtcNode->pbtcNext=pbtcContext->pbtcNext;
//			pbtcContext->pbtcNext=pbtcNode;
//			pbtcNode->pbtTree=pbt->pbtLeft;
//		}
//		EXIT("BTreeGeneratorH");
//		return NULL;					/* ignored return value */
//	}
//
//	/* return the next key */
//
//	pbtcContext=(BTCONTEXTP)*ppvContext;
//	pbtc=pbtcContext->pbtcNext;
//	if(!pbtc)
//	{
//		*ppvContext=NULL;
//		EXIT("BTreeGeneratorH");
//		return NULL;					/* show we're done */
//	}
//	pbtCurrent=pbtc->pbtTree;			/* pop */
//	pbtcContext->pbtcNext=pbtc->pbtcNext;
//
//	if(pbtCurrent->pbtRight)
//	{
//		pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//		pbtcNode->pbtcNext=pbtcContext->pbtcNext;
//		pbtcContext->pbtcNext=pbtcNode;
//		pbtcNode->pbtTree=pbtCurrent->pbtRight;
//
//		for(pbt=pbtCurrent->pbtRight;pbt->pbtLeft;pbt=pbt->pbtLeft)
//		{
//			pbtcNode=(BTCONTEXTP)MemAlloc(sizeof(BTCONTEXT));
//			pbtcNode->pbtcNext=pbtcContext->pbtcNext;
//			pbtcContext->pbtcNext=pbtcNode;
//			pbtcNode->pbtTree=pbt->pbtLeft;
//		}
//	}
//	EXIT("BTreeGeneratorH");
//	return pbtCurrent->pcKey;
//}

// BTreeGeneratorWithBoundVars

// Description:
//	Special version for the case when the first of the
//	generator's arguments are bound.
// Note:
//	This version uses a stack and is fully reentrant.

CELLP BTreeGeneratorWithBoundVars
(
	BTREEP pbtTree,
	void **ppvContext,
	CELLP pcArgs
)
{
	BTREEP pbt;
	BTREEP pbtCurrent;

	/* handle initialization.*/
	
	ENTER("BTreeGeneratorWithBoundVars",TRUE);
	if(!*ppvContext)
	{
		if(!pbtTree)
		{
//			fprintf(stderr,"Empty btree+\n");
			EXIT("BTreeGeneratorWithBoundVars");
			return NULL;
		}
		*ppvContext=(void *)GetBTreeSP();	// save stack base index

		/* find the root of the subtree containing potentially matching tuples */
		
		for(pbt=pbtTree;pbt;)
		{
			switch((*pfArgsCmpQWithVars)(pcArgs,pbt->pcKey))
			{
				case -1: 
					pbt=pbt->pbtLeft;
					break;
				case 1:
					pbt=pbt->pbtRight;
					break;
				default:
					goto end;
			}
		}
end:

		/* Now we can walk the subtree rooted by pbt pretending that
		all nodes with non-zero argscomqwithvars don't exist.
		First put a NULL termination marker on the stack, and move
		to the least most matching nodes leaving a trail on the stack*/
		
		CheckBTreeStack();
		BTreePush(NULL);
		while(pbt)
		{
			switch((*pfArgsCmpQWithVars)(pcArgs,pbt->pcKey)) 
			{
				case 0:					// Put on stack and try to find a smaller node
					CheckBTreeStack();
					BTreePush(pbt);
					pbt=pbt->pbtLeft;
					break;
				case -1:				// node is greater than any possible match. Move left
					ErrorMessage("Bug in BTreeGenWithBoundVars. This shouldn't happen.\n");
	
					break;
				case 1:					// node is greater than any possible match. Move left.
					pbt=pbt->pbtRight;
			}
		}

		/*Now top of stack should be least matching node and a "path"
		to the node (i.e., a path with the non-matching nodes delted
		nodes to be considered are on the stack */
		
//		ShowBTreeStack("New+",(int)*ppvContext);
		EXIT("BTreeGeneratorWithBoundVars");
		return NULL;					/* ignored return value */
	}
	
	/* This is the entry point for getting the next tuple. This
	next tuple is already stored in the context. Check first to see
	if it is NULL which would indicate we are finished. */

	pbtCurrent=BTreePop();				// next node to enumerate
	if(!pbtCurrent)
	{
#ifdef _DEBUG
		if((int)*ppvContext!=nBTreeSP)
			fprintf(stderr,"\nStack Pointer doesn't match... is %d should be %d\n",
				nBTreeSP,(int)*ppvContext);
#endif //_DEBUG
//		ShowBTreeStack("Done+",(int)*ppvContext);
		EXIT("BTreeGeneratorWithBoundVars");
		return NULL;					/* show we're done */
	}
	
	/* now update the stack to put on it a path the matching successor
	of pbtCurrent if one exists. We have already visited all of
	pbtCurrent's left subtree. Note that the right subtree might have
	some matching left most nodes even if it and its left child are not
	possible matches. Only add the children to the stack if they match
	---we need never explore the right subtrees of non-matching nodes
	as all non-matching nodes must be greater than (and thus all nodes
	in their rightsubtrees must also be greater than).*/
	
	pbt=pbtCurrent->pbtRight;
	if(pbt)
	{
		if((*pfArgsCmpQWithVars)(pcArgs,pbt->pcKey)==0) 
		{
			CheckBTreeStack();
			BTreePush(pbt);
			
			/*special case if the top matches all of its left children must also
			match, so we can avoid comparing them*/
			
			for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft)
			{
				CheckBTreeStack();
				BTreePush(pbt);
			}
		}
		else
		{
			/* even the left children might not match, so we must compare them*/
			
			for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft) 
			{
				if((*pfArgsCmpQWithVars)(pcArgs,pbt->pcKey)==0)
				{
					CheckBTreeStack();
					BTreePush(pbt);
				}
			}
		}
	}

//	ShowBTreeStack("Key+",(int)*ppvContext);
	EXIT("BTreeGeneratorWithBoundVars");
	return pbtCurrent->pcKey;
}
 
//CELLP BTreeGeneratorWithBoundVars
//(
// BTREEP pbtTree,
// void **ppvContext,
// CELLP pcArgs
// )
//{
//  BTREEP pbt;
//  BTREEP pbtCurrent;
//  /* handle initialization.*/
//  ENTER("BTreeGeneratorWithBoundVars",TRUE);
//  /*CommandPrintf(stdout,"Entered BTGeneratorWithBoundVars.\n");
//    PrintFormulaList(stdout, pcArgs);*/
//  if(!*ppvContext)
//    {
//      if(!pbtTree)
//	{
//	  EXIT("BTreeGeneratorWithBoundVars");
//	  return NULL;
//	}
//      /* find the root of the subtree containing potentially matching
//	 tuples */
//      for(pbt=pbtTree;pbt;)
//	{
//	  switch(ArgsCmpQWithVars(pcArgs,pbt->pcKey))
//	    {
//	    case -1: 
//	      pbt=pbt->pbtLeft;
//	      break;
//	    case 1:
//	      pbt=pbt->pbtRight;
//	      break;
//	    default:
//	      goto end;
//	    }
//	}
//    end:
//      /* Now we can walk the subtree rooted by pbt pretending that
//	 all nodes with non-zero argscomqwithvars don't exist.
//	 First put a NULL termination marker on the stack, and move
//	 to the least most matching nodes leaving a trail on the stack*/
//      walkstack[topws++] = NULL;
//      while(pbt) {
//	switch(ArgsCmpQWithVars(pcArgs,pbt->pcKey)) 
//	  {
//	  case 0:  //Put on stack and try to find a smaller node
//	    walkstack[topws++]=pbt;
//	    pbt=pbt->pbtLeft;
//	    break;
//	  case -1: //node is greater than any possible match. Move left
//	    ErrorMessage("Bug in BTreeGenWithBoundVars. This shouldn't happen.\n");
//	    break;
//	  case 1: //node is greater than any possible match. Move left.
//	    pbt=pbt->pbtRight;
//	  }
//      }
//      /*Now top of stack should be least matching node and a "path"
//	to the node (i.e., a path with the non-matching nodes delted
//	nodes to be considered are on the stack */
//
//      /*Pop the top of the stack into the returned context*/
//      *ppvContext=(void *)walkstack[--topws];
//      EXIT("BTreeGeneratorWithBoundVars");
//      return NULL;					/* ignored return value */
//    }
//
//  /* This is the entry point for getting the next tuple. This
//     next tuple is already stored in the context. Check first to see
//     if it is NULL which would indicate we are finished. */
//  pbtCurrent=(BTREEP)*ppvContext;
//  if(!pbtCurrent) {
//    EXIT("BTreeGeneratorWithBoundVars");
//    return NULL;					/* show we're done */
//  }
//  /* now update the stack to put on it a path the matching successor
//     of pbtCurrent if one exists. We have already visited all of
//     pbtCurrent's left subtree. Note that the right subtree might have
//     some matching left most nodes even if it and its left child are not
//     possible matches. Only add the children to the stack if they match
//     ---we need never explore the right subtrees of non-matching nodes
//     as all non-matching nodes must be greater than (and thus all nodes
//     in their rightsubtrees must also be greater than).*/
//
//  if(pbt=pbtCurrent->pbtRight)
//    {
//      if(ArgsCmpQWithVars(pcArgs,pbt->pcKey)==0) {
//	walkstack[topws++]=pbt;
//	/*special case if the top matches all of its left children must also
//	  match, so we can avoid comparing them*/
//	for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft) {
//	  walkstack[topws++]=pbt;
//	}
//      }
//      else
//	/* even the left children might not match, so we must compare them*/
//	for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft) 
//	  if(ArgsCmpQWithVars(pcArgs,pbt->pcKey)==0) {
//		walkstack[topws++]=pbt;
//	  }
//    }
//  /*Finally pop the top of the stack into the context*/
//  *ppvContext=(void *)walkstack[--topws];
//  EXIT("BTreeGeneratorWithBoundVars");
//  return pbtCurrent->pcKey;
//}

/* MarkBTree

Description:
	Mark an entire btree (for garbage collection)
*/

void MarkBTree
(
	BTREEP pbtTree
)
{
	BTREEP pbt;

	ENTER("MarkBTree",TRUE);

	for(pbt=pbtTree;pbt;pbt=pbt->pbtRight)
	{
		if(ZoneMarkedQ(pbt))
		{
			EXIT("MarkBTree");
			return;
		}
		ZoneMark(pbt);
		MarkFormulaList(pbt->pcKey);
		if(pbt->pcValue)
			MarkFormula(pbt->pcValue);
		MarkBTree(pbt->pbtLeft);
	}
	EXIT("MarkBTree");
}

/* BTreeSizeOf

Description:
	Calculate the incremental size of a btree.
	Only unmarked memory is included in the sum.
*/

void BTreeSizeOf
(
	BTREEP pbtTree,
	int *pnSize
)
{
	BTREEP pbt;

	ENTER("BTreeSizeOf",TRUE);

	for(pbt=pbtTree;pbt;pbt=pbt->pbtRight)
	{
		if(!ZoneMarkedQ(pbt))
			*pnSize+=ZoneSizeOf(pbt);
		ZoneMark(pbt);
		FormulaListSizeOf(pbt->pcKey,pnSize);
		MarkFormulaList(pbt->pcKey);
		if(pbt->pcValue)
		{
			FormulaSizeOf(pbt->pcValue,pnSize);
			MarkFormula(pbt->pcValue);
		}
		BTreeSizeOf(pbt->pbtLeft,pnSize);
	}
	EXIT("BTreeSizeOf");
}

/* BTreeBalancedQ

Description:
	Count all the nodes in a btree and determine its maximal depth.
*/

//void BTreeBalancedQ
//(
//	BTREEP pbtTree,
//	int *pnCount,						// number of nodes
//	int *pnDepth						// depth
//)
//{
//	BTREEP pbt;
//
//	ENTER("BTreeBalancedQ",TRUE);
//
//	(*pnDepth)++;
//	for(pbt=pbtTree;pbt;pbt=pbt->pbtRight)
//	{
//		(*pnCount)++;
//		BTreeBalancedQ(pbt->pbtLeft,pnCount,pnDepth);
//	}
//	EXIT("BTreeBalancedQ");
//}

/* BTreeBalance

Description:
	Balance a btree.  First count the number of nodes in the btree.
	Then create an array of node pointers, and fill it in using an
	inorder walk of the tree.  Finally, reconstruct the tree.
Note:
	This routine can only be called before planning commences, since 
	btrees are shared between worlds.
*/

BTREEP BTreeBalance
(
	BTREEP pbtRoot						/* root of btree */
)
{
	int n;								/* number of nodes in the btree */
	BTREEP *ppbtNodes;					/* pointer to node pointer array */
	BTREEP *ppbt;

	if(!pbtRoot)
		return NULL;

	/* allocate an array of pointers */

	n=BTreeCount(pbtRoot,0);
	ppbtNodes=(BTREEP *)MemAlloc(n*sizeof(BTREEP));
	
	/* fill in the array */
	
	ppbt=ppbtNodes;
	FillNodeArray(&ppbt,pbtRoot);

	/* reconstruct the btree */

	return BTreeBuild(ppbtNodes,0,n-1);
}

/* FillNodeArray

Description
	Fill in the node array for BTreeBalance.
	This is an in-order walk.
*/

static void FillNodeArray
(
	BTREEP **pppbt,						/* address of pointer to node pointer array */
	BTREEP pbt							/* pointer to current node */
)
{
	if(pbt)
	{
		FillNodeArray(pppbt,pbt->pbtLeft);
		**pppbt=pbt;
		(*pppbt)++;
		FillNodeArray(pppbt,pbt->pbtRight);
	}
}

/* BTreeBuild

Description:
	Build a btree out of nodes i to j.
*/

static BTREEP BTreeBuild
(
	BTREEP *ppbtNodes,					/* sorted node array */
	int i,								/* lower limit */
	int j								/* upper limit */
)
{
	int nMid;
	BTREEP pbtRoot;

	if(j<i)
		return NULL;
	nMid=(j+i+1)/2;
	pbtRoot=ppbtNodes[nMid];
	pbtRoot->pbtLeft=BTreeBuild(ppbtNodes,i,nMid-1);
	pbtRoot->pbtRight=BTreeBuild(ppbtNodes,nMid+1,j);
	return pbtRoot;
}

/* BTreeDepth

Description:
	Count up the depth of a btree.

*/

static int BTreeDepth
(
	BTREEP pbtTree						/* btree to measure */
)
{
	int nLeft,nRight;

	if(!pbtTree)
		return 0;
	nLeft=BTreeDepth(pbtTree->pbtLeft);
	nRight=BTreeDepth(pbtTree->pbtRight);
	return MAX(nLeft,nRight)+1;
}

/* ZoneCopyBTree

Description:
	Copy a btree from the scratch zone to permanent zone.
*/

//void ZoneCopyBTree
//(
//	BTREEP pbtTree
//)
//{
//	BTREEP pbt;
//
//	ENTER("ZoneCopyBTree",TRUE);
//
//	for(pbt=pbtTree;pbt;pbt=pbt->pbtRight)
//	{
//		ZoneCopy(pbt);
//		ZoneCopyFormulaList(pbt->pcKey);
//		if(pbt->pcValue)
//			ZoneCopyFormula(pbt->pcValue);
//		ZoneCopyBTree(pbt->pbtLeft);
//	}
//	EXIT("ZoneCopyBTree");
//}

/* ZoneRelocBTree

Description:
	Relocate a btree from the scratch zone to permanent zone.
*/

//void ZoneRelocBTree
//(
//	BTREEP pbtTree
//)
//{
//	BTREEP pbt,pbt1;
//
//	ENTER("ZoneRelocBTree",TRUE);
//
//	for(pbt=pbtTree;pbt;pbt=pbt->pbtRight)
//	{
//		ZoneReloc(&pbt);
//		ZoneReloc(&pbt->pcKey);
//		ZoneRelocFormulaList(pbt->pcKey);
//		if(pbt->pcValue)
//		{
//			ZoneReloc(&pbt->pcValue);
//			ZoneRelocFormula(pbt->pcValue);
//		}
//		ZoneReloc(&pbt->pbtLeft);
//		ZoneRelocBTree(pbt->pbtLeft);
//		ZoneReloc(&pbt->pbtRight);
//	}
//	EXIT("ZoneRelocBTree");
//}

// BTree Stack Support ---------------------------------------------------------

// InitBTreeStack

// Description:
//	Allocate a default quantity of memory to the btree stack.
//	The first entry of the stack is reserved, and filled with a sanity marker.

void InitBTreeStack(void)
{
	nBTreeMaxSP=MIN_BTREE_STACK-1;
	ppbtStack=(BTREEP *)MemAlloc((nBTreeMaxSP+1)*sizeof(BTREEP));
	nBTreeSP=0;
	ppbtStack[nBTreeSP++]=(BTREEP)BTREE_STACK_SIGNATURE;	// stack 
}

// ResizeBTreeStack

// Description:
//	Double the size of the btree stack.
// Notes:
//	We always allocate our memory in the permanent zone.

void ResizeBTreeStack(void)
{
	BTREEP *ppbt;
	ZONEP pz;

	nBTreeMaxSP++;
	pz=pzCurrent;
	SetZone(&zPermanent);
	ppbt=(BTREEP *)MemAlloc(2*nBTreeMaxSP*sizeof(BTREEP));
	SetZone(pz);
	memcpy(ppbt,ppbtStack,nBTreeMaxSP*sizeof(BTREEP));
	ppbtStack=ppbt;
	nBTreeMaxSP=2*nBTreeMaxSP-1;
#ifdef _DEBUG
	fprintf(stderr,"Resizing btree stack to %d entries.\n",nBTreeMaxSP+1);
	if(*ppbtStack!=(BTREEP)BTREE_STACK_SIGNATURE)
		fprintf(stderr,"Btree stack signature overwritten\n");
#endif // _DEBUG
}

/* MarkBTreeStack

Description:
	Mark the btree stack (for garbage collection)
*/

void MarkBTreeStack(void)
{
	ENTER("MarkBTreeStack",TRUE);
	ZoneMark(ppbtStack);
	EXIT("MarkBTreeStack");
}

// ShowBTreeStack

// Description:
//	Display the contents of the btree stack on stderr.

#ifdef _DEBUG
//static void ShowBTreeStack
//(
//	char *ps,							// (one word) description
//	int nBaseSP							// base btree stack index
//)
//{
//	int i;
//			
//	fprintf(stderr,"%08X %s Base %d SP %d: ",pbtc,ps,nBaseSP,nBTreeSP);
//	for(i=nBTreeSP-1;i>=1;i--)
//	{
//		if(ppbtStack[i])
//			fprintf(stderr,"%s, ",ppbtStack[i]->pcKey->pfForm->psName);
//		else
//			fprintf(stderr,"0. ");
//	}
//	fprintf(stderr,"\n");
//}
#endif // _DEBUG

// ResetBTreeStack

// Description:
//	Restore the BTreeStack pointer.

void ResetBTreeStack
(
	PREDCONTEXTP pbcContext 
)
{
#ifdef _DEBUG
	int nBaseSP;

	nBaseSP=(int)(pbcContext->pvContext);

	if(nBTreeSP<nBaseSP)
		fprintf(stderr,"\nStack Pointer too small... is %d should be %d\n",
			nBTreeSP,nBaseSP);
	if(nBTreeSP<1)
		fprintf(stderr,"\nStack Pointer non-positive... is %d should be %d\n",
			nBTreeSP,nBaseSP);
	if(nBTreeSP>=nBTreeMaxSP)
		fprintf(stderr,"\nStack Pointer too large... is %d should be %d\n",
			nBTreeSP,nBaseSP);
	if(ppbtStack[nBaseSP])
		fprintf(stderr,"\nBase Pointer points to non-zero value... pointer is %d value is %d\n",
			nBTreeSP,ppbtStack[nBaseSP]);
	nBTreeSP=nBaseSP;
//	ShowBTreeStack("Early",nBaseSP);
#else // _DEBUG
	nBTreeSP=(int)(pbcContext->pvContext);
#endif // _DEBUG
}
