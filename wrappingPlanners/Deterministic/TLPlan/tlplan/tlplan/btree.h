/* btree.h -- binary tree support */

#ifndef __BTREE_H
#define __BTREE_H

/* global definitions */

#define CheckBTreeStack()	if(nBTreeSP>=nBTreeMaxSP)ResizeBTreeStack()
#define GetBTreeSP()		(nBTreeSP)
#define SetBTreeSP(n)		(nBTreeSP=n)
#define BTreePush(pbt)		(ppbtStack[nBTreeSP++]=(pbt))
#define BTreePop()			(ppbtStack[--nBTreeSP])

/* global data */

extern BTREEP *ppbtStack;				// btree stack
extern int nBTreeSP;					// btree stack pointer
extern int nBTreeMaxSP;					// btree stack limit

/* global function prototypes */

BTREEP MakeBTree
(
	CELLP pcKey,
	CELLP pcValue,
	BTREEP pbtLeft,
	BTREEP pbtRight,
	BTREEP *pbtOwner
);
BTREEP BTreeInsert
(
	BTREEP *apbtOwner,					/* owner of ? */
	BTREEP pbtNode,						/* node to insert */
	BTREEP pbtTree						/* tree to act on */
);
BTREEP BTreeDelete
(
	BTREEP *apbtOwner,
	CELLP pcKey,
	BTREEP pbtTree
);
BTREEP BTreeSearch
(
	CELLP pcKey,						/* search key */
	BTREEP pbtTree
);
LISTP BTreeToList
(
	BTREEP pbtTree,						/* tree to enumerate */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* key argument for test (optional) */
	LISTP (*pfTransform)(CELLP, BTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
);
CELLP BTreeToFormulaList
(
	BTREEP pbtTree,						/* tree to enumerate */
	BOOL (*pfTest)(CELLP,BTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* key argument for test (optional) */
	CELLP (*pfTransform)(CELLP, BTREEP),	/* node transform */
	CELLP pcArg							/* argument to transform */
);
BOOL BTreeEqQ
(
	BTREEP pbtTree1,
	BTREEP pbtTree2
);
int BTreeCount
(
	BTREEP pbtTree,						/* btree to count */
	int nCount							/* initialize to 0 */
);
BTREEP *BTreeEnumerate
(
	BTREEP pbtTree,						/* btree to count */
	BTREEP *ppbtArray					/* array of btree pointers to fill */
);
CELLP BTreeGenerator
(
	BTREEP pbtTree,
	void **pvContext
);
CELLP BTreeGeneratorWithBoundVars		/*FAHIEM DEC 2000*/
(
	BTREEP pbtTree,
	void **pvContext,
	CELLP pcArgs
);
void MarkBTree
(
	BTREEP pbtTree
);
BTREEP BTreeBalance
(
	BTREEP pbtRoot						/* root of btree */
);
void BTreeSizeOf
(
	BTREEP pbtTree,
	int *pnSize
);
void InitBTreeStack(void);
void ResizeBTreeStack(void);
void MarkBTreeStack(void);
void ResetBTreeStack
(
	PREDCONTEXTP pbcContext 
);

#endif /* __BTREE_H */
