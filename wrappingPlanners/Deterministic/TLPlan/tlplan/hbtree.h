#ifndef __HBTREE_H
#define __HBTREE_H
#endif


/* global definitions */

#define HCheckBTreeStack()	if(nHBTreeSP>=nHBTreeMaxSP)HResizeBTreeStack()
#define HGetBTreeSP()		(nHBTreeSP)
#define HSetBTreeSP(n)		(nHBTreeSP=n)
#define HBTreePush(pbt)		(pphbtStack[nHBTreeSP++]=(pbt))
#define HBTreePop()			(pphbtStack[--nHBTreeSP])

/* global data */

extern HBTREEP *pphbtStack;				// btree stack
extern int nHBTreeSP;					// btree stack pointer
extern int nHBTreeMaxSP;					// btree stack limit

HBTREEP BTree2HBTree
(
 BTREEP pbtTree,
 HBTREEP *apbtOwner,
 int nIndex
);
HBTREEP HBTreeInsert
(
	HBTREEP *apbtOwner,					/* owning world */
	HBTREEP pbtNode,						/* node to insert */
	HBTREEP pbtTree,					/* tree to act on */
	BOOL *insert
);
HBTREEP HBTreeSearch
(
	CELLP pcKey,						/* search key */
	HBTREEP pbtTree
);
HBTREEP HBTreeDelete
(
	HBTREEP *apbtOwner,
	CELLP pcKey,
	HBTREEP pbtTree
);
HBTREEP HMakeBTree
(
	CELLP pcKey,
	CELLP pcValue,
	HBTREEP pbtLeft,
	HBTREEP pbtRight,
	HBTREEP *apbtOwner,
	int nIndex,
	double hsp_cost
);
HBTREEP HMakeBTree_FF
(
	CELLP pcKey,
	CELLP pcValue,
	HBTREEP pbtLeft,
	HBTREEP pbtRight,
	HBTREEP *apbtOwner,
	int nIndex,
	double hsp_cost,
        OPERINSTANCEP poiOperInstance	
);
CELLP HBTreeGenerator
(
	HBTREEP pbtTree,
	void **ppvContext
);
CELLP HBTreeGeneratorWithBoundVars
(
	HBTREEP pbtTree,
	void **ppvContext,
	CELLP pcArgs
);
void HInitBTreeStack(void);
void HResizeBTreeStack(void);
void MarkHBTreeStack(void);


void HResetBTreeStack
(
	PREDCONTEXTP pbcContext 
);

int HBTreeCount
(
	HBTREEP pbtTree,						/* btree to count */
	int nCount							/* initialize to 0 */
);


LISTP HBTreeToList
(
	HBTREEP pbtTree,						/* tree to enumerate */
	BOOL (*pfTest)(CELLP,HBTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* key argument for test (optional) */
	LISTP (*pfTransform)(CELLP, HBTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
);

LISTP InsertOperInstance
(
 OPERINSTANCEP poiNewInstance,
 LISTP plList
);
