/* hbtree.c

Most of the functions in btree.c are copied here, and all are preceded by an H.

These functions implement btrees but for worlds utilized by the heuristic function


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
#include "hbtree.h"
#include "formula.h"
#include "iface.h"
#include "list.h"
#include "search.h"
#include "tl_tab.h"
#include "tlparse.h"
#include "util.h"
#include "zone.h"
#include "var.h"
#include "adl.h"

#define MIN_BTREE_STACK			32		// default btree stack size
#define BTREE_STACK_SIGNATURE	0xABCDDCBA	// stack reserved world signature


HBTREEP *pphbtStack=NULL;					// btree stack
int nHBTreeSP=0;							// btree stack pointer
int nHBTreeMaxSP=0;						// btree stack limit

static void addAddedFact
(
 HBTREEP pbtNode,
 OPERINSTANCEP poiOperInstance
);

static HBTREEP HInsert
(
	HBTREEP *apbtOwner,					/* owning world */
	HBTREEP pbtNode,						/* node to insert */
	HBTREEP pbtTree,						/* tree to act on */
	BOOL *insert
);
static HBTREEP HBTreeSuccessor
(
	CELLP pcKey,
	HBTREEP pbtTree
);

static LISTP HGetReversedPath
(
	CELLP pcKey,
	HBTREEP pbtTree
);
static HBTREEP HBTreeMin
(
	HBTREEP pbtTree
);
static LISTP HBTreeWalk
(
	LISTP plAccumulator,				/* accumulated list */
	HBTREEP pbtTree,						/* current tree location */
	BOOL (*pfTest)(CELLP,HBTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* test argument (optional) */
	LISTP (*pfTransform)(CELLP, HBTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
);



static BOOL (*pfKeyLtQ)(CELLP, CELLP)=ArgsLtQ_Hash;
static BOOL (*pfKeyEqQ)(CELLP, CELLP)=FormulaListEqQ;
static BOOL (*pfArgsCmpQ)(CELLP, CELLP)=ArgsCmpQ_Hash;
static BOOL (*pfArgsCmpQWithVars)(CELLP, CELLP)=ArgsCmpQWithVars_Hash;

// Old comparitors
//static BOOL (*pfKeyLtQ)(CELLP, CELLP)=ArgsLtQ;
//static BOOL (*pfKeyEqQ)(CELLP, CELLP)=FormulaListEqQ;
//static BOOL (*pfArgsCmpQ)(CELLP, CELLP)=ArgsCmpQ;
//static BOOL (*pfArgsCmpQWithVars)(CELLP, CELLP)=ArgsCmpQWithVars;




void addAddedFact
(
 HBTREEP pbtNode,
 OPERINSTANCEP poiOperInstance
)
{
  LISTP pl;
  pl=(LISTP)MemAlloc(sizeof(LIST));
  pl->uValue.phbtTree=pbtNode;
  pl->plNext=poiOperInstance->plAddedFacts;
  poiOperInstance->plAddedFacts=pl;

  poiOperInstance->nAddedFacts++;
}


HBTREEP HBTreeInsert
(
	HBTREEP *apbtOwner,					/* owning world */
	HBTREEP pbtNode,						/* node to insert */
	HBTREEP pbtTree,			  /* tree to act on */
	BOOL *inserted                   /* whether or not the element was inserted in the tree */
)
{
	pbtNode->pbtRight=NULL;				/* Ensure only single node is inserted */
	pbtNode->pbtLeft=NULL;
	return HInsert(apbtOwner,pbtNode,pbtTree,inserted);
}


/* inserts an instance in order into an list of action instances ordered by nHspCost */

LISTP InsertOperInstance
(
 OPERINSTANCEP poiNewInstance,
 LISTP plList
)
{
  if (!plList ||
      plList->uValue.poiOperInstance->nDepth > poiNewInstance->nDepth ||
      (plList->uValue.poiOperInstance->nDepth == poiNewInstance->nDepth &&
       plList->uValue.poiOperInstance->dHspCost > poiNewInstance->dHspCost) ) {
    
    LISTP newAction;
    newAction=(LISTP)MemAlloc(sizeof(LIST));
    newAction->uValue.poiOperInstance=poiNewInstance;
    newAction->plNext=plList;
    return newAction;
  } else {
    plList->plNext=InsertOperInstance(poiNewInstance,plList->plNext);
    return plList;
  }
}

static HBTREEP HInsert
(
	HBTREEP *apbtOwner,					/* owning world */
	HBTREEP pbtNode,						/* node to insert */
	HBTREEP pbtTree,						/* tree to act on */
	BOOL *inserted                /* whether or not the element was inserted in the tree */
)
{
	CELLP pcNewKey;
	HBTREEP pbtNewTree;

//	nInsertCount++;

	ENTER("HInsert",TRUE);
	if(!pbtTree)
	{
		EXIT("HInsert");
		*inserted=1;
		if (pbtNode->hsp_cost==HSP_COST_FROM_IGNORED_OPERATOR)
		  pbtNode->hsp_cost=0; /* added by an ignored action, its cost should be 0 */
		//if (bOcclusions) {
		  // add this instance to the action list
		  pbtNode->plActionList=(LISTP)MemAlloc(sizeof(LIST));
		  pbtNode->plActionList->uValue.poiOperInstance=pbtNode->poiOperInstance;
		  pbtNode->plActionList->plNext=NULL;
		  //}
		pbtNode->apbtOwner=apbtOwner;
		addAddedFact(pbtNode,pbtNode->poiOperInstance);

		return pbtNode;					/* No tree, return single node tree. */
	}
	pcNewKey=pbtNode->pcKey;
	switch((*pfArgsCmpQ)(pcNewKey,pbtTree->pcKey))	/* put into correct side. */
	{
	case -1:
	  //	pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?pbtTree:HCopyBTree(pbtTree);
	        pbtNewTree=pbtTree;
		pbtNewTree->pbtLeft=HInsert(apbtOwner,pbtNode,pbtTree->pbtLeft,inserted);
	  //	pbtNewTree->apbtOwner=apbtOwner;
		//		*inserted=1;
		break;
	case 1:
	  //	pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?pbtTree:HCopyBTree(pbtTree);
	        pbtNewTree=pbtTree;
	        pbtNewTree->pbtRight=HInsert(apbtOwner,pbtNode,pbtTree->pbtRight,inserted);
	  //	pbtNewTree->apbtOwner=apbtOwner;
		//	*inserted=1;
		break;
       
	default: 
	  

	       if (pbtNode->hsp_cost==HSP_COST_FROM_IGNORED_OPERATOR) {
		 *inserted=0; /* There has been no addition because the node is there */
		 pbtNewTree=pbtTree; /* we've found the key, just return */
	       } else { 
		 if (pbtNode->hsp_cost < pbtTree->hsp_cost) { 
		   LISTP newAction;
		 /* cost of existing inserted node is lower than that of the
		    inserted node. We just update the cost, and behave as if it was inserted */
		 // pbtTree->hsp_cost=pbtNode->hsp_cost;

		   /* copy all information instead of replacing */

		   pbtTree->hsp_cost=pbtNode->hsp_cost;

		   /* relaxed plan info */
		   pbtTree->poiOperInstance=pbtNode->poiOperInstance;

		   //		   pbtTree->pcFFSupFacts=pbtNode->pcFFSupFacts;
		   //		   pbtTree->nFFOperNumber=pbtNode->nFFOperNumber;
		   //		   pbtTree->nFFOperInstance=pbtNode->nFFOperInstance;
		   //		   pbtTree->nFFDepth=pbtNode->nFFDepth;
		   //		   pbtTree->pcFFActionInstance=pbtNode->pcFFActionInstance;
		   //		   pbtTree->pbBindings=pbtNode->pbBindings;

		   /* occlusion info */
		   pbtTree->bNotOccluded=pbtNode->bNotOccluded;
		   pbtTree->nDepthAdded=pbtNode->nDepthAdded;
		   
		   /* we return the pbtTree */
		   pbtNewTree=pbtTree;
		 
		   //if (!bComputingOcclusions) {
		     // add this instance to the action list

		     newAction=(LISTP)MemAlloc(sizeof(LIST));
		     newAction->uValue.poiOperInstance=pbtNode->poiOperInstance;
		     newAction->plNext=pbtTree->plActionList;
		     //newAction->plNext=NULL;
		     pbtNewTree->plActionList=newAction;
		     //}
		   
		   addAddedFact(pbtNewTree,pbtNode->poiOperInstance);

		   *inserted=1;
		 } else {

		   pbtTree->bNotOccluded=pbtNode->bNotOccluded;
		   pbtTree->nDepthAdded=pbtNode->nDepthAdded;
		   pbtNewTree=pbtTree; 
		   *inserted=0; /* don't need to do anything */

		   //  if (!bComputingOcclusions&&pbtNode->hsp_cost==pbtTree->hsp_cost) {
		     // add this instance to the action list
		   //  LISTP newAction;
		   //  newAction=(LISTP)MemAlloc(sizeof(LIST));
		   //  newAction->uValue.poiOperInstance=pbtNode->poiOperInstance;
		   //  newAction->plNext=pbtNewTree->plActionList;
		   //  pbtNewTree->plActionList=newAction;  // adding element to list
		   //} else if (!bComputingOcclusions) {
		   if (!bComputingOcclusions) {
		     pbtNewTree->plActionList=InsertOperInstance(pbtNode->poiOperInstance,
								 pbtNewTree->plActionList);
		   }
		   addAddedFact(pbtNewTree,pbtNode->poiOperInstance);
		 }
		 if (bComputingOcclusions) {
		   // add this instance to the action list
		   LISTP newAction;
		   newAction=(LISTP)MemAlloc(sizeof(LIST));
		   newAction->uValue.poiOperInstance=pbtNode->poiOperInstance;
		   newAction->plNext=pbtNewTree->plActionList;
		   pbtNewTree->plActionList=newAction;  // adding element to list
		 }
	       }
	       break;
	}
	EXIT("HInsert");
	return pbtNewTree;
}


/* static HBTREEP HCopyBTree */
/* ( */
/* 	HBTREEP pbt */
/* ) */
/* { */
/* 	HBTREEP pbtCopy; */

/* 	ENTER("HCopyBTree",TRUE); */
/* 	pbtCopy=(HBTREEP)CopyAlloc(pbt,sizeof(struct HBTree)); */
/* 	EXIT("HCopyBTree"); */
/* 	return pbtCopy; */
/* } */

HBTREEP HBTreeSearch
(
	CELLP pcKey,						/* search key */
	HBTREEP pbtTree
)
{
	HBTREEP pbt;

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


HBTREEP HBTreeDelete
(
	HBTREEP *apbtOwner,
	CELLP pcKey,
	HBTREEP pbtTree
)
{
	HBTREEP pbt;
	HBTREEP pbtNewTree;
	HBTREEP pbtSuccessorNode;

	ENTER("HBTreeDelete",TRUE);

	if (!pbtTree) {/* we have not found the element to delete */
	  EXIT("HBTreeDelete");
	  return NULL; 
	}
	if((*pfKeyEqQ)(pcKey,pbtTree->pcKey))	/* Need to delete root. */
	{
		if(pbtTree->pbtRight&&pbtTree->pbtLeft)	/* Has two children */
		{
			pbtSuccessorNode=HBTreeSuccessor(pcKey,pbtTree);
//			pbtNewTree=(pbtSuccessorNode->apbtOwner==apbtOwner)?
//				pbtSuccessorNode:CopyBTree(pbtSuccessorNode);
			pbtNewTree=pbtSuccessorNode;
			pbtNewTree->pbtRight=HBTreeDelete(apbtOwner,pbtSuccessorNode->pcKey,
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
//		pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?
//		pbtTree:CopyBTree(pbtTree);
	        pbtNewTree=pbtTree;
		pbtNewTree->pbtLeft=HBTreeDelete(apbtOwner,pcKey,pbtTree->pbtLeft);
		pbtNewTree->apbtOwner=apbtOwner;
		pbtNewTree->pbtRight=pbtTree->pbtRight;
		pbt=pbtNewTree;
	}
	else
	{
//		pbtNewTree=(pbtTree->apbtOwner==apbtOwner)?
//			pbtTree:CopyBTree(pbtTree);
	        pbtNewTree=pbtTree;
		pbtNewTree->pbtRight=HBTreeDelete(apbtOwner,pcKey,pbtTree->pbtRight);
		pbtNewTree->apbtOwner=apbtOwner;
		pbtNewTree->pbtLeft=pbtTree->pbtLeft;
		pbt=pbtNewTree;
	}
	EXIT("HBTreeDelete");
	return pbt;
}


static HBTREEP HBTreeSuccessor
(
	CELLP pcKey,
	HBTREEP pbtTree
)
{
	HBTREEP pbt;
	HBTREEP pbtParent;
	LISTP plRevPathToItem;
	LISTP pl;

	ENTER("HBTreeSuccessor",TRUE);
	plRevPathToItem=HGetReversedPath(pcKey,pbtTree);
	if(!plRevPathToItem)
	{
		EXIT("HBTreeSuccessor");
		return NULL;
	}
	if(!(*pfKeyEqQ)(pcKey,((HBTREEP)plRevPathToItem->uValue.pbtTree)->pcKey))
	{
		EXIT("HBTreeSuccessor");
		return NULL;					/* key not found */
	}
	pl=plRevPathToItem;					/* point to node */
	pbt=pl->uValue.phbtTree;
	if(pbt->pbtRight)					/* if right tree exists */
	{
		pbt=HBTreeMin(pbt->pbtRight);
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
	pbtParent=pl->uValue.phbtTree;
	while(pbt==pbtParent->pbtRight)
	{
		pbt=pbtParent;
		pl=pl->plNext;					/* point to parent node */
		if(!pl)
		{
			EXIT("BTreeSuccessor");
			return NULL;
		}
		pbtParent=pl->uValue.phbtTree;
	}
	EXIT("HBTreeSuccessor");
	return pbtParent;
}


static LISTP HGetReversedPath
(
	CELLP pcKey,
	HBTREEP pbtTree
)
{
	LISTP plAccumulator;				/* list accumulator */
	LISTP pl;							/* list pointer */
	HBTREEP pbt;							/* tree pointer */

	ENTER("GetReversedPath",TRUE);
	plAccumulator=NULL;					/* initialize list */

	for(pbt=pbtTree;pbt;)
	{
		pl=NewList(ATOM_HBTREEP,"",pbt);	/* insert new node at front of list */
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


static HBTREEP HBTreeMin
(
	HBTREEP pbtTree
)
{
	HBTREEP pbt;							/* tree pointer */

	ENTER("BTreeMin",TRUE);
	for(pbt=pbtTree;pbt->pbtLeft;pbt=pbt->pbtLeft);
	EXIT("BTreeMin");
	return(pbt);
}





HBTREEP BTree2HBTree
(
 BTREEP pbtTree,
 HBTREEP *apbtOwner,
 int nIndex
)
{
  HBTREEP phbt;
  if (pbtTree == NULL) return NULL;
  phbt = (HBTREEP) MemAlloc(sizeof(struct HBTree));
  
  phbt->pbtNext = NULL;
  phbt->hsp_cost = 0;
  phbt->nIndex = nIndex;
  phbt->pcKey = pbtTree->pcKey;
  phbt->pcValue = pbtTree->pcValue;
  phbt->pbtLeft = BTree2HBTree(pbtTree->pbtLeft,apbtOwner,nIndex);
  phbt->pbtRight = BTree2HBTree(pbtTree->pbtRight,apbtOwner,nIndex);
  
  phbt->poiOperInstance=(OPERINSTANCEP)MemAlloc(sizeof(OPERINSTANCE));
  phbt->poiOperInstance->nDepth=0;
  phbt->poiOperInstance->nOperInstance=-1;
  phbt->poiOperInstance->nOperNumber=-1;
  phbt->poiOperInstance->pbBindings=NULL;
  phbt->poiOperInstance->pcSupFacts=NULL;
  phbt->poiOperInstance->pcActionInstance=NULL;
  
  phbt->bFFSearched=FALSE;
  phbt->bNotOccluded=1;
  phbt->nDepthAdded=0;
  phbt->plActionList=NULL;
  phbt->aphbtComplement=NULL;
  phbt->apbtOwner=apbtOwner; 

  phbt->nDepthOccCounted=-1;
  phbt->nDepthAddedOcc=0; // added at depth 0
  phbt->bAddedByRelaxed=1; // nodes copied from original world, i.e. regarded as added by current relaxed plan
  phbt->nDepthAddedByRelaxed=0; // added at depth 0
  phbt->nMinDepthAddedByRelaxed=0; // added at depth 0
  return phbt;
}
  

  
HBTREEP HMakeBTree
(
	CELLP pcKey,
	CELLP pcValue,
	HBTREEP pbtLeft,
	HBTREEP pbtRight,
	HBTREEP *apbtOwner,
	int nIndex,
	double hsp_cost
)
{
	HBTREEP pbt;

	ENTER("HMakeBTree",TRUE);
	pbt=(HBTREEP)MemAlloc(sizeof(struct HBTree));
	pbt->pcKey=CopyFormulaList(pcKey);
	pbt->pcValue=pcValue;
	pbt->pbtLeft=pbtLeft;
	pbt->pbtRight=pbtRight;
	pbt->apbtOwner=apbtOwner;
	pbt->hsp_cost = hsp_cost;
	pbt->bNotOccluded=0;
	pbt->nDepthAdded=0;
	pbt->nIndex=nIndex;
	pbt->plActionList=NULL;
	pbt->aphbtComplement=NULL;
	EXIT("HMakeBTree");
	return pbt;
}


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
)
{
	HBTREEP pbt;

	ENTER("HMakeBTree",TRUE);
	pbt=(HBTREEP)MemAlloc(sizeof(struct HBTree));
	pbt->pcKey=CopyFormulaList(pcKey);
	pbt->pcValue=pcValue;
	pbt->pbtLeft=pbtLeft;
	pbt->pbtRight=pbtRight;
	pbt->apbtOwner=apbtOwner;
	pbt->hsp_cost = hsp_cost;
	pbt->nIndex=nIndex;
	pbt->poiOperInstance=poiOperInstance;
	pbt->bFFSearched=FALSE;
	pbt->bNotOccluded=1;
	pbt->nDepthAdded=poiOperInstance->nDepth;
	pbt->plActionList=NULL;
	pbt->aphbtComplement=NULL;

	pbt->nDepthOccCounted=-1;
	pbt->nDepthAddedOcc=-1; // not added yet
	pbt->bAddedByRelaxed=0; // nodes newly created: regarded as added by current relaxed plan
	pbt->nDepthAddedByRelaxed=-1;
	pbt->nMinDepthAddedByRelaxed=10000; 
	pbt->plActionListRelPlan=NULL; // no action has added this fact
	EXIT("HMakeBTree");
	return pbt;
}



/* same as the BTreeGenerator but for HBtrees */

CELLP HBTreeGenerator
(
	HBTREEP pbtTree,
	void **ppvContext
)
{
	HBTREEP pbt;
	HBTREEP pbtCurrent;

	/* handle initialization.*/
	
	ENTER("HBTreeGenerator",TRUE);
	if(!*ppvContext)
	{
		if(!pbtTree)
		{
//			fprintf(stderr,"Empty btree\n");
			EXIT("HBTreeGenerator");
			return NULL;				/* nothing to do */
		}
		
		*ppvContext=(void *)HGetBTreeSP();	// save stack base index
		
		/* Traverse the tree to the left and store each node's 
		address on the stack. */

		HCheckBTreeStack();
		HBTreePush(NULL);
		for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
		{
			HCheckBTreeStack();
			HBTreePush(pbt);
		}
//		ShowBTreeStack("New",(int)*ppvContext);
		EXIT("BTreeGenerator");
		return NULL;					/* ignored return value */
	}
	
	// are we done yet?

	pbtCurrent=HBTreePop();				// next node to enumerate
	if(!pbtCurrent)
	{
#ifdef _DEBUG
		if((int)*ppvContext!=nHBTreeSP)
			fprintf(stderr,"\nStack Pointer doesn't match... is %d should be %d\n",
				nHBTreeSP,(int)*ppvContext);
//		ShowBTreeStack("Done",(int)*ppvContext);
#endif // _DEBUG
		EXIT("BTreeGenerator");
		return NULL;					// we're done
	}

	/* find successor, saving any new context on stack */
	
	for(pbt=pbtCurrent->pbtRight;pbt;pbt=pbt->pbtLeft)
	{
		HCheckBTreeStack();
		HBTreePush(pbt);
	}

//	ShowBTreeStack("Key",(int)*ppvContext);
	EXIT("HBTreeGenerator");
	return pbtCurrent->pcKey;
}






// BTreeGeneratorWithBoundVars

// Description:
//	Special version for the case when the first of the
//	generator's arguments are bound.
// Note:
//	This version uses a stack and is fully reentrant.

CELLP HBTreeGeneratorWithBoundVars
(
	HBTREEP pbtTree,
	void **ppvContext,
	CELLP pcArgs
)
{
	HBTREEP pbt;
	HBTREEP pbtCurrent;

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
		*ppvContext=(void *)HGetBTreeSP();	// save stack base index

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
		
		HCheckBTreeStack();
		HBTreePush(NULL);
		while(pbt)
		{
			switch((*pfArgsCmpQWithVars)(pcArgs,pbt->pcKey)) 
			{
				case 0:					// Put on stack and try to find a smaller node
					HCheckBTreeStack();
					HBTreePush(pbt);
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

	pbtCurrent=HBTreePop();				// next node to enumerate
	if(!pbtCurrent)
	{
#ifdef _DEBUG
		if((int)*ppvContext!=nHBTreeSP)
			fprintf(stderr,"\nStack Pointer doesn't match... is %d should be %d\n",
				nHBTreeSP,(int)*ppvContext);
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
			HCheckBTreeStack();
			HBTreePush(pbt);
			
			/*special case if the top matches all of its left children must also
			match, so we can avoid comparing them*/
			
			for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft)
			{
				HCheckBTreeStack();
				HBTreePush(pbt);
			}
		}
		else
		{
			/* even the left children might not match, so we must compare them*/
			
			for(pbt=pbt->pbtLeft;pbt;pbt=pbt->pbtLeft) 
			{
				if((*pfArgsCmpQWithVars)(pcArgs,pbt->pcKey)==0)
				{
					HCheckBTreeStack();
					HBTreePush(pbt);
				}
			}
		}
	}

//	ShowBTreeStack("Key+",(int)*ppvContext);
	EXIT("HBTreeGeneratorWithBoundVars");
	return pbtCurrent->pcKey;
}












// InitBTreeStack

// Description:
//	Allocate a default quantity of memory to the btree stack.
//	The first entry of the stack is reserved, and filled with a sanity marker.

void HInitBTreeStack(void)
{
	nHBTreeMaxSP=MIN_BTREE_STACK-1;
	pphbtStack=(HBTREEP *)MemAlloc((nHBTreeMaxSP+1)*sizeof(HBTREEP));
	nHBTreeSP=0;
	pphbtStack[nHBTreeSP++]=(HBTREEP)BTREE_STACK_SIGNATURE;	// stack 
}

// ResizeBTreeStack

// Description:
//	Double the size of the btree stack.
// Notes:
//	We always allocate our memory in the permanent zone.

void HResizeBTreeStack(void)
{
	HBTREEP *ppbt;
	ZONEP pz;

	nHBTreeMaxSP++;
	pz=pzCurrent;
	SetZone(&zPermanent);
	ppbt=(HBTREEP *)MemAlloc(2*nHBTreeMaxSP*sizeof(HBTREEP));
	SetZone(pz);
	memcpy(ppbt,pphbtStack,nHBTreeMaxSP*sizeof(HBTREEP));
	pphbtStack=ppbt;
	nHBTreeMaxSP=2*nHBTreeMaxSP-1;
#ifdef _DEBUG
	fprintf(stderr,"Resizing btree stack to %d entries.\n",nHBTreeMaxSP+1);
	if(*pphbtStack!=(HBTREEP)BTREE_STACK_SIGNATURE)
		fprintf(stderr,"Btree stack signature overwritten\n");
#endif // _DEBUG
}




void HResetBTreeStack
(
	PREDCONTEXTP pbcContext 
)
{
#ifdef _DEBUG
	int nBaseSP;

	nBaseSP=(int)(pbcContext->pvContext);

	if(nHBTreeSP<nBaseSP)
		fprintf(stderr,"\nStack Pointer too small... is %d should be %d\n",
			nHBTreeSP,nHBaseSP);
	if(nHBTreeSP<1)
		fprintf(stderr,"\nStack Pointer non-positive... is %d should be %d\n",
			nHBTreeSP,nHBaseSP);
	if(nHBTreeSP>=nHBTreeMaxSP)
		fprintf(stderr,"\nStack Pointer too large... is %d should be %d\n",
			nHBTreeSP,nHBaseSP);
	if(pphbtStack[nHBaseSP])
		fprintf(stderr,"\nBase Pointer points to non-zero value... pointer is %d value is %d\n",
			nHBTreeSP,pphbtStack[nHBaseSP]);
	nHBTreeSP=nHBaseSP;
//	ShowBTreeStack("Early",nBaseSP);
#else // _DEBUG
	nHBTreeSP=(int)(pbcContext->pvContext);
#endif // _DEBUG
}


int HBTreeCount
(
	HBTREEP pbtTree,						/* btree to count */
	int nCount							/* initialize to 0 */
)
{
	HBTREEP pbt;

	/* to reduce recursion, we walk directly down the left branch of the tree */

	ENTER("HBTreeCount",TRUE);
	for(pbt=pbtTree;pbt;pbt=pbt->pbtLeft)
	{
		nCount++;						/* count this node */
		if(pbt->pbtRight)
			nCount+=HBTreeCount(pbt->pbtRight,0);	/* count the right subtree */
	}
	EXIT("BTreeCount");
	return nCount;
}



LISTP HBTreeToList
(
	HBTREEP pbtTree,						/* tree to enumerate */
	BOOL (*pfTest)(CELLP,HBTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* key argument for test (optional) */
	LISTP (*pfTransform)(CELLP, HBTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
)
{
	LISTP pl;
	LISTP plAccumulator;

	ENTER("HBTreeToList",TRUE);
	plAccumulator=NULL;
	pl=HBTreeWalk(plAccumulator,pbtTree,pfTest,pcKey,pfTransform,pcArg);
	EXIT("HBTreeToList");
	return pl;
}

static LISTP HBTreeWalk
(
	LISTP plAccumulator,				/* accumulated list */
	HBTREEP pbtTree,						/* current tree location */
	BOOL (*pfTest)(CELLP,HBTREEP),		/* test to perform (optional) */
	CELLP pcKey,						/* test argument (optional) */
	LISTP (*pfTransform)(CELLP, HBTREEP),	/* node transform (optional) */
	CELLP pcArg							/* argument to transform */
)
{
	LISTP pl;

	ENTER("HBTreeWalk",TRUE);
	if(!pbtTree)
	{
		EXIT("BTreeWalk");
		return plAccumulator;
	}
	plAccumulator=HBTreeWalk(plAccumulator,pbtTree->pbtRight,
		pfTest,pcKey,pfTransform,pcArg);
	if((pfTest&&(*pfTest)(pcKey,pbtTree))||(!pfTest&&!pcKey))
	{
		if(pfTransform)
			plAccumulator=Cons((*pfTransform)(pcArg,pbtTree),plAccumulator);
		else
			plAccumulator=Cons(NewList(ATOM_BTREEP,"",pbtTree),plAccumulator);
	}
	pl=HBTreeWalk(plAccumulator,pbtTree->pbtLeft,
		pfTest,pcKey,pfTransform,pcArg);
	EXIT("HBTreeWalk");
	return pl;
}


void MarkHBTreeStack(void)
{
	ENTER("MarkBTreeStack",TRUE);
	ZoneMark(pphbtStack);
	EXIT("MarkBTreeStack");
}
