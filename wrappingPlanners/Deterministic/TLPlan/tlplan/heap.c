/* heap.c

Copyright C, 2001, Fahiem Bacchus

Description:
	Heap support routines for best-first search open list.
Note:
	The routines are specific to best-first search.  They are not general purpose!
	The heap is also inverted, i.e. it has its smallest member at the root.
*/

#include <stdio.h>
#include <stddef.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "tlplan.h"
#include "heap.h"
#include "plan.h"
#include "search.h"
#include "util.h"
#include "zone.h"
#include "eval.h"

// local definitions

#define Parent(i) ((i)>>1)
#define Left(i) ((i)<<1)
#define Right(i) (((i)<<1)+1)

#define HEAP_MINLENGTH		1024

/* global data */
// old gobal data :)
//LINEARPLANP *pplpHeap;					// heap array
//int nHeapLength;						// size of heap array
//int nHeapCount;							// number of nodes stored in heap

/* local function prototypes */

static void HeapExpand(HEAPP phHeap);
static void Heapify
(
        HEAPP phHeap,
	int nNode							// node to put in order
);

/* CreateHeap
									
Description:
	Allocate and initialize a heap.
*/

HEAPP CreateHeap(void)
{
	ZONEP pz;
	HEAPP phHeap;

	pz=pzCurrent;
	SetZone(&zPermanent);
	phHeap = (HEAPP) MemAlloc(sizeof(HEAP));
	phHeap->pplpHeap=(LINEARPLANP *)MemAlloc(HEAP_MINLENGTH*sizeof(LINEARPLANP));
	SetZone(pz);
	phHeap->nHeapLength=HEAP_MINLENGTH;
	phHeap->nHeapCount=0;

	return phHeap;
}

/* Heapify

Description:
	Move a single node to its proper location in the heap.
*/

static void Heapify
(
        HEAPP phHeap,
	int nNode							// node to put in order
)
{
	LINEARPLANP plp;					// temporary pointer
	int nSmallest;						// index of smallest node
	int nLeft,nRight;					// child node indices

	nLeft=Left(nNode);
	nRight=Right(nNode);
	if(nLeft<=phHeap->nHeapCount&&MergeCompareFn(phHeap->pplpHeap[nLeft-1],phHeap->pplpHeap[nNode-1])==-1)
		nSmallest=nLeft;
	else
		nSmallest=nNode;
	if(nRight<=phHeap->nHeapCount&&MergeCompareFn(phHeap->pplpHeap[nRight-1],phHeap->pplpHeap[nSmallest-1])==-1)
		nSmallest=nRight;
	if(nSmallest!=nNode)
	{
		plp=phHeap->pplpHeap[nNode-1];			// exchange nodes
		phHeap->pplpHeap[nNode-1]=phHeap->pplpHeap[nSmallest-1];
		phHeap->pplpHeap[nSmallest-1]=plp;
		Heapify(phHeap,nSmallest);				// heapify smallest node
	}
}

/* HeapBuild

Description:
	Put an array in heap order, according to heuristic value.
*/

void HeapBuild(HEAPP phHeap)
{
	int i;

	for(i=phHeap->nHeapCount/2;i>0;--i)
	  Heapify(phHeap,i);
}

/* HeapSort

Description:
	Sort an array.
*/

void HeapSort(HEAPP phHeap)
{
	int i;
	LINEARPLANP plp;

	HeapBuild(phHeap);
	for(i=phHeap->nHeapCount;i>1;--i)
	{
		--phHeap->nHeapCount;					// shorten heap
		plp=phHeap->pplpHeap[0];				// exchange... extract node with smallest value from heap
		phHeap->pplpHeap[0]=phHeap->pplpHeap[i];
		phHeap->pplpHeap[i]=plp;
		Heapify(phHeap,1);						// heapify first node
	}
}

/* HeapExtractMin

Description:
	Extract the node with the smallest heuristic value.
*/

LINEARPLANP HeapExtractMin(HEAPP phHeap)
{
	LINEARPLANP plpMin;

	if(phHeap->nHeapCount<1)
		return NULL;
	plpMin=phHeap->pplpHeap[0];
	--phHeap->nHeapCount;
	phHeap->pplpHeap[0]=phHeap->pplpHeap[phHeap->nHeapCount];
	Heapify(phHeap,1);
	return plpMin;
}

/* HeapInsert

Description:
	Insert a single node into a heap.  We insert the node at the end of the
	heap, then shift as far towards the start as it will go.
*/

void HeapInsert
(
        HEAPP phHeap,
	LINEARPLANP plpNode					// node world to insert into heap 
)
{
	int i;

	if(phHeap->nHeapCount>=phHeap->nHeapLength)
		HeapExpand(phHeap);
	phHeap->nHeapCount++;
	for(i=phHeap->nHeapCount;i>1&&MergeCompareFn(phHeap->pplpHeap[Parent(i)-1],plpNode)==1;i=Parent(i))
		phHeap->pplpHeap[i-1]=phHeap->pplpHeap[Parent(i)-1];
	phHeap->pplpHeap[i-1]=plpNode;
}

/* HeapDelete

Description:
	Delete a single node from a heap.  First we have to search for what we're looking
	for.  Then overwrite the node in question with the last element
	of the heap.  Then call heapify to fix the heap.
Returns:
	TRUE if the node was found in the heap, and was deleted.
*/

BOOL HeapDelete
(
        HEAPP phHeap,
	LINEARPLANP plpNode					// node world to delete
)
{
	int nNode;							// index of node to delete

	// find the node in the heap
	
	for(nNode=0;nNode<phHeap->nHeapCount;nNode++)
		if(phHeap->pplpHeap[nNode]==plpNode)
			break;
	if(nNode==phHeap->nHeapCount)
		return FALSE;					// node not found

	// delete the node from the heap
	
	--phHeap->nHeapCount;
	if(nNode<phHeap->nHeapCount)
	{
		phHeap->pplpHeap[nNode]=phHeap->pplpHeap[phHeap->nHeapCount];
		Heapify(phHeap,nNode+1);
	}
	return TRUE;
}

/* HeapExpand

Description:
	Double the size of the heap.
*/

static void HeapExpand(HEAPP phHeap)
{
	LINEARPLANP *pplp;					// new heap pointer
	ZONEP pz;

	ENTER("HeapExpand",TRUE);

//	fprintf(stderr,"Expanding heap to %d cells\n",phHeap->nHeapLength*2);

	// allocate a new heap, doubling the size

	pz=pzCurrent;
	SetZone(&zPermanent);
	pplp=(LINEARPLANP *)MemAlloc(2*phHeap->nHeapLength*sizeof(LINEARPLANP));
	SetZone(pz);
	memcpy(pplp,phHeap->pplpHeap,phHeap->nHeapLength*sizeof(LINEARPLANP));
	phHeap->nHeapLength*=2;
	phHeap->pplpHeap=pplp;

	EXIT("HeapExpand");
}

/* MarkHeap

Description:
	Mark the contents of the heap for garbage collection.
*/

void MarkHeap(HEAPP phHeap)
{
	int i;								// loop index

	ENTER("CRCHashInsert",TRUE);


	if (phHeap) {

	  ZoneMark(phHeap);
	  
	  if(phHeap->pplpHeap)
	    {
	      ZoneMark(phHeap->pplpHeap);
	      
	      for(i=0;i<phHeap->nHeapCount;i++) {
		MarkLinearPlan(phHeap->pplpHeap[i]);
	      }
	    }
	}	
}
