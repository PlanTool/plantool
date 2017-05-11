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

// local definitions

#define Parent(i) ((i)>>1)
#define Left(i) ((i)<<1)
#define Right(i) (((i)<<1)+1)

#define HEAP_MINLENGTH		1024

/* global data */

LINEARPLANP *pplpHeap;					// heap array
int nHeapLength;						// size of heap array
int nHeapCount;							// number of nodes stored in heap

/* local function prototypes */

static void HeapExpand(void);
static void Heapify
(
	int nNode							// node to put in order
);

/* CreateHeap
									
Description:
	Allocate and initialize a heap.
*/

void CreateHeap(void)
{
	ZONEP pz;

	pz=pzCurrent;
	SetZone(&zPermanent);
	pplpHeap=(LINEARPLANP *)MemAlloc(HEAP_MINLENGTH*sizeof(LINEARPLANP));
	SetZone(pz);
	nHeapLength=HEAP_MINLENGTH;
	nHeapCount=0;
}

/* Heapify

Description:
	Move a single node to its proper location in the heap.
*/

static void Heapify
(
	int nNode							// node to put in order
)
{
	LINEARPLANP plp;					// temporary pointer
	int nSmallest;						// index of smallest node
	int nLeft,nRight;					// child node indices

	nLeft=Left(nNode);
	nRight=Right(nNode);
	if(nLeft<=nHeapCount&&MergeCompareFn(pplpHeap[nLeft-1],pplpHeap[nNode-1])==-1)
		nSmallest=nLeft;
	else
		nSmallest=nNode;
	if(nRight<=nHeapCount&&MergeCompareFn(pplpHeap[nRight-1],pplpHeap[nSmallest-1])==-1)
		nSmallest=nRight;
	if(nSmallest!=nNode)
	{
		plp=pplpHeap[nNode-1];			// exchange nodes
		pplpHeap[nNode-1]=pplpHeap[nSmallest-1];
		pplpHeap[nSmallest-1]=plp;
		Heapify(nSmallest);				// heapify smallest node
	}
}

/* HeapBuild

Description:
	Put an array in heap order, according to heuristic value.
*/

void HeapBuild(void)
{
	int i;

	for(i=nHeapCount/2;i>0;--i)
		Heapify(i);
}

/* HeapSort

Description:
	Sort an array.
*/

void HeapSort(void)
{
	int i;
	LINEARPLANP plp;

	HeapBuild();
	for(i=nHeapCount;i>1;--i)
	{
		--nHeapCount;					// shorten heap
		plp=pplpHeap[0];				// exchange... extract node with smallest value from heap
		pplpHeap[0]=pplpHeap[i];
		pplpHeap[i]=plp;
		Heapify(1);						// heapify first node
	}
}

/* HeapExtractMin

Description:
	Extract the node with the smallest heuristic value.
*/

LINEARPLANP HeapExtractMin(void)
{
	LINEARPLANP plpMin;

	if(nHeapCount<1)
		return NULL;
	plpMin=pplpHeap[0];
	--nHeapCount;
	pplpHeap[0]=pplpHeap[nHeapCount];
	Heapify(1);
	return plpMin;
}

/* HeapInsert

Description:
	Insert a single node into a heap.  We insert the node at the end of the
	heap, then shift as far towards the start as it will go.
*/

void HeapInsert
(
	LINEARPLANP plpNode					// node world to insert into heap 
)
{
	int i;

	if(nHeapCount>=nHeapLength)
		HeapExpand();
	nHeapCount++;
	for(i=nHeapCount;i>1&&MergeCompareFn(pplpHeap[Parent(i)-1],plpNode)==1;i=Parent(i))
		pplpHeap[i-1]=pplpHeap[Parent(i)-1];
	pplpHeap[i-1]=plpNode;
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
	LINEARPLANP plpNode					// node world to delete
)
{
	int nNode;							// index of node to delete

	// find the node in the heap
	
	for(nNode=0;nNode<nHeapCount;nNode++)
		if(pplpHeap[nNode]==plpNode)
			break;
	if(nNode==nHeapCount)
		return FALSE;					// node not found

	// delete the node from the heap
	
	--nHeapCount;
	if(nNode<nHeapCount)
	{
		pplpHeap[nNode]=pplpHeap[nHeapCount];
		Heapify(nNode+1);
	}
	return TRUE;
}

/* HeapExpand

Description:
	Double the size of the heap.
*/

static void HeapExpand(void)
{
	LINEARPLANP *pplp;					// new heap pointer
	ZONEP pz;

	ENTER("HeapExpand",TRUE);

//	fprintf(stderr,"Expanding heap to %d cells\n",nHeapLength*2);

	// allocate a new heap, doubling the size

	pz=pzCurrent;
	SetZone(&zPermanent);
	pplp=(LINEARPLANP *)MemAlloc(2*nHeapLength*sizeof(LINEARPLANP));
	SetZone(pz);
	memcpy(pplp,pplpHeap,nHeapLength*sizeof(LINEARPLANP));
	nHeapLength*=2;
	pplpHeap=pplp;

	EXIT("HeapExpand");
}

/* MarkHeap

Description:
	Mark the contents of the heap for garbage collection.
*/

void MarkHeap(void)
{
	int i;								// loop index

	ENTER("CRCHashInsert",TRUE);

	if(pplpHeap)
	{
		ZoneMark(pplpHeap);

		for(i=0;i<nHeapCount;i++)
			MarkLinearPlan(pplpHeap[i]);
	}
}	
