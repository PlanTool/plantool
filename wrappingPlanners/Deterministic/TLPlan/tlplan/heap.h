// heap.h

typedef struct heap {
  LINEARPLANP *pplpHeap;					// heap array
  int nHeapLength;						// size of heap array
  int nHeapCount;							// number of nodes stored in heap
} HEAP, *HEAPP;

HEAPP CreateHeap(void);
void HeapBuild(HEAPP phHeap);
void HeapSort(HEAPP phHeap);
LINEARPLANP HeapExtractMin(HEAPP phHeap);

void HeapInsert
(
        HEAPP phHeap,
	LINEARPLANP plpNode					// node world to insert into heap 
);
BOOL HeapDelete
(
        HEAPP phHeap,
	LINEARPLANP plpNode					// node world to delete
);

void MarkHeap(HEAPP phHeap);
