// heap.h

void CreateHeap(void);
void HeapBuild(void);
void HeapSort(void);
LINEARPLANP HeapExtractMin(void);
void HeapInsert
(
	LINEARPLANP plpNode					// node world to insert into heap 
);
BOOL HeapDelete
(
	LINEARPLANP plpNode					// node world to delete
);
void MarkHeap(void);
