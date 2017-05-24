/* msort.h -- merge sort declarations

*/

/* global structures and definitions */

typedef struct MergeList				/* minimal list structure */
{
	struct MergeList *pmNext;			/* pointer to next */
}MERGELIST, *MERGELISTP;

typedef int (*MERGECOMPAREP)(const void *, const void *);	/* comparison function */

/* global function prototypes */

MERGELISTP MergeSort
(
	MERGELISTP pmList,					/* pointer to start of list */
	int (*pfCmp)(const void *, const void *)	/* comparison function */
);
MERGELISTP MergeSortedLists(MERGELISTP pm1, MERGELISTP pm2,
	int (*pfCmp)(const void *, const void *));

