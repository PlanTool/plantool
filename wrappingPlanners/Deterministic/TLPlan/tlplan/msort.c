/* msort.c

Copyright C, 1997 - 99  Winter City Software Corp.

Description:
	Sort a linked list, using a merge sort.
*/

#include <stdio.h>

#include "tlplan.h"
#include "msort.h"

/* local function prototypes */

static MERGELISTP Divide(MERGELISTP pmList);

/* MergeSort

Description:
	Sort a linked list.
*/

MERGELISTP MergeSort
(
	MERGELISTP pmList,					/* pointer to start of list */
	int (*pfCmp)(const void *, const void *)	/* comparison function */
)
{
	MERGELISTP pmLast;					/* pointer to last half of list */

	ENTER("MergeSort",TRUE);
	if(pmList&&pmList->pmNext)			/* if something to sort */
	{
		pmLast=Divide(pmList);			/* split the list */
		pmList=MergeSort(pmList,pfCmp);	/* sort each half */
		pmLast=MergeSort(pmLast,pfCmp);
		pmList=MergeSortedLists(pmList,pmLast,pfCmp);	/* merge together */
	}
	EXIT("MergeSort");
	return(pmList);
}

/* Divide

Description:
	Divide a linked list into 2 parts.
	Returns a pointer to the second half.
	If the number of links is odd, then the larger half is the first half.
*/

static MERGELISTP Divide
(
	MERGELISTP pmList					/* pointer to list to divide */
)
{
	MERGELISTP pm1,pm2;

	ENTER("Divide",TRUE);
	pm1=pmList;
	pm2=pmList->pmNext->pmNext;			/* must be at least 2 nodes in list */
	while(pm2)							/* move pm2 twice for each move of pm1 */
	{
		pm1=pm1->pmNext;
		pm2=pm2->pmNext;
		if(pm2)
			pm2=pm2->pmNext;
	}
	pm2=pm1->pmNext;					/* break after pm1 */
	pm1->pmNext=0;
	EXIT("Divide");
	return(pm2);						/* return pointer to second half */
}

/* MergeSortedLists

Description:
	Merge 2 sorted lists.
Note:
	Must not be called with null pointers.
*/

MERGELISTP MergeSortedLists
(
	MERGELISTP pm1,						/* input list */
	MERGELISTP pm2,						/* input list */
	int (*pfCmp)(const void *, const void *)	/* comparison function */
)
{
	MERGELISTP pmHead;
	MERGELISTP pm;

	ENTER("MergeSortedLists",TRUE);
	if(!pm1)							/* handle empty lists gracefully */
		return pm2;
	if(!pm2)
		return pm1;
	if((*pfCmp)(pm1,pm2)<=0)
	{
		pmHead=pm1;
		pm1=pm1->pmNext;
	}
	else
	{
		pmHead=pm2;
		pm2=pm2->pmNext;
	}
	pm=pmHead;							/* point to first entry */
	while(pm1&&pm2)						/* attach node with smaller key */
	{
		if((*pfCmp)(pm1,pm2)<=0)
		{
			pm=pm->pmNext=pm1;
			pm1=pm1->pmNext;
		}
		else
		{
			pm=pm->pmNext=pm2;
			pm2=pm2->pmNext;
		}
	}
	if(pm1)
		pm->pmNext=pm1;					/* attach remaining list */
	else
		pm->pmNext=pm2;
	EXIT("MergeSortedLists");
	return(pmHead);
}
