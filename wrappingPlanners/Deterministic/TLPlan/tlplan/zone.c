// in zone clear, we should deallocate the memory we clear!

/* zone.c -- zoned memory allocation routines.

Copyright C, 1997 - 2001  F. Bacchus


Description:
	This module implements zoned memory allocation routines, using next fit.
	Two zones are supported... one is for (semi)permanent allocations, and
	the other is for scratch work.  Both zones are implemented identically,
	but the latter tends to be cleared more often.

	Memory is allocated to zones in large chunks called master blocks.
	When a zone is cleared, it is coalesced by simply resetting the free list.

	When memory is freed, it is simply added to the free list.  Memory is
	coalesced during allocation by checking for next adjacent free blocks
	and merging them.

	Free memory is maintained in a doubly linked circular buffer.
	The implementation of next fit involves keeping a pointer to the next
	available memory block after each allocation, and using that pointer to
	commence the search for new memory on the next allocation.

	If the search for a block of memory runs into the previous scan's pointer,
	no block of memory is large enough, so we extend the zone.  To extend
	memory, a new master block is is obtained and linked to the free list.
	The next pointer is pointed at the new memory.
*/

#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "iface.h"
#include "search.h"
#include "tlparse.h"
#include "util.h"
#include "world.h"
#include "zone.h"

/* local structures and definitions */

#define MASK		0x0000001F			/* block size mask (minimum block size is 0x10) */
#define MINSIZE		(MASK+1)			/* minimum block size */
#define SHIFT		4					/* block size shift */
#define GRANULARITY	0x00010000			/* windows reserves memory, 64 k at a time */
#define ALLOCATION	(4*GRANULARITY)		/* obtain memory in large increments */

#define ROUNDUP(x,y) (((x)+(y)-1)/(y)*(y))

//#define ZONE_DEBUG	1					/* to enable memory allocation checking */

#ifdef _WIN32
#define VMALLOC VMalloc
#define VFREE VFree
#else // _WIN32
#define VMALLOC malloc
#define VFREE free
#endif // _WIN32

/* global data */

ZONE zScratch;							/* scratch data zone */
ZONE zPermanent;						/* permanent data zone */
ZONEP pzCurrent;						/* current zone */
int nZoneLimit;							/* zone memory limit */

/* local data */

static int nReserved;					/* reserved memory tag */

/* local function prototypes */

static BOOL ExtendZone
(
	ZONEP pzZone,						/* zone to extend */
	int nLength							/* minimum length */
);
#ifdef _WIN32
static void *VMalloc
(
	DWORD dwSize						/* size of block to allocate */ 
);
static void VFree
(
	void *pMemory						/* pointer to block to free */ 
);
#endif // _WIN32

//static void Histogram
//(
//	int nTable,
//	int nLength 
//);
//static void PrintHistogram(void);

/* ZoneAlloc

Description:
	Allocate a block of memory from the current zone.
*/

void *ZoneAlloc
(
	int nLength							/* size of block to allocate */
)
{
	MEMORYP pm,pm1;						/* memory pointers */
	int k;								/* block excess */
	int nFlag;							/* second pass flag */

	StartTimer(tsTimers.adfMemoryAllocation);

#ifdef ZONE_DEBUG
	pzCurrent->nAllocations++;
	if(!nLength)						/* don't honor request for no memory */
	{
		CommandPrintf(stderr,"Attempt to allocate 0 length memory\n");
		return NULL;
	}
#endif /* ZONE_DEBUG */
	nLength=(nLength+offsetof(MEMORY,pmNext)+MASK)&~MASK;	/* round up allocation */
	pzCurrent->nTotal+=nLength;

	for(;;)
	{
		nFlag=TRUE;
		for(pm=pzCurrent->pmRover;;pm=pm->pmNext)
		{
			if(pm==pzCurrent->pmRover)
			{
				if(!nFlag)
					break;
				nFlag=FALSE;
			}
#ifdef ZONE_DEBUG
			if(pm->nTag!=FREE)
			{
				if(pm!=(MEMORYP)pzCurrent)
					CommandPrintf(stderr,"Reserved or corrupted block in free list: %08X\n",
						pm->nTag);
			}
#endif /* ZONE_DEBUG */

			/* attempt to coalesce with next block */

			pm1=(MEMORYP)((char *)pm+pm->nLength);
			if(pm1->nTag==FREE)
			{
				pm1->pmNext->pmPrev=pm1->pmPrev;	/* unlink second block */
				pm1->pmPrev->pmNext=pm1->pmNext;
				pm->nLength+=pm1->nLength;
				if(pzCurrent->pmRover==pm1)
					pzCurrent->pmRover=pm;
			}
#ifdef ZONE_DEBUG
			else if(pm1->nTag!=nReserved&&pm1->nTag!=RELOCATED)
				CommandPrintf(stderr,"Corrupt block abutts free block\n");
#endif /* ZONE_DEBUG */

			/* attempt to split or allocate this block */

			k=pm->nLength-nLength;		/* calculate remainder */
			if(k>=(int)sizeof(MEMORY))						/* if block is too big */
			{
				pzCurrent->pmRover=pm;

				pm->nLength=k;			/* shorten current block */
				pm1=(MEMORYP)((char *)pm+k);	/* allocate memory at end of block */
				pm1->nTag=nReserved;
				pm1->nLength=nLength;
				StopTimer(tsTimers.adfMemoryAllocation);
				return (void *)&pm1->pmNext;
			}
			if(!k)						/* if block is right size */
			{
				pzCurrent->pmRover=pm->pmNext;

				pm->pmPrev->pmNext=pm->pmNext;	/* unlink block */
				pm->pmNext->pmPrev=pm->pmPrev;
				pm->nTag=nReserved;
				StopTimer(tsTimers.adfMemoryAllocation);
				return (void *)&pm->pmNext;
			}
		}

		/* no block was large enough, allocate more memory */

		if(!ExtendZone(pzCurrent,nLength))
		{
			StopTimer(tsTimers.adfMemoryAllocation);
			return NULL;
		}
	}
}

/* ZoneFree

Description:
	Deallocate a block of memory from the current zone.
*/

DECLSPEC void ZoneFree
(
	void *pvBlock						/* pointer to memory to free */
)
{
	MEMORYP pm;

#ifdef ZONE_DEBUG
	if(!pvBlock)
	{
		CommandPrintf(stderr,"Freeing memory at location 0\n");
		return;
	}
#endif

	pm=(MEMORYP)((char *)pvBlock-offsetof(MEMORY,pmNext));
#ifdef ZONE_DEBUG
	/* verify that block hasn't been clobbered */

	if((pm->nTag!=nReserved&&pm->nTag!=RELOCATED)||pm->nLength&0x0000000F)
	{
		CommandPrintf(stderr,"Freeing invalid or corrupted memory %08X\n",pvBlock);
		return;
	}

	/* verify this block is owned by this zone */

	{
		ALLOCP pa;

		for(pa=pzCurrent->paMaster;pa;pa=pa->paNext)
		{
			if(pm>=pa->pmAddress&&(char *)pm<(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY))
				break;
		}
		if(!pa)
		{
			CommandPrintf(stderr,"Attempt to deallocate memory from wrong zone.\n");
			return;
		}
	}
	pzCurrent->nFrees++;
	memset(pvBlock,0xFF,pm->nLength-offsetof(MEMORY,pmNext));
#endif /* ZONE_DEBUG */
	pm->nTag=FREE;
	pm->pmNext=pzCurrent->pmNext;		/* link block to free list */
	pm->pmPrev=(MEMORYP)pzCurrent;
	pzCurrent->pmNext->pmPrev=pm;
	pzCurrent->pmNext=pm;
}

/* ExtendZone

Description:
	Add another block of memory to the master list.
*/

static BOOL ExtendZone
(
	ZONEP pzZone,						/* zone to extend */
	int nLength							/* minimum size of zone */
)
{
	ALLOCP pa;							/* master allocation block pointer */
	MEMORYP pm;
	MEMORYP pm1;

	nLength+=sizeof(MEMORY);
	nLength=((nLength+ALLOCATION-1)/ALLOCATION)*ALLOCATION;
	pa=(ALLOCP)malloc(sizeof(ALLOC));
	if(!pa)								/* failed to allocate memory */
		return FALSE;
	pa->nLength=nLength;
	pm=pa->pmAddress=(MEMORYP)VMALLOC(pa->nLength);
	if(!pm)								/* failed to allocate memory */
	{
		free(pa);
		return FALSE;
	}
	assert(sizeof(MEMORY)<=MINSIZE);
	pm1=(MEMORYP)((char *)pa->pmAddress+nLength-sizeof(MEMORY));
	pm1->nTag=nReserved;				/* don't coalesce beyond end of block */
	pm1->nLength=sizeof(MEMORY);
	pm1->pmNext=pm1->pmPrev=NULL;

	pm->nTag=FREE;
	pm->nLength=pa->nLength-sizeof(MEMORY);
	pm->pmNext=pzCurrent->pmNext;		/* link block to free list */
	pm->pmPrev=(MEMORYP)pzCurrent;
	pzCurrent->pmNext->pmPrev=pm;
	pzCurrent->pmNext=pm;
	pzCurrent->pmRover=pm;

	pa->paPrev=NULL;					/* update master list */
	pa->paNext=pzZone->paMaster;
	pa->paNext->paPrev=pa;
	pzZone->paMaster=pa;
	return TRUE;
}

/* InitZone

Description:
	Do initial setup on a zone.
*/

BOOL InitZone
(
	ZONEP pzZone						/* zone to initialize */
)
{
	ALLOCP pa;							/* master allocation block pointer */
	MEMORYP pm;
	MEMORYP pm1;
	int nReserved;

	nReserved=pzZone==&zPermanent?RESERVED_P:RESERVED_S;

	if(!pzZone->paMaster)
	{
		pa=(ALLOCP)malloc(sizeof(ALLOC));
		if(!pa)							/* failed to allocate memory */
			return FALSE;
		pa->paPrev=pa->paNext=NULL;
		pa->nLength=ALLOCATION;
		pm=pa->pmAddress=(MEMORYP)VMALLOC(pa->nLength);
		if(!pm)							/* failed to allocate memory */
		{
			free(pa);
			return FALSE;
		}
		pm1=(MEMORYP)((char *)pa->pmAddress+ALLOCATION-sizeof(MEMORY));
		pm1->nTag=nReserved;			/* don't coalesce beyond end of block */
		pm1->nLength=sizeof(MEMORY);
		pm1->pmNext=pm1->pmPrev=NULL;

		pm->nTag=FREE;					/* initialize free memory block */
		pm->nLength=pa->nLength-sizeof(MEMORY);
		pm->pmPrev=pm->pmNext=(MEMORYP)pzZone;

		pzZone->paMaster=pa;			/* initialize zone */
		pzZone->nTag=nReserved;
		pzZone->nLength=0;
		pzZone->pmNext=pzZone->pmPrev=pm;
	pzZone->pmRover=pm;
#ifdef ZONE_DEBUG
		pzZone->nAllocations=0;
		pzZone->nFrees=0;
#endif /* ZONE_DEBUG */
		pzZone->nTotal=0;
	}
	return TRUE;
}

/* ZoneClear

Description:
	Free all the memory in a zone, by resetting it.
*/

void ZoneClear
(
	ZONEP pzZone						/* zone to clear */
)
{
	ALLOCP pa;
	MEMORYP pm;

#ifdef ZONE_DEBUG
	int nReserved;

	nReserved=pzZone==&zPermanent?RESERVED_P:RESERVED_S;

	/* check each master block for corruption */

	for(pa=pzZone->paMaster;pa;pa=pa->paNext)
	{
		for(pm=pa->pmAddress;
			(char *)pm<(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY);
			pm=(MEMORYP)((char *)pm+pm->nLength))
		{
			if(pm->nTag!=FREE&&pm->nTag!=nReserved&&pm->nTag!=RELOCATED)
				CommandPrintf(stderr,"Corrupted master block at %p, tag %08X\n",
					pm,pm->nTag);
		}
		if((char *)pm!=(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY)||
			pm->nTag!=nReserved)
			CommandPrintf(stderr,"Corrupted master block end tag at %p, tag %08X\n",
				pm,pm->nTag);
	}
	if(pzZone->nTag!=nReserved||pzZone->nLength!=0)
		CommandPrintf(stderr,"Zone block Corrupted\n");
#endif /* ZONE_DEBUG */

	pzZone->pmNext=pzZone->pmPrev=(MEMORYP)pzZone;
#ifdef ZONE_DEBUG
	pzZone->nAllocations=0;
	pzZone->nFrees=0;
#endif /* ZONE_DEBUG */
	pzZone->nTotal=0;
	for(pa=pzZone->paMaster;pa;pa=pa->paNext)
	{
		pm=pa->pmAddress;
		pm->nTag=FREE;					/* initialize free memory block */
		pm->nLength=pa->nLength-sizeof(MEMORY);
		pm->pmNext=pzZone->pmNext; 		/* link block to free list */
		pm->pmPrev=(MEMORYP)pzZone;
		pzZone->pmNext->pmPrev=pm;
		pzZone->pmNext=pm;
#ifdef ZONE_DEBUG
		memset((char *)pm+sizeof(MEMORY),0xFF,pm->nLength-sizeof(MEMORY));
#endif /* ZONE_DEBUG */
	}
	pzZone->pmRover=pzZone->pmNext;
}

/* DeallocateZone

Description:
	Free all the memory in a zone, by releasing it.
*/

//void DeallocateZone
//(
//	ZONEP pzZone						/* zone to deallocate */
//)
//{
//	ALLOCP pa,pa1;
//#ifdef ZONE_DEBUG
//	MEMORYP pm;
//
//	/* check each master block for corruption */
//
//	for(pa=pzCurrent->paMaster;pa;pa=pa->paNext)
//	{
//		for(pm=pa->pmAddress;
//			(char *)pm<(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY);
//			pm=(MEMORYP)((char *)pm+pm->nLength))
//		{
//			if(pm->nTag!=FREE&&pm->nTag!=nReserved)
//				CommandPrintf(stderr,"Corrupted master block at %p, tag %08X\n",
//					pm,pm->nTag);
//		}
//		if((char *)pm!=(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY)||
//			pm->nTag!=nReserved)
//			CommandPrintf(stderr,"Corrupted master block end tag at %p, tag %08X\n",
//				pm,pm->nTag);
//	}
//	if(pzZone->nTag!=nReserved||pzZone->nLength!=0)
//		CommandPrintf(stderr,"Zone block Corrupted\n");
//#endif /* ZONE_DEBUG */
//
//	for(pa=pzZone->paMaster;pa;pa=pa1)
//	{
//		pa1=pa->paNext;
//		free(pa);
//	}
//	pzZone->pmNext=NULL;
//	pzZone->pmPrev=NULL;
//	pzZone->pmRover=NULL;
//	pzZone->paMaster=NULL;
//#ifdef ZONE_DEBUG
//	pzZone->nAllocations=0;
//	pzZone->nFrees=0;
//#endif /* ZONE_DEBUG */
//	pzZone->nTotal=0;
//}

/* SetZone

Description:
	Select the current zone.
	Initialize the zone if it hasn't been initialized yet.
*/

void SetZone
(
	ZONEP pzZone						/* zone to set current */
)
{
	pzCurrent=pzZone;
	nReserved=pzZone==&zPermanent?RESERVED_P:RESERVED_S;
}

/* ZoneStatistics

Description:
	Report zone usage statistics.
*/

void ZoneStatistics
(
	FILE *pfOutput,						/* output file stream */
	ZONEP pzZone						/* zone to report */
)
{
	int nFree,nFreeSize;
	int nMaster,nMasterSize;
	MEMORYP pm;
	ALLOCP pa;

	nFree=nFreeSize=0;
	for(pm=pzZone->pmNext;pm!=(MEMORYP)pzZone;pm=pm->pmNext)
	{
		nFree++;
		nFreeSize+=pm->nLength;
	}
	nMaster=nMasterSize=0;
	for(pa=pzZone->paMaster;pa;pa=pa->paNext)
	{
		nMaster++;
		nMasterSize+=pa->nLength;
	}
	CommandPrintf(pfOutput,"%s Zone Statistics:\n",(pzZone==&zScratch)?"Scratch":"Permanent");
	CommandPrintf(pfOutput,"Total %d blocks allocated, %d freed %d total memory\n",
		pzZone->nAllocations,pzZone->nFrees,pzZone->nTotal);
	CommandPrintf(pfOutput,"%d blocks free, total size %d\n",nFree,nFreeSize);
	CommandPrintf(pfOutput,"%d blocks master, total size %d\n",nMaster,nMasterSize);
	CommandPrintf(pfOutput,"%d blocks outstanding, total size %d\n",
		pzZone->nAllocations-pzZone->nFrees,
		nMasterSize-nFreeSize-nMaster*sizeof(MEMORY));
}

/* ZoneCollect

Description:
	Scan a zone, freeing all unmarked memory, and unmarking all marked memory.
Notes:
	This is the workhorse routine for garbage collection.
*/

void ZoneCollect
(
	ZONEP pzZone						/* zone to scan */
)
{
	ALLOCP pa,pa1;						/* master block pointers */
	MEMORYP pm;							/* memory block pointer */
	ZONEP pzSaved;						/* saved zone pointer */
	BOOL bUsed;							/* block has reserved memory */
//	int nCount;

	pzSaved=pzCurrent;					/* save current zone */
	pzCurrent=pzZone;					/* set working zone */

//	nCount=0;
	pzCurrent->nTotal=0;
	for(pa=pzZone->paMaster;pa;pa=pa1)
	{
		pa1=pa->paNext;
		bUsed=FALSE;
		for(pm=pa->pmAddress;
			(char *)pm<(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY);
			pm=(MEMORYP)((char *)pm+pm->nLength))
		{
			switch(pm->nTag)
			{
				case RESERVED_P:		/* free unmarked, reserved memory */
				case RESERVED_S:
				case RELOCATED:
//					Histogram(1,pm->nLength);
					ZoneFree(&pm->pmNext);
					break;
				case MARKED:			/* unmark marked memory */
//					Histogram(0,pm->nLength);
					pm->nTag=nReserved;
					bUsed=TRUE;
					pzCurrent->nTotal+=pm->nLength;
					break;
#ifdef ZONE_DEBUG
				case FREE:
//					Histogram(1,pm->nLength);
					break;
				default:
					CommandPrintf(stderr,"Block with bad tag at %p, tag %08X\n",
						pm,pm->nTag);
#endif /* ZONE_DEBUG */
			}
		}
#ifdef ZONE_DEBUG
		if((char *)pm!=(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY))
			CommandPrintf(stderr,"Bad block length in zoned memory\n");
		else
		{
			if(pm->nTag!=nReserved||pm->nLength!=sizeof(MEMORY)||
				pm->pmNext||pm->pmPrev)
				CommandPrintf(stderr,"End of zone marker corrupt.\n");
		}
#endif /* ZONE_DEBUG */

		/* deallocate unused memory blocks */
		
		if(!bUsed&&(pa->paNext||pa->paPrev))	/* don't deallocate all memory! */
		{
			/* remove all free blocks from the free list */
			
			for(pm=pa->pmAddress;
				(char *)pm<(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY);
				pm=(MEMORYP)((char *)pm+pm->nLength))
			{
				if(pm->nTag==FREE)
				{
					if(pzCurrent->pmRover==pm)
						pzCurrent->pmRover=pm->pmNext;
					pm->pmNext->pmPrev=pm->pmPrev;
					pm->pmPrev->pmNext=pm->pmNext;
				}
			}
			
			/* deallocate the master block and the memory block */			
			
			if(pa->paPrev)
				pa->paPrev->paNext=pa->paNext;
			else
				pzCurrent->paMaster=pa->paNext;
			if(pa->paNext)
				pa->paNext->paPrev=pa->paPrev;
			VFREE(pa->pmAddress);
			free(pa);
//			nCount++;
		}	
	}
//	if(nCount)
//		Message("\n Freed %d memory blocks\n",nCount);

//	PrintHistogram();
	pzCurrent=pzSaved;					/* restore saved zone */
}

/* ZoneDelta

Description:
	Report the amount of memory used since the last call.
*/

void ZoneDelta
(
	int *pnRunningTotal,
	char *psString
)
{
#ifdef ZONE_DEBUG
	int nDelta;

	nDelta=pzCurrent->nTotal-*pnRunningTotal;
	*pnRunningTotal=pzCurrent->nTotal;
	fprintf(stderr,"%s %d\n",psString,nDelta);
#endif // ZONE_DEBUG
}

/* ZoneCopy

Description:
	Copy a block of allocated memory to the permanent zone.
	If the memory is already in the permanent zone, it isn't copied.
	The header of the block is marked for relocation.
*/

void *ZoneCopy
(
	void *pv1							// pointer to old block
)
{
	MEMORYP pm1;						// pointer to old memory
	MEMORYP pm2;						// pointer to new memory
	void *pv2;							// pointer to new block
	int nSize;							// amount of memory to allocate

#ifdef ZONE_DEBUG
	/* can't copy memory from location 0 */

	if(!pv1)
	{
		CommandPrintf(stderr,"Copying memory at location 0\n");
		return NULL;
	}

	/* verify that the permanent zone is current */

	if(pzCurrent!=&zPermanent)
		CommandPrintf(stderr,"Copying memory to non-permanent zone.\n");
#endif

	pm1=(MEMORYP)((char *)pv1-offsetof(MEMORY,pmNext));
#ifdef ZONE_DEBUG
	/* verify that block hasn't been clobbered */

	if((pm1->nTag!=RESERVED_P&&pm1->nTag!=RESERVED_S&&pm1->nTag!=RELOCATED)||
		pm1->nLength&0x0000000F)
	{
		CommandPrintf(stderr,"Copying invalid or corrupted memory %08X\n",pv1);
		return NULL;
	}
#endif /* ZONE_DEBUG */

	/* if memory already copied, don't do it again */

	if(pm1->nTag==RELOCATED)
		return (void *)pm1->pmNext;
	
	/* verify this block is owned by scratch zone */

	if(pm1->nTag!=RESERVED_S)			/* if not in scratch zone */
		return pv1;

	/* copy memory and mark the old memory */
	
	nSize=pm1->nLength-offsetof(MEMORY,pmNext);
	pv2=ZoneAlloc(nSize);
	if(!pv2)
		ErrorExit("Failed to allocate %d bytes of memory, exiting.\n",nSize);
	pm2=(MEMORYP)((char *)pv2-offsetof(MEMORY,pmNext));
	memcpy(pv2,pv1,nSize);
	pm1->nTag=RELOCATED;
	pm1->pmNext=(MEMORYP)pv2;
	return pv2;
}

/* ZoneReloc

Description:
	Relocate a pointer, if it points to a block of relocated memory.
*/

void ZoneReloc
(
	void **ppv							/* pointer to relocate */ 
)
{
	MEMORYP pm;							/* pointer to memory */

#ifdef ZONE_DEBUG
	/* can't reloate memory at location 0 */

	if(!ppv||!*ppv)
	{
		CommandPrintf(stderr,"Relocating memory at location 0\n");
		return;
	}
#endif

	pm=(MEMORYP)((char *)*ppv-offsetof(MEMORY,pmNext));
#ifdef ZONE_DEBUG
	/* verify that block hasn't been clobbered */

	if((pm->nTag!=nReserved&&pm->nTag!=RELOCATED)||pm->nLength&0x0000000F)
	{
		CommandPrintf(stderr,"Relocating invalid or corrupted memory %08X\n",*ppv);
		return;
	}
#endif /* ZONE_DEBUG */

	if(pm->nTag==RELOCATED)
		*ppv=(void *)pm->pmNext;
#ifdef ZONE_DEBUG
	/* verify relocated pointer is owned by permanent zone */

	{
		ALLOCP pa;

		for(pa=zPermanent.paMaster;pa;pa=pa->paNext)
		{
			if((MEMORYP)*ppv>=pa->pmAddress&&
				(char *)*ppv<(char *)pa->pmAddress+pa->nLength-sizeof(MEMORY))
				break;
		}
		if(!pa)
			CommandPrintf(stderr,"Relocating to memory in non-permanent zone.\n");
	}
#endif /* ZONE_DEBUG */
}

#ifdef _WIN32

/* VMalloc

Description:
	Allocate and commit a block of virtual memory.
*/

static void *VMalloc
(
	DWORD dwSize							/* size of memory to allocate */ 
)
{
	void *pv;

	if((dwSize&(GRANULARITY-1))!=0)
		ErrorMessage("Allocating odd-ball memory block\n");
	pv=VirtualAlloc(0,dwSize,MEM_COMMIT,PAGE_READWRITE); 
	if(!pv)
        ErrorMessage("Failed to allocate virtual memory\n");
	return pv;
}

/* VFree

Description:
	Deallocate and decommit a block of virtual memory.
*/

static void VFree
(
	void *pMemory						/* pointer to block to free */ 
)
{
	if(!VirtualFree(pMemory,0,MEM_RELEASE))
		ErrorMessage("Failed to release virtual memory\n");
}

#endif // _WIN32

//static int anHistogram[2][10];

//static void Histogram
//(
//	int nTable,
//	int nLength 
//)
//{
//	int n;
//	
//	n=nLength/MINSIZE;
//	if(n>=10)
//		n=0;
//	anHistogram[nTable][n]++;
//}

//static void PrintHistogram(void)
//{
//	int i;
//
//	fprintf(stderr,"\n");
//	for(i=1;i<10;i++)
//		fprintf(stderr," %d0: %8d (%9d bytes) %8d (%9d bytes)\n",i,
//			anHistogram[0][i],anHistogram[0][i]*i*16,anHistogram[1][i],anHistogram[1][i]*i*16);
//	fprintf(stderr," A0+ %8d                   %8d\n",anHistogram[0][0],anHistogram[1][0]);
//	for(i=0;i<10;i++)
//	{
//		anHistogram[0][i]=0;
//		anHistogram[1][i]=0;
//	}
//}
