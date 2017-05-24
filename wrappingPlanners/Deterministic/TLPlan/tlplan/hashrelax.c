/* crchash.c

Based on crchash.c code

*/

#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "tlplan.h"
#include "hashrelax.h"
#include "hash.h"
#include "iface.h"
#include "util.h"
#include "zone.h"

#include "eval.h"
#include "tlparse.h"
#include "world.h"

/* local definitions */

#define CRC_MINHASHBITS		10
#define CRC_THRESHOLD		4


typedef struct RelCRCHashCell
{
    struct RelCRCHashCell *phNext;		// pointer to next
    WORLDSUMP pWorldSummary;                    // a summary of the world
} RELCRCHASH, *RELCRCHASHP;

/* local data */

static RELCRCHASHP *pphRelHashTable;			// pointer to hash table
static int nRelHashSize;					// number of cells in table
static int nRelHashCount;					// total number of entries
static int nRelHashMax;					// maximum number of entries
static unsigned int nRelHashMask;			// hash function mask

/* local function prototypes */

static int RelCRCHashExpand(void);

/* RelCRCHashCreate

Description:
	Allocate and fill in a hash table.
Outputs:
	A pointer to the generated hash table.
	The length of the hash table in records.
Returned Status:
	HASH__SUCCESS
	HASH__NO_MEMORY
*/

int RelCRCHashCreate(void)
{
	ZONEP pz;

	ENTER("RelCRCHashCreate",FALSE);

	nRelHashSize=1<<CRC_MINHASHBITS;

	pz=pzCurrent;
	SetZone(&zPermanent);
	pphRelHashTable=(RELCRCHASHP *)MemAlloc(nRelHashSize*sizeof(RELCRCHASHP));
	SetZone(pz);
	if(!pphRelHashTable)
	{
		EXIT("CRCHashCreate");
		return(HASH__NO_MEMORY);
	}

	nRelHashCount=0;
	nRelHashMax=nRelHashSize*CRC_THRESHOLD;
	nRelHashMask=nRelHashSize-1;
	
	EXIT("CRCHashCreate");
	return(HASH__SUCCESS);
}

/* HashClear

Description:
	Clear the CRC hash table.
Returned Status:
	HASH__SUCCESS
*/

int RelCRCHashClear(void)
{
	ENTER("CRCHashClear",FALSE);
	memset(pphRelHashTable,0,nRelHashSize*sizeof(RELCRCHASHP));
	nRelHashCount=0;

	EXIT("CRCHashClear");
	return(HASH__SUCCESS);
}

/* RelCRCHashInsert

Description
	Insert a record into the RelCRC hash table.
Outputs:
	A pointer to the (available) hash record.
Returns Status:
	HASH__SUCCESS
	HASH__NO_MEMORY
*/

WORLDSUMP RelCRCHashInsert
(
    unsigned int worldCRC, 
    int nGoalsSatisfied,
    int nPreferencesSatisfied,
    double dMetric
)
{
	int nStatus;				// returned status
	unsigned int nHash;			// hash value
	RELCRCHASHP phHash;			// hash element pointer
	ZONEP pz;

	ENTER("RelCRCHashInsert",TRUE);

	// don't overfill hash table

	if(++nRelHashCount>nRelHashMax)
	{
		nStatus=RelCRCHashExpand();
//		if(nStatus) return nStatus;
		if(nStatus) return NULL;
	}

	// calculate hash function
	
	nHash=worldCRC&nRelHashMask;

	// link world into hash table
	
	pz=pzCurrent;
	SetZone(&zPermanent);
	phHash=(RELCRCHASHP)MemAlloc(sizeof(RELCRCHASH));
	SetZone(pz);
	if(!phHash)
	{
		EXIT("RelCRCHashInsert");
//		return(HASH__NO_MEMORY);
		return NULL;
	}

	SetZone(&zPermanent);
	phHash->pWorldSummary=(WORLDSUMP)MemAlloc(sizeof(WORLDSUM));
	SetZone(pz);
	phHash->pWorldSummary->worldCRC=worldCRC;
	phHash->pWorldSummary->nGoalsSatisfied=nGoalsSatisfied;
	phHash->pWorldSummary->nPreferencesSatisfied=nPreferencesSatisfied;
	phHash->pWorldSummary->dMetric=dMetric;
	phHash->phNext=pphRelHashTable[nHash];		
	pphRelHashTable[nHash]=phHash;
	
	EXIT("RelCRCHashInsert");
	return(phHash->pWorldSummary);
}


WORLDSUMP RelCRCHashInsert_Qual
(
    unsigned int worldCRC, 
    int nGoalsSatisfied,
    int nPreferenceValue
)
{
	int nStatus;				// returned status
	unsigned int nHash;			// hash value
	RELCRCHASHP phHash;			// hash element pointer
	ZONEP pz;

	ENTER("RelCRCHashInsert",TRUE);

	// don't overfill hash table

	if(++nRelHashCount>nRelHashMax)
	{
		nStatus=RelCRCHashExpand();
//		if(nStatus) return nStatus;
		if(nStatus) return NULL;
	}

	// calculate hash function
	
	nHash=worldCRC&nRelHashMask;

	// link world into hash table
	
	pz=pzCurrent;
	SetZone(&zPermanent);
	phHash=(RELCRCHASHP)MemAlloc(sizeof(RELCRCHASH));
	SetZone(pz);
	if(!phHash)
	{
		EXIT("RelCRCHashInsert");
//		return(HASH__NO_MEMORY);
		return NULL;
	}

	SetZone(&zPermanent);
	phHash->pWorldSummary=(WORLDSUMP)MemAlloc(sizeof(WORLDSUM));
	SetZone(pz);
	phHash->pWorldSummary->worldCRC=worldCRC;
	phHash->pWorldSummary->nGoalsSatisfied=nGoalsSatisfied;
	phHash->pWorldSummary->nPreferenceValue=nPreferenceValue;
	phHash->phNext=pphRelHashTable[nHash];		
	pphRelHashTable[nHash]=phHash;
	
	EXIT("RelCRCHashInsert");
	return(phHash->pWorldSummary);
}



/* RelCRCHashSearch

Description
	Search for a matching world in the RelCRC hash table.
Outputs:
	A pointer to the Hash cell
Returns Status:
	TRUE if the world matches another in the hash table
*/

WORLDSUMP RelCRCHashSearch
(
    unsigned int worldCRC			/* CRC of world searched  */
)
{
    unsigned int nHash;					// hash value
    RELCRCHASHP phHash;					// hash element pointer
    
    // calculate hash function
    
    nHash=worldCRC&nRelHashMask;
    
    // search for signature in hash chain
    
    for(phHash=pphRelHashTable[nHash];phHash;phHash=phHash->phNext)
    {
	if(worldCRC==phHash->pWorldSummary->worldCRC)
	{
	    return phHash->pWorldSummary;  // we return the world summary
	}
    }
    EXIT("CRCHashSearch");
    return NULL;
}

/* CRCHashDelete

Description
	Delete a record from the CRC hash table.
Outputs:
	Nothing.
Returns Status:
	HASH__SUCCESS
	HASH__NOT_FOUND
*/

/* we don't need a delete, commenting this function */

/* int RelCRCHashDelete */
/* ( */
/* 	LINEARPLANP plpWorld				/\* world pointer *\/ */
/* ) */
/* { */
/* 	unsigned int nHash;					// hash value */
/* 	CRCHASHP phPrev;					// previous hash element pointer */
/* 	CRCHASHP phHash;					// hash element pointer */

/* 	ENTER("CRCHashDelete",TRUE); */

/* 	// calculate hash function */
	
/* 	nHash=plpWorld->nSignature&nRelHashMask; */

/* 	// search for world in hash chain */

/* 	phPrev=(CRCHASHP)(pphRelHashTable+nHash); */
/* 	for(phHash=pphRelHashTable[nHash];phHash;phHash=phHash->phNext) */
/* 	{ */
/* 		if(plpWorld==phHash->plpWorld) */
/* 		{ */
/* 			nRelHashCount--; */
/* 			phPrev->phNext=phHash->phNext;	// unlink world from hash table */
/* 			EXIT("CRCHashDelete"); */
/* 			return(HASH__SUCCESS); */
/* 		} */
/* 		phPrev=phHash;					// pass the wand */
/* 	} */
/* 	EXIT("CRCHashDelete"); */
/* 	return(HASH__NOT_FOUND); */
/* } */

/* RelCRCHashCount

Description:
	Count up the longest hash chain in the CRC hash table.
Returns:
	Length of longest hash chain in the CRC hash table.
*/

int RelCRCHashCount(void)
{
	int i;								/* loop index */
	int nLength;						// length of current hash chain
	int nMax;							// length of maximum hash chain
	RELCRCHASHP ph;						// hash pointer

	ENTER("RelCRCHashCount",TRUE);
	nMax=0;
	for(i=0;i<nRelHashSize;i++)			// for all hash buckets
	{
		nLength=0;
		for(ph=pphRelHashTable[i];ph;ph=ph->phNext)	// for all entries in bucket
			nLength++;
		if(nLength>nMax)
			nMax=nLength;
	}
	EXIT("RelCRCHashCount");
	return nMax;
}

/* CRCHashExpand

Description:
	Double the size of tha hash table.
*/

static int RelCRCHashExpand(void)
{
	int i;								// loop index
	RELCRCHASHP ph1,ph2;					// hash element pointers
	unsigned int nHash;					// hash value
	RELCRCHASHP *pph;						// new hash table pointer
	ZONEP pz;

	ENTER("CRCHashExpend",TRUE);

//	fprintf(stderr,"Expanding CRC hash table to %d cells (max chain %d)\n",
//		nRelHashSize*2,CRCHashCount());

	// allocate a new hash table, doubling the size

	pz=pzCurrent;
	SetZone(&zPermanent);
	pph=(RELCRCHASHP *)MemAlloc(2*nRelHashSize*sizeof(RELCRCHASHP));
	SetZone(pz);

	// copy the old hash table into the new, rehashing each entry
	
	nRelHashMask=(nRelHashMask<<1)+1;			// new hash mask

	for(i=0;i<nRelHashSize;i++)
	{
		for(ph1=pphRelHashTable[i];ph1;ph1=ph2)
		{
			ph2=ph1->phNext;			// pass the wand
			nHash=ph1->pWorldSummary->worldCRC&nRelHashMask;	// new hash value
			ph1->phNext=pph[nHash];		// relink the entry
			pph[nHash]=ph1;
		}
	}

	// update hash table parameters
	
	pphRelHashTable=pph;
	nRelHashSize<<=1;
	nRelHashMax<<=1;
	
	EXIT("CRCHashExpand");
	return(HASH__SUCCESS);
}

/* MarkCRCHash

Description:
	Mark the contents of the CRC hash table for garbage collection.
*/

void RelMarkCRCHash(void)
{
	int i;								// loop index
	RELCRCHASHP ph;						// hash element pointer

	ENTER("MarkCRCHash",TRUE);

	if(pphRelHashTable)
	{
		ZoneMark(pphRelHashTable);

		for(i=0;i<nRelHashSize;i++)
		{
		    for(ph=pphRelHashTable[i];ph;ph=ph->phNext){
			ZoneMark(ph);
			ZoneMark(ph->pWorldSummary);
		    }
		}
	}
	EXIT("MarkCRCHash");
}	
