/* crchash.c

Copyright c, 2001 F. Bacchus

Log:

Name		Date	Description

M Ady		010215	First C version.

Notes:
	The hash routines in this module support hash tables for world
	signatures.  These hash tables have the following properties:

	1.  The hash value is the n lsb's of the crc.

	2.	The hash table size is a power of 2.

	3.	When the table size is expanded, it is doubled in size and
	the length of the hash value increases by one bit.

	4.	The table size is expanded when its average number of entries
	per bucket exceeds a threshold.

Usage:
	1.  Create a hash table by calling CRCHashCreate.  This routine will allocate
	the hash table from the heap, and fill in the initial hash table data.

	2.  Insert, delete and lookup entries in the hash table by calling
	CRCHashInsert, CRCHashDelete, and CRCHashSearch.  Call CRCHashInsert and 
	CRCHashDelete take care of counting the number of entries in the table, and
	CRCHashInsert expands the table when it's loading exceeds a threshold value.
	Call CRCHashCount to determine the maximum loading of any cell.
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
#include "crchash.h"
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

typedef struct CRCHashCell
{
	struct CRCHashCell *phNext;			// pointer to next
	LINEARPLANP plpWorld;				// pointer to linear plan
}CRCHASH, *CRCHASHP;

/* local data */

static CRCHASHP *pphHashTable;			// pointer to hash table
static int nHashSize;					// number of cells in table
static int nHashCount;					// total number of entries
static int nHashMax;					// maximum number of entries
static unsigned int nHashMask;			// hash function mask

/* local function prototypes */

static int CRCHashExpand(void);

/* CRCHashCreate

Description:
	Allocate and fill in a hash table.
Outputs:
	A pointer to the generated hash table.
	The length of the hash table in records.
Returned Status:
	HASH__SUCCESS
	HASH__NO_MEMORY
*/

int CRCHashCreate(void)
{
	ZONEP pz;

	ENTER("CRCHashCreate",FALSE);

	nHashSize=1<<CRC_MINHASHBITS;

	pz=pzCurrent;
	SetZone(&zPermanent);
	pphHashTable=(CRCHASHP *)MemAlloc(nHashSize*sizeof(CRCHASHP));
	SetZone(pz);
	if(!pphHashTable)
	{
		EXIT("CRCHashCreate");
		return(HASH__NO_MEMORY);
	}

	nHashCount=0;
	nHashMax=nHashSize*CRC_THRESHOLD;
	nHashMask=nHashSize-1;
	
	EXIT("CRCHashCreate");
	return(HASH__SUCCESS);
}

/* HashClear

Description:
	Clear the CRC hash table.
Returned Status:
	HASH__SUCCESS
*/

int CRCHashClear(void)
{
	ENTER("CRCHashClear",FALSE);
	memset(pphHashTable,0,nHashSize*sizeof(CRCHASHP));
	nHashCount=0;

	EXIT("CRCHashClear");
	return(HASH__SUCCESS);
}

/* CRCHashInsert

Description
	Insert a record into the CRC hash table.
Outputs:
	A pointer to the (available) hash record.
Returns Status:
	HASH__SUCCESS
	HASH__NO_MEMORY
*/

int CRCHashInsert
(
	LINEARPLANP plpWorld				/* world pointer */
)
{
	int nStatus;						// returned status
	unsigned int nHash;					// hash value
	CRCHASHP phHash;					// hash element pointer
	ZONEP pz;

	ENTER("CRCHashInsert",TRUE);

	// don't overfill hash table

	if(++nHashCount>nHashMax)
	{
		nStatus=CRCHashExpand();
		if(nStatus)
			return(nStatus);
	}

	// calculate hash function
	
	nHash=plpWorld->nSignature&nHashMask;

	// link world into hash table
	
	pz=pzCurrent;
	SetZone(&zPermanent);
	phHash=(CRCHASHP)MemAlloc(sizeof(CRCHASH));
	SetZone(pz);
	if(!phHash)
	{
		EXIT("CRCHashInsert");
		return(HASH__NO_MEMORY);
	}
	
	phHash->plpWorld=plpWorld;
	phHash->phNext=pphHashTable[nHash];		
	pphHashTable[nHash]=phHash;
	
	EXIT("CRCHashInsert");
	return(HASH__SUCCESS);
}

/* CRCHashSearch

Description
	Search for a matching world in the CRC hash table.
Outputs:
	A pointer to the world.
Returns Status:
	TRUE if the world matches another in the hash table
*/

LINEARPLANP CRCHashSearch
(
	LINEARPLANP plpWorld				/* world pointer */
)
{
	unsigned int nHash;					// hash value
	CRCHASHP phHash;					// hash element pointer

	// calculate hash function
	
	nHash=plpWorld->nSignature&nHashMask;

	// search for signature in hash chain

	for(phHash=pphHashTable[nHash];phHash;phHash=phHash->phNext)
	{
		if(plpWorld->nSignature==phHash->plpWorld->nSignature)
		{
			if((*pfPlanEqQ)(plpWorld,phHash->plpWorld))
			{
				if(nTrace>=3)
					CommandPrintf(pfTraceStream,"\nWorld %d is equivalent to world %d\n",plpWorld->nWorldNumber,phHash->plpWorld->nWorldNumber);
				EXIT("CRCHashSearch");
				return phHash->plpWorld;
			}
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

int CRCHashDelete
(
	LINEARPLANP plpWorld				/* world pointer */
)
{
	unsigned int nHash;					// hash value
	CRCHASHP phPrev;					// previous hash element pointer
	CRCHASHP phHash;					// hash element pointer

	ENTER("CRCHashDelete",TRUE);

	// calculate hash function
	
	nHash=plpWorld->nSignature&nHashMask;

	// search for world in hash chain

	phPrev=(CRCHASHP)(pphHashTable+nHash);
	for(phHash=pphHashTable[nHash];phHash;phHash=phHash->phNext)
	{
		if(plpWorld==phHash->plpWorld)
		{
			nHashCount--;
			phPrev->phNext=phHash->phNext;	// unlink world from hash table
			EXIT("CRCHashDelete");
			return(HASH__SUCCESS);
		}
		phPrev=phHash;					// pass the wand
	}
	EXIT("CRCHashDelete");
	return(HASH__NOT_FOUND);
}

/* CRCHashCount

Description:
	Count up the longest hash chain in the CRC hash table.
Returns:
	Length of longest hash chain in the CRC hash table.
*/

int CRCHashCount(void)
{
	int i;								/* loop index */
	int nLength;						// length of current hash chain
	int nMax;							// length of maximum hash chain
	CRCHASHP ph;						// hash pointer

	ENTER("CRCHashCount",TRUE);
	nMax=0;
	for(i=0;i<nHashSize;i++)			// for all hash buckets
	{
		nLength=0;
		for(ph=pphHashTable[i];ph;ph=ph->phNext)	// for all entries in bucket
			nLength++;
		if(nLength>nMax)
			nMax=nLength;
	}
	EXIT("CRCHashCount");
	return nMax;
}

/* CRCHashExpand

Description:
	Double the size of tha hash table.
*/

static int CRCHashExpand(void)
{
	int i;								// loop index
	CRCHASHP ph1,ph2;					// hash element pointers
	unsigned int nHash;					// hash value
	CRCHASHP *pph;						// new hash table pointer
	ZONEP pz;

	ENTER("CRCHashExpend",TRUE);

//	fprintf(stderr,"Expanding CRC hash table to %d cells (max chain %d)\n",
//		nHashSize*2,CRCHashCount());

	// allocate a new hash table, doubling the size

	pz=pzCurrent;
	SetZone(&zPermanent);
	pph=(CRCHASHP *)MemAlloc(2*nHashSize*sizeof(CRCHASHP));
	SetZone(pz);

	// copy the old hash table into the new, rehashing each entry
	
	nHashMask=(nHashMask<<1)+1;			// new hash mask

	for(i=0;i<nHashSize;i++)
	{
		for(ph1=pphHashTable[i];ph1;ph1=ph2)
		{
			ph2=ph1->phNext;			// pass the wand
			nHash=ph1->plpWorld->nSignature&nHashMask;	// new hash value
			ph1->phNext=pph[nHash];		// relink the entry
			pph[nHash]=ph1;
		}
	}

	// update hash table parameters
	
	pphHashTable=pph;
	nHashSize<<=1;
	nHashMax<<=1;
	
	EXIT("CRCHashExpand");
	return(HASH__SUCCESS);
}

/* MarkCRCHash

Description:
	Mark the contents of the CRC hash table for garbage collection.
*/

void MarkCRCHash(void)
{
	int i;								// loop index
	CRCHASHP ph;						// hash element pointer

	ENTER("MarkCRCHash",TRUE);

	if(pphHashTable)
	{
		ZoneMark(pphHashTable);

		for(i=0;i<nHashSize;i++)
		{
			for(ph=pphHashTable[i];ph;ph=ph->phNext)
				ZoneMark(ph);
		}
	}
	EXIT("MarkCRCHash");
}	
