/* hash.c

Copyright c, 1997 - 2001 F. Bacchus

Log:

Name		Date	Description

M Ady		970108	First C version.

Notes:
	The hash routines in this module support binless hash tables
	with collision handling. The following assumptions are made:

	1.  The first field of the hash record must be a pointer to
	the hash key.  The hash key itself must be a null teriminated string.

	2.  All hash tables have a prime number of entries.
	We do this to guarantee that all collision rings are
	the size of the table.  (If the table length is non-prime,
	then hash values which are divisors of the table length
	will generate collision rings of length=count/hash.)

	The sentinel value for an empty entry is a null key pointer.
	The sentinel value for a deleted key is a key pointer of -1.

	3.  For efficiency reasons, we do not deal with case independence here.
	Case independent strings should be converted to lowercase before inserting 
	into a hash table and they should be converted to lowercase before lookup.

Usage:

	1.  First define a structure for a symbol table entry.  The first element
	of the structure must be a pointer to the lookup key string.

	2.  Create a hash table by calling HashCreate.  This routine will allocate
	the hash table from the heap, and return a pointer to the table and a
	count of the number of records in the table.  These two parameters are
	required to call the other hash routines.

	HashCreate adds 20% to the specified minimum number of hash records and
	then selects the next prime number for the hash table size.  This ensures
	that the record count is prime, and that access to the hash table
	will remain efficient (typically no worse than a single colision on
	average).  The symbol table should be enlarged if it gets more full than
	the specified minimum number of entries.

	3.  Insert, delete and lookup entries in the hash table by calling
	HashInsert, HashDelete, and HashSearch.  Call HashCount to determine the
	hash table loading (when all hash table operations have been completed).

Analysis:
	Let f be the fraction of the table that is full. The chance that an entry 
	requires just one probe to insert is 1-f
	The chance that 2 probes are required is f(1-f)
	The chance that 3 probes are required is f^2(1-f)
	In general the chance that k probes are required is f^(k-1)(1-f)
	So the average number of probes expected is
	(1-f) + 2f(1-f) + 3f^2(1-f) + ... = 1/(1-f)

	Suppose the table has grown to fraction F full. The average lookup (or 
	insertion time) for a single entry is the average of 1/(1-f) as f varies 
	from 0 to F. This is found by integration and is

	-(ln(1-F))/F

	When F=0.5 this is 1.39 probes
	When F=0.8 this is 2.01 probes
	When F=0.9 this is 2.56 probes
	When F=0.95 this is 3.15 probes
*/

#include <ctype.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "tlplan.h"
#include "hash.h"
#include "tlparse.h"
#include "util.h"
#include "zone.h"

const int anFibonacci[16]=				/* table of 24-bit fibonacci coefficients */
{
	0x00834271L,						/* 0 */
	0x00915E23L,						/* 1 */
	0x00D85447L,						/* 2 */
	0x00F71B95L,						/* 3 */
	0x00A19325L,						/* 4 */
	0x00A04C01L,						/* 5 */
	0x00E78139L,						/* 6 */
	0x00810A49L,						/* 7 */
	0x00BB1F71L,						/* 8 */
	0x00FE19E3L,						/* 9 */
	0x00C0AB43L,						/* 10 */
	0x0091A8EBL,						/* 11 */
	0x00C23AE9L,						/* 12 */
	0x00BA33E5L,						/* 13 */
	0x00ED3259L,						/* 14 */
	0x00DD103DL							/* 15 */
};

/* local function prototypes */

static int Hash
(
	int nCount,							/* number of records in hash table */
	char *psKey							/* key to hash */
);
static int ReHash
(
	int nCount,							/* number of records in hash table */
	char *psKey							/* key to hash */
);
static int PrimeGen
(
	int nCount							/* initial value */
);

/* HashError

Description:
	Print an error message for the given hash error code.
*/

void HashError
(
	FILE *pf,							/* output file stream */
	int nCode							/* error code */
)
{
	ENTER("HashError",TRUE);
	switch(nCode)
	{
		case HASH__SUCCESS:
			break;
		case HASH__TABLE_FULL:
			CommandPrintf(pf,"Hash Error: Hash table full\n");
			break;
		case HASH__DUPLICATE_KEY:
			CommandPrintf(pf,"Hash Error: Duplicate key found\n");
			break;
		case HASH__NOT_FOUND:
			CommandPrintf(pf,"Hash Error: Key not found\n");
			break;
		case HASH__NO_MEMORY:
			CommandPrintf(pf,"Hash Error: Failed to allocate memory\n");
			break;
		default:
			CommandPrintf(pf,"Unknown hash error: %d\n",nCode);
	}
	EXIT("HashError");
}

/* HashCreate

Description:
	Allocate a hash table and fill in its header.
Returned Status:
	HASH__SUCCESS			header filled in
	HASH__NO_MEMORY			header unchanged
*/


int HashCreate
(
	HASHTABP pht,						/* hash table structure */
	int nLimit,							/* number of entries needed */
	int nRecLen							/* record length */
)
{
	void **pavTable;
	int nSize;
	ZONEP pz;

	ENTER("HashCreate",FALSE);
	nSize=PrimeGen((int)(nLimit*1.2));
	pz=pzCurrent;
	SetZone(&zPermanent);
	pavTable=(void **)MemAlloc(nSize*nRecLen);
	SetZone(pz);
	if(!pavTable)
	{
		EXIT("HashCreate");
		return(HASH__NO_MEMORY);
	}

	pht->pavTable=pavTable;
	pht->nSize=nSize;
	pht->nRecLen=nRecLen;
	pht->nLimit=nLimit;
	pht->nCount=0;
	
	EXIT("HashCreate");
	return(HASH__SUCCESS);
}

/* HashExpand

Description:
	Double the size of a hash table.
*/

static int HashExpand
(
	HASHTABP phtOld						/* hash table structure */
)
{
	HASHTAB htNew;						// new hash table
	int i;								// loop index
	ZONEP pz;							// zone pointer
	char *pcCursor;						// old record pointer
	void *pvRec;						// new record pointer
	
	ENTER("HashExpand",TRUE);

	// allocate a new hash table, twice the size

	htNew.nCount=0;
	htNew.nLimit=phtOld->nLimit*2;
	htNew.nSize=PrimeGen((int)(htNew.nLimit*1.2));
	htNew.nRecLen=phtOld->nRecLen;
	pz=pzCurrent;
	SetZone(&zPermanent);
	htNew.pavTable=(void **)MemAlloc(htNew.nSize*htNew.nRecLen);
	SetZone(pz);
	if(!htNew.pavTable)
	{
		EXIT("HashExpand");
		return(HASH__NO_MEMORY);
	}
//	fprintf(stderr,"Expanding hash table to %d cells\n",htNew.nLimit);

	// copy the old hash table into the new, rehashing each entry
	
	pcCursor=(char *)phtOld->pavTable;
	for(i=0;i<phtOld->nSize;i++)
	{
		if(*(char **)pcCursor!=NULL&&*(char **)pcCursor!=(char *)-1)
		{
			HashInsert(&htNew,*(char **)pcCursor,&pvRec);
			memcpy(pvRec,pcCursor,htNew.nRecLen);
		}
		pcCursor+=phtOld->nRecLen;
	}
	
	// update hash table parameters
	
	*phtOld=htNew;
	
	EXIT("HashExpand");
	return(HASH__SUCCESS);
}

/* HashClear

Description:
	Delete all records from a hash table.
Returned Status:
	HASH__SUCCESS			hashptr points to table, hashlen contains table length
*/

void HashClear
(
	HASHTABP pht						/* hash table structure */
)
{
	ENTER("HashClear",FALSE);
	if(pht->pavTable)
		memset(pht->pavTable,0,pht->nSize*pht->nRecLen);
	pht->nCount=0;
	EXIT("HashClear");
}

/* HashInsert

Description
	Insert a record into a hash table.
Outputs:
	A pointer to the (available) hash record.
Returns Status:
	HASH__SUCCESS			recptr points to empty slot
	HASH__TABLE_FULL		recptr not modified
	HASH__DUPLICATE_KEY		recptr points to duplicate slot
*/

int HashInsert
(
	HASHTABP pht,						/* hash table structure */
	char *psKey,						/* pointer to key string */
	void **ppvRec						/* pointer to returned record pointer */
)
{
	char *pcStart;						/* initial hash pointer */
	char *pcCursor;						/* running hash pointer */
	int nSize;							/* hash table size */
	char *pcLimit;						/* pointer to end of hash table */
	int nCycle;							/* rehash value */
	char *pcFirst;						/* pointer to first available slot */
	int nStatus;						/* status */

	ENTER("HashInsert",TRUE);

	// Expand the hash table if necessary
	
	if(pht->nCount>=pht->nLimit)
	{
		nStatus=HashExpand(pht);
		if(nStatus!=HASH__SUCCESS)
		{
			EXIT("HashInsert");
			return nStatus;
		}
	}

	// Allocate a record
	
	pcFirst=NULL;
	pcCursor=pcStart=(char *)pht->pavTable+Hash(pht->nSize,psKey)*pht->nRecLen;
	if(*(char **)pcCursor==NULL)		/* if empty entry */
	{
		*ppvRec=(void *)pcCursor;
		pht->nCount++;
		EXIT("HashInsert");
		return(HASH__SUCCESS);
	}
	if(*(char **)pcCursor==(char *)-1)	/* if deleted entry */
		pcFirst=pcCursor;
	else
	{
		if(strcmp(psKey,*(char **)pcCursor)==0)	/* if duplicate key */
		{
			*ppvRec=(void *)pcCursor;
			EXIT("HashInsert");
			return(HASH__DUPLICATE_KEY);	/* duplicate key */
		}
	}
	nSize=pht->nSize*pht->nRecLen;
	pcLimit=(char *)pht->pavTable+nSize;
	nCycle=ReHash(pht->nSize,psKey)*pht->nRecLen;
	pcCursor+=nCycle;
	if(pcCursor>=pcLimit)
		pcCursor-=nSize;
	while(pcCursor!=pcStart)
	{
		if(*(char **)pcCursor==NULL)	/* if empty entry */
		{
			if(pcFirst)
				*ppvRec=(void *)pcFirst;	/* return pointer to first deleted entry */
			else
				*ppvRec=(void *)pcCursor;	/* return pointer to current record */
			pht->nCount++;
			EXIT("HashInsert");
			return(HASH__SUCCESS);
		}
		if(*(char **)pcCursor==(char *)-1)
		{
			if(!pcFirst)
				pcFirst=pcCursor;		/* point to first deleted entry */
		}
		else
		{
			if(strcmp(psKey,*(char **)pcCursor)==0)	/* if duplicate key */
			{
				*ppvRec=(void *)pcCursor;
				EXIT("HashInsert");
				return(HASH__DUPLICATE_KEY);
			}
		}
		pcCursor+=nCycle;				/* point to next possible record */
		if(pcCursor>=pcLimit)
			pcCursor-=nSize;
	};

	/* the hash table is full...  this shouldn't happen */

	EXIT("HashInsert");
	return HASH__TABLE_FULL;
}

/* HashSearch

Description
	Search for a record in a hash table.
Outputs:
	A pointer to the hash record.
Returns Status:
	HASH__SUCCESS			recptr points to slot
	HASH__NOT_FOUND			recptr not modified
*/

int HashSearch
(
	HASHTABP pht,						/* hash table structure */
	char *psKey,						/* pointer to key string */
	void **ppvRec						/* pointer to returned record pointer */
)
{
	char *pcStart;						/* initial hash pointer */
	char *pcCursor;						/* running hash pointer */
	int nSize;							/* hash table size */
	char *pcLimit;						/* pointer to end of hash table */
	int nCycle;							/* rehash value */

	ENTER("HashSearch",TRUE);
	pcCursor=pcStart=(char *)pht->pavTable+Hash(pht->nSize,psKey)*pht->nRecLen;
	if(*(char **)pcCursor==NULL)		/* if empty entry */
	{
		EXIT("HashSearch");
		return(HASH__NOT_FOUND);
	}
	if(*(char **)pcCursor!=(char *)-1)	/* if not deleted entry */
	{
		if(strcmp(psKey,*(char **)pcCursor)==0)	/* if key matches */
		{
			*ppvRec=(void *)pcCursor;
			EXIT("HashSearch");
			return(HASH__SUCCESS);
		}
	}
	nSize=pht->nSize*pht->nRecLen;
	pcLimit=(char *)pht->pavTable+nSize;
	nCycle=ReHash(pht->nSize,psKey)*pht->nRecLen;
	pcCursor+=nCycle;
	if(pcCursor>=pcLimit)
		pcCursor-=nSize;
	while(pcCursor!=pcStart)
	{
		if(*(char **)pcCursor==NULL)	/* if empty entry */
		{
			EXIT("HashSearch");
			return(HASH__NOT_FOUND);
		}
		if(*(char **)pcCursor!=(char *)-1)
		{
			if(strcmp(psKey,*(char **)pcCursor)==0)	/* if key matches */
			{
				*ppvRec=(void *)pcCursor;
				EXIT("HashSearch");
				return(HASH__SUCCESS);
			}
		}
		pcCursor+=nCycle;				/* point to next possible record */
		if(pcCursor>=pcLimit)
			pcCursor-=nSize;
	};
	EXIT("HashSearch");
	return(HASH__NOT_FOUND);
}

/* HashDelete

Description
	Delete a record from a hash table.
Outputs:
	Nothing.
Returns Status:
	HASH__SUCCESS
	HASH__NOT_FOUND
*/

int HashDelete
(
	HASHTABP pht,						/* hash table structure */
	char *psKey							/* pointer to key string */
)
{
	char *pcStart;						/* initial hash pointer */
	char *pcCursor;						/* running hash pointer */
	int nSize;							/* hash table size */
	char *pcLimit;						/* pointer to end of hash table */
	int nCycle;							/* rehash value */

	ENTER("HashDelete",TRUE);
	pcCursor=pcStart=(char *)pht->pavTable+Hash(pht->nSize,psKey)*pht->nRecLen;
	if(*(char **)pcCursor==NULL)		/* if empty entry */
	{
		EXIT("HashDelete");
		return(HASH__NOT_FOUND);
	}
	if(*(char **)pcCursor!=(char *)-1)	/* if not deleted entry */
	{
		if(strcmp(psKey,*(char **)pcCursor)==0)	/* if key matches */
		{
			*(char **)pcCursor=(char *)-1;	/* mark slot deleted */
			pht->nCount--;
			EXIT("HashDelete");
			return(HASH__SUCCESS);
		}
	}
	nSize=pht->nSize*pht->nRecLen;
	pcLimit=(char *)pht->pavTable+nSize;
	nCycle=ReHash(pht->nSize,psKey)*pht->nRecLen;
	pcCursor+=nCycle;
	if(pcCursor>=pcLimit)
		pcCursor-=nSize;
	while(pcCursor!=pcStart)
	{
		if(*(char **)pcCursor==NULL)	/* if empty entry */
		{
			EXIT("HashDelete");
			return(HASH__NOT_FOUND);
		}
		if(*(char **)pcCursor!=(char *)-1)
		{
			if(strcmp(psKey,*(char **)pcCursor)==0)	/* if key matches */
			{
				*(char **)pcCursor=(char *)-1;	/* mark slot deleted */
				pht->nCount--;
				EXIT("HashDelete");
				return(HASH__SUCCESS);
			}
		}
		pcCursor+=nCycle;				/* point to next possible record */
		if(pcCursor>=pcLimit)
			pcCursor-=nSize;
	};
	EXIT("HashDelete");
	return(HASH__NOT_FOUND);
}

/* HashCount

Description:
	Count up the number of records in use in a hash table.
*/

int HashCount
(
	HASHTABP pht						/* hash table structure */
)
{
	int i;								/* loop index */
	int n;								/* record count */
	char *pcCursor;						/* record pointer */

	ENTER("HashCount",TRUE);
	n=0;
	pcCursor=(char *)pht->pavTable;
	for(i=0;i<pht->nSize;i++)
	{
		if(*(char **)pcCursor!=NULL&&*(char **)pcCursor!=(char *)-1)
			n++;
		pcCursor+=pht->nRecLen;
	}
	if(n!=pht->nCount)
		fprintf(stderr,"Bad hash count\n");
	EXIT("HashCount");
	return n;
}

/* HashScan

Description:
	Scan the hash table, executing the passed function for each used record.
*/

void HashScan
(
	HASHTABP pht,						/* hash table structure */
	void (*pfFunc)(void *)				/* function to execute */
)
{
	int i;								/* loop index */
	char *pcCursor;						/* record pointer */

	ENTER("HashScan",TRUE);
	pcCursor=(char *)pht->pavTable;
	for(i=0;i<pht->nSize;i++)
	{
		if(*(char **)pcCursor!=NULL&&*(char **)pcCursor!=(char *)-1)
			(*pfFunc)((void *)pcCursor);
		pcCursor+=pht->nRecLen;
	}
	EXIT("HashScan");
}

/* Hash

Description:
	Primary hash function.
Algorithm:
	Fibonacci style random number generator.
	Here we select the fibonacci element using bits 12-15 of
	the previous value.
	We return a number between 0 and count-1.
Note:
	The number of records in the hash table should be prime.
*/

static int Hash
(
	int nCount,							/* number of records in hash table */
	char *psKey							/* key to hash */
)
{
	unsigned int nValue;				/* hash value */
	unsigned char *ps;					/* string pointer */

	ENTER("Hash",TRUE);
	nValue=0;
	for(ps=(unsigned char *)psKey;*ps;ps++)
		nValue+=anFibonacci[(nValue>>12)&0x0000000F]**ps;
	EXIT("Hash");
	return(nValue%nCount);
}

/* ReHash

Description:
	Secondary hash function.
Algorithm:
	Fibonacci style random number generator.
	Here we select the fibonacci element using bits 3, 7, 11, 13
	of the previous value using the mask 0x8888 and
	the magic multiplier 0x2490 (obtained mostly by trial and error).
	We return a number between 1 and count-1;
	(We must guarantee that a value of 0 is not generated, so
	that we don't generate a nil-potent collision ring... that
	is a collision ring of length 0.)
Note:
	The number of records in the hash table should be prime.  If it is, then
	every non-nil-potent collision ring is the size of the entire hash table.
*/

static int ReHash
(
	int nCount,							/* number of records in hash table */
	char *psKey							/* key to hash */
)
{
	unsigned int nValue;				/* hash value */
	unsigned char *ps;					/* string pointer */

	ENTER("ReHash",TRUE);
	nValue=8;
	for(ps=(unsigned char *)psKey;*ps;ps++)
		nValue+=anFibonacci[(((nValue&0x00008888)*0x00002490)>>16)&0x0000000F]**ps;
	EXIT("ReHash");
	return(nValue%(nCount-1)+1);
}

/* PrimeGen

Description:
	Find the smallest prime number greater than or equal
	to the passed number.
Note:
	We don't worry about values less than 3.
*/

static int PrimeGen
(
	int nCount							/* initial value */
)
{
	int nValue;							/* trial value */
	int nMax;							/* maximum dividend */
	int nDivisor;						/* trial divisor */

	ENTER("PrimeGen",TRUE);
	for(nValue=(nCount&~1)+1;;nValue+=2)	/* try all odd values greater than count */
	{
		nMax=(int)sqrt(nValue);
		for(nDivisor=3;nDivisor<=nMax;nDivisor+=2)
			if((nValue%nDivisor)==0)	/* if value not prime */
				break;
		if(nDivisor>nMax)				/* if we've tried all divisors */
		{
			EXIT("PrimeGen");
			return(nValue);				/* must be prime */
		}
	}
}

// Resizeable Table Support ----------------------------------------------------

typedef struct ResizeTab				// resizeable table header
{
	void **pavTable;					/* table address */
	int nSize;							/* actual size of table, records */
	int nRecLen;						/* record length */
	int nCount;							/* current number of records */
}RESIZETAB, *RESIZETABP;

/* ResizeCreate

Description:
	Allocate a resizeable table and fill in its header.
Returned Status:
	HASH__SUCCESS			header is filled in
	HASH__NO_MEMORY			header unchanged
*/

//int ResizeCreate
//(
//	RESIZETABP prt,						/* resize table structure */
//	int nSize,							/* number of entries needed */
//	int nRecLen							/* record length */
//)
//{
//	void **pavTable;
//	ZONEP pz;
//
//	ENTER("ResizeCreate",FALSE);
//	pz=pzCurrent;
//	SetZone(&zPermanent);
//	pavTable=(void **)MemAlloc(nSize*nRecLen);
//	SetZone(pz);
//	if(!pavTable)
//	{
//		EXIT("ResizeCreate");
//		return(HASH__NO_MEMORY);
//	}
//
//	prt->pavTable=pavTable;
//	prt->nSize=nSize;
//	prt->nRecLen=nRecLen;
//	prt->nCount=0;
//	
//	EXIT("ResizeCreate");
//	return(HASH__SUCCESS);
//}

/* ResizeExpand

Description:
	Double the size of a resizeable table.
*/

static int ResizeExpand
(
	RESIZETABP prt						/* resizeable table header */
)
{
	void **pavTable;
	ZONEP pz;							// zone pointer
	
	ENTER("ResizeExpand",TRUE);

	// allocate a new resizeable table, twice the size

	pz=pzCurrent;
	SetZone(&zPermanent);
	pavTable=(void **)MemAlloc(2*prt->nSize*prt->nRecLen);
	SetZone(pz);
	if(!pavTable)
	{
		EXIT("ResizeExpand");
		return(HASH__NO_MEMORY);
	}

	// copy the old resizeable table into the new
	
	memcpy(pavTable,prt->pavTable,prt->nSize*prt->nRecLen);
	
	// update resizeable table parameters
	
	prt->pavTable=pavTable;
	prt->nSize*=2;
//	fprintf(stderr,"Expanding resizeable table to %d cells\n",prt->nSize);
	
	EXIT("ResizeExpand");
	return(HASH__SUCCESS);
}

/* ResizeClear

Description:
	Delete all records from a resizeable table.
*/

//void ResizeClear
//(
//	RESIZETABP prt						/* resizeable table structure */
//)
//{
//	ENTER("ResizeClear",FALSE);
//	if(prt->pavTable)
//		memset(prt->pavTable,0,prt->nSize*prt->nRecLen);
//	prt->nCount=0;
//	EXIT("ResizeClear");
//}

/* ResizeInsert

Description
	Insert a record into a resizeable table.
Outputs:
	A pointer to the (available) resizeable record.
Returns Status:
	HASH__SUCCESS				ppvRec points to empty record
	HASH__NO_MEMORY			ppvRec not modified
*/

//int ResizeInsert
//(
//	RESIZETABP prt,						/* resizeable table structure */
//	char *psKey,						/* pointer to key string */
//	void **ppvRec						/* pointer to returned record pointer */
//)
//{
//	int nStatus;
//
//	ENTER("ResizeInsert",TRUE);
//
//	// Expand the resizeable table if necessary
//	
//	if(prt->nCount>=prt->nSize)
//	{
//		nStatus=ResizeExpand(prt);
//		if(nStatus!=HASH__SUCCESS)
//		{
//			EXIT("ResizeInsert");
//			return nStatus;
//		}
//	}
//
//	// Allocate next record
//	
//	*ppvRec=(void *)((char *)prt->pavTable+prt->nCount*prt->nRecLen);
//	prt->nCount++;
//
//	EXIT("ResizeInsert");
//	return HASH__SUCCESS;
//}

