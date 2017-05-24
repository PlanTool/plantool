/* zone.h -- zoned memory allocation support */

/* structures and definitions */

#define ZoneUnmark(pv) (((MEMORYP)((char *)pv-offsetof(MEMORY,pmNext)))->nTag=RESERVED)
#define ZoneMark(pv) (((MEMORYP)((char *)pv-offsetof(MEMORY,pmNext)))->nTag=MARKED)
#define ZoneMarkedQ(pv) (((MEMORYP)((char *)pv-offsetof(MEMORY,pmNext)))->nTag==MARKED)
#define ZoneSizeOf(pv) (((MEMORYP)((char *)pv-offsetof(MEMORY,pmNext)))->nLength)

#define FREE		0x12345678			/* free memory tag */
#define RESERVED_P	0x8765432A			/* reserved permanent memory tag */
#define RESERVED_S	0x8765432B			/* reserved scratch memory tag */
#define MARKED		0x12344321			/* marked memory tag (for garbage collection) */
#define RELOCATED	0xABCDABCD			/* relocated memory tag */

typedef struct alloc					/* master allocation block */
{
	struct alloc *paNext;				/* pointer to next */
	struct alloc *paPrev;				/* pointer to previous */
	struct memory *pmAddress;			/* address of block */
	int nLength;						/* length of block */
}ALLOC, *ALLOCP;

typedef struct zone						/* the data used to maintain a zone */
{
	int nTag;							/* dummy reserved tag */
	int nLength;						/* dummy length of block */
	struct memory *pmNext;				/* free list */
	struct memory *pmPrev;
//	struct memory *pmONext;				/* orphan list */
//	struct memory *pmOPrev;
	struct memory *pmRover;				/* roving pointer to free list */
	struct alloc *paMaster;				/* master allocation list */
	int nAllocations;					/* number of allocations */
	int nFrees;							/* number of frees */
	int nTotal;							/* total amount of memory allocated */
}ZONE, *ZONEP;

typedef struct memory					/* the header for each memory block */
{
	int nTag;							/* free/reserved tag */
	int nLength;						/* length of block */
	struct memory *pmNext;				/* pointer to next block */
	struct memory *pmPrev;				/* pointer to previous block */
}MEMORY, *MEMORYP;

/* global data */

extern ZONE zScratch;					/* scratch data zone */
extern ZONE zPermanent;					/* permanent data zone */
extern ZONEP pzCurrent;					/* current zone */
extern int nZoneLimit;					/* zone memory limit */

/* global function prototypes */

void *ZoneAlloc
(
	int nLength							/* size of block to allocate */
);
//void *ZoneArrayAlloc
//(
//	int nCount,							/* number of elements */
//	int nSize,							/* desired size of each element */
//	int *pnActual,						/* returned actual size */
//	BOOL bClear							/* clear memory flag */
//);
DECLSPEC void ZoneFree
(
	void *pvBlock						/* pointer to memory to free */
);
void ZoneClear
(
	ZONEP pzZone						/* zone to clear */
);
void ZoneStatistics
(
	FILE *pfOutput,						/* output file stream */
	ZONEP pzZone						/* zone to report */
);
void SetZone
(
	ZONEP pzZone						/* zone to select */
);
BOOL InitZone
(
	ZONEP pzZone						/* zone to initialize */
);
void ZoneCollect
(
	ZONEP pzZone						/* zone to scan */
);
void ZoneDelta
(
	int *pnRunningTotal,
	char *pString
);
void *ZoneCopy
(
	void *pv1							// pointer to old block
);
void ZoneReloc
(
	void **ppv							/* pointer to relocate */ 
);

