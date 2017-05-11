#ifndef __UTIL_H
#define __UTIL_H

/* util.h  -- utility functions */

/* macro and structure definitions */

//#define StringEqQ(ps1,ps2) (!stricmp((ps1),(ps2)))
#define StringEqQ(ps1,ps2) ((ps1)==(ps2))
#define StringLtQ(x,y) (stricmp((x),(y))<0)
#define Free(x) ZoneFree(x)

/* global data */

extern FILE *pfError;					/* error message log file stream */
extern FILE *pfJBCRC;					/* crc32 file */
extern int nErrorCount;					/* number of error messages emitted */

/* global function prototypes */

DECLSPEC void *MemAlloc
(
	size_t nSize
);
void *CopyAlloc
(
	void *pBuffer,
	size_t nSize
);
DECLSPEC void *StrAlloc
(
	char *psString
);
DECLSPEC void *StrAllocAndPos
(
 char *psString,
 int *position
);
DECLSPEC void *IdentAllocAndPos
(
 char *psString,
 int *position
);
DECLSPEC void *IdentAlloc
(
	char *psString
);
void Message
(
	char *psFormat,
	...
);
DECLSPEC void ErrorMessage
(
	char *psFormat,
	...
);
void ErrorExit
(
	char *psFormat,
	...
);
void DumpBindings
(
	BINDINGP pbBindings
);
double GetInternalRunTime(void);
unsigned int SizeOfMemory(void);
unsigned int CRC32
(
	unsigned char *pcBuffer,			/* pointer to buffer */
	unsigned int nSize,					/* buffer size */
	unsigned int nCRC					/* seed or previous crc */
);
DECLSPEC int FibRand(void);
DECLSPEC void FibSeed
(
	int nSeed							/* seed value */
);
int MSRand(void);
void MSSeed
(
	int nSeed							/* seed value */
);

BOOL FormatText
(
	char *psBuffer,						/* output buffer */
	char *psFormat,						/* format string */
	CELLP pcArgs,						/* argument list */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

#ifndef WIN32
int stricmp
(
	const char *s1,
	const char *s2
);
char *strlwr
(
	char *psString
);
#else /* WIN32 */
#define stricmp _stricmp
#endif /* WIN32 */

#endif /* __UTIL_H */
